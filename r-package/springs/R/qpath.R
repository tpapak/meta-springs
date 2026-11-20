# springs — path-based inconsistency: Q_path and the Netpath plot
# (Tahmasebi, Davies, Papakonstantinou, Rücker & Nikolakopoulou,
#  Statistics in Medicine 2026, doi:10.1002/sim.70752).
#
# Spring reading. Every direct comparison (edge) is a spring: rest length = its
# pooled direct estimate y_e, stiffness w_e = 1/v_e. The network estimate of
# comparison i-j is the equilibrium distance between nodes i and j; it is linear
# in the rest lengths, θ_ij = Σ_e h_e y_e, where h (the hat-matrix row) is the
# force flow through each spring when i and j are pulled apart. The sign of h_e
# orients each spring, and every directed i -> j route along the flow is a
# *path* of evidence: its estimate is the sum of the rest lengths along it, its
# variance the sum of the compliances 1/w_e, and two paths covary through the
# springs they share. Q_path asks whether the paths agree with the equilibrium:
#   Q = (θ_π - θ_ij 1)ᵀ Σ⁻¹ (θ_π - θ_ij 1)  ~  χ²(P' - 1)   (P' independent paths).

# ---- data: study contrasts -> per-study spring weights -> pooled edges ------------

# Within one study (m arms), the contrast variances V_ab (m x m, from seTE²) are
# effective resistances of an electrical/spring network; its Laplacian gives the
# equivalent two-arm weights (Rücker 2012): L⁺ = -½ C V C, w_ab = -L_ab.
.study_springs <- function(arms, V) {
  m <- length(arms)
  if (m == 2) return(data.frame(a = arms[1], b = arms[2], w = 1 / V[1, 2]))
  C <- diag(m) - matrix(1 / m, m, m)
  Lp <- -0.5 * C %*% V %*% C
  e <- eigen(Lp, symmetric = TRUE)
  keep <- e$values > 1e-10 * max(e$values)
  L <- e$vectors[, keep, drop = FALSE] %*% diag(1 / e$values[keep], sum(keep)) %*%
       t(e$vectors[, keep, drop = FALSE])
  pr <- utils::combn(m, 2)
  data.frame(a = arms[pr[1, ]], b = arms[pr[2, ]], w = -L[t(pr)])
}

#' Pool study contrasts into direct-comparison springs (common-effect)
#'
#' @param data Contrast-level data: one row per pairwise comparison within a
#'   study, with columns `TE` (treat1 vs treat2), `seTE`, `treat1`, `treat2`,
#'   `studlab` (netmeta `pairwise()` layout). Multi-arm studies list all their
#'   pairwise contrasts; their weights are reduced by Rücker's method.
#' @return Data frame of edges: treat1, treat2, TE (treat1 vs treat2), var.
#' @keywords internal
spring_edges <- function(data) {
  d <- data.frame(TE = data$TE, seTE = data$seTE, t1 = as.character(data$treat1),
                  t2 = as.character(data$treat2), s = as.character(data$studlab),
                  stringsAsFactors = FALSE)
  out <- lapply(split(d, d$s), function(x) {
    arms <- sort(unique(c(x$t1, x$t2)))
    V <- matrix(0, length(arms), length(arms), dimnames = list(arms, arms))
    V[cbind(x$t1, x$t2)] <- x$seTE^2; V[cbind(x$t2, x$t1)] <- x$seTE^2
    sp <- .study_springs(arms, V)
    # contrast for each spring (a vs b), from the reported TE (consistent within a study)
    te <- mapply(function(a, b) {
      i <- which(x$t1 == a & x$t2 == b); if (length(i)) return(x$TE[i[1]])
      i <- which(x$t1 == b & x$t2 == a); if (length(i)) return(-x$TE[i[1]])
      NA_real_
    }, sp$a, sp$b)
    sp$TE <- te; sp$study <- x$s[1]; sp
  })
  st <- do.call(rbind, out)
  st <- st[st$w > 1e-12, ]
  key <- paste(st$a, st$b, sep = "\r")
  e <- do.call(rbind, lapply(split(st, key), function(x)
    data.frame(treat1 = x$a[1], treat2 = x$b[1], TE = sum(x$w * x$TE) / sum(x$w), var = 1 / sum(x$w))))
  rownames(e) <- NULL
  e
}

# ---- Q_path -----------------------------------------------------------------------

#' Path-based inconsistency for one comparison: Q_path and Netpath z-scores
#'
#' Implements the path-based approach of Tahmasebi et al. (2026): all directed
#' paths of evidence from `treat1` to `treat2` along the network's force flow
#' (the hat-matrix row), their estimates and covariance, the global
#' inconsistency statistic Q_path ~ χ²(P' − 1) over the P' linearly independent
#' paths, and the pairwise path-disagreement z-scores of the Netpath plot.
#'
#' @param data Contrast-level data (see [spring_edges()]) or a data frame of
#'   pooled edges with columns treat1, treat2, TE, var.
#' @param treat1,treat2 The comparison (estimate = treat1 vs treat2).
#' @return An object of class `springs_qpath`: `paths` (path, estimate,
#'   variance, independent), `theta_nma`, `Q`, `df`, `pval`, `z` (P' x P'
#'   |z| matrix), `row_score`, `flow` (hat-matrix row over edges).
#' @export
qpath <- function(data, treat1, treat2) {
  edges <- if (all(c("treat1", "treat2", "TE", "var") %in% names(data)) && !("seTE" %in% names(data)))
    data else spring_edges(data)
  nodes <- sort(unique(c(edges$treat1, edges$treat2)))
  if (!all(c(treat1, treat2) %in% nodes)) stop("springs::qpath: unknown treatment.", call. = FALSE)
  E <- nrow(edges); N <- length(nodes)
  B <- matrix(0, E, N, dimnames = list(NULL, nodes))
  B[cbind(seq_len(E), match(edges$treat1, nodes))] <- 1      # TE = μ_t1 − μ_t2
  B[cbind(seq_len(E), match(edges$treat2, nodes))] <- -1
  W <- 1 / edges$var
  L <- crossprod(B, B * W)                                    # spring-network Laplacian
  ev <- eigen(L, symmetric = TRUE); k <- ev$values > 1e-10 * max(ev$values)
  Lp <- ev$vectors[, k, drop = FALSE] %*% diag(1 / ev$values[k], sum(k)) %*% t(ev$vectors[, k, drop = FALSE])
  b <- setNames(numeric(N), nodes); b[treat1] <- 1; b[treat2] <- -1
  h <- as.vector(t(b) %*% Lp %*% t(B * W))                    # force flow through each spring
  theta_nma <- sum(h * edges$TE)

  # directed graph: spring e traversed t1 -> t2 if h_e > 0, t2 -> t1 if h_e < 0
  tol <- 1e-10 * max(abs(h))
  arcs <- do.call(rbind, lapply(which(abs(h) > tol), function(e)
    if (h[e] > 0) data.frame(from = edges$treat1[e], to = edges$treat2[e], e = e, s = 1)
    else data.frame(from = edges$treat2[e], to = edges$treat1[e], e = e, s = -1)))
  # all simple directed paths treat1 -> treat2 (depth-first)
  paths <- list()
  dfs <- function(node, visited, used) {
    if (node == treat2) { paths[[length(paths) + 1]] <<- used; return(invisible()) }
    for (r in which(arcs$from == node)) {
      nx <- arcs$to[r]
      if (!(nx %in% visited)) dfs(nx, c(visited, nx), rbind(used, arcs[r, ]))
    }
  }
  dfs(treat1, treat1, arcs[0, ])
  P <- length(paths)
  S <- matrix(0, P, E)                                         # signed path-edge incidence
  for (p in seq_len(P)) S[p, paths[[p]]$e] <- paths[[p]]$s
  est <- as.vector(S %*% edges$TE)
  Sigma <- S %*% (t(S) * edges$var)                            # shared springs -> covariance
  label <- vapply(paths, function(u) paste(c(u$from, u$to[nrow(u)]), collapse = " -> "), "")

  # drop linearly dependent paths (Q is invariant to which ones)
  indep <- logical(P); kept <- integer()
  for (p in seq_len(P)) {
    cand <- c(kept, p)
    if (qr(S[cand, , drop = FALSE])$rank == length(cand)) { kept <- cand; indep[p] <- TRUE }
  }
  r <- est[kept] - theta_nma
  Sk <- Sigma[kept, kept, drop = FALSE]
  Q <- as.numeric(t(r) %*% solve(Sk, r))
  df <- length(kept) - 1L
  # Netpath: |z| between paths, z = (θ_p − θ_p') / sqrt(var_p + var_p' − 2 cov)
  vd <- outer(diag(Sk), diag(Sk), "+") - 2 * Sk
  z <- abs(outer(est[kept], est[kept], "-")) / sqrt(pmax(vd, 1e-300))
  diag(z) <- 0
  dimnames(z) <- list(label[kept], label[kept])
  structure(list(comparison = c(treat1, treat2), theta_nma = theta_nma,
                 paths = data.frame(path = label, estimate = est, variance = diag(Sigma),
                                    independent = indep, stringsAsFactors = FALSE),
                 Sigma = Sigma, Q = Q, df = df, pval = stats::pchisq(Q, df, lower.tail = FALSE),
                 z = z, row_score = rowSums(z),
                 flow = data.frame(edges[, c("treat1", "treat2", "TE", "var")], flow = h)),
            class = "springs_qpath")
}

#' @export
print.springs_qpath <- function(x, digits = 3, ...) {
  cat(sprintf("Path-based inconsistency, %s vs %s: network estimate %.*f\n",
              x$comparison[1], x$comparison[2], digits, x$theta_nma))
  cat(sprintf("%d paths (%d linearly independent); Q_path = %.*f on %d df, p = %.4f\n\n",
              nrow(x$paths), sum(x$paths$independent), digits, x$Q, x$df, x$pval))
  p <- x$paths; p$row_score <- NA_real_; p$row_score[p$independent] <- x$row_score
  print(format(p, digits = digits), row.names = FALSE)
  invisible(x)
}

#' Netpath plot: pairwise disagreement between paths of evidence
#'
#' Heat map of |z| between every pair of independent paths (darker = more
#' conflict), with the row scores (total disagreement of each path).
#' @param x A `springs_qpath` object.
#' @param ... Passed to [graphics::image()].
#' @export
netpath_plot <- function(x, ...) {
  z <- x$z; P <- nrow(z)
  op <- graphics::par(mar = c(10, 12, 3, 6)); on.exit(graphics::par(op))
  cols <- grDevices::colorRampPalette(c("white", "#fcbba1", "#cb181d", "#67000d"))(64)
  graphics::image(seq_len(P), seq_len(P), t(z[P:1, , drop = FALSE]), col = cols, axes = FALSE,
                  xlab = "", ylab = "", zlim = c(0, max(z, 1)), ...)
  graphics::axis(1, seq_len(P), colnames(z), las = 2, cex.axis = 0.7)
  graphics::axis(2, seq_len(P), rev(rownames(z)), las = 1, cex.axis = 0.7)
  # cell (x = column j, y = k) shows z[P + 1 - k, j], matching image()
  graphics::text(rep(seq_len(P), times = P), rep(seq_len(P), each = P),
                 sprintf("%.2f", as.vector(t(z[P:1, , drop = FALSE]))), cex = 0.6)
  graphics::mtext(sprintf("%s vs %s: Q_path = %.2f, df = %d, p = %.4f", x$comparison[1],
                          x$comparison[2], x$Q, x$df, x$pval), side = 3, line = 1)
  graphics::axis(4, seq_len(P), sprintf("%.2f", rev(x$row_score)), las = 1, cex.axis = 0.7, tick = FALSE)
  graphics::mtext("row score", side = 4, line = 4)
  invisible(x)
}
