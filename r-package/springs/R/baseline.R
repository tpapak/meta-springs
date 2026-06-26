# springs — bnma-compatible facade for baseline-risk meta-regression.
#
# MetaInsight fits baseline-risk regression with bnma (Achana 2013):
#   logit(p_ik) = η_i + d_t + B·(η_i − mx)    (binary, OR)
#   y_ik        = η_i + d_t + B·(η_i − mx)    (continuous, MD)
# This facade reproduces the slice of the bnma `network.run` object MetaInsight
# consumes, backed by the verified spring baseline-risk engine (`brisk`), so the
# baseline path works by swapping bnma calls for springs ones.
#
# The returned object (class "springs_brisk_result") mimics bnma:
#   $network$Treat.order / $type / $response / $mx_bl
#   $samples : 4-chain coda mcmc.list, columns d[1..K], b_bl[1..K]
#   $rank.tx : ranking-probability table (rows = "treatment <name>")
#   summary(): $summary.samples$quantiles (d[k], b_bl[k] rows), $deviance, $total_n

# Resolve the brisk engine binary (same scheme as the mtc engine).
brisk_engine_bin <- function() {
  cand <- c(getOption("springs.brisk", default = ""),
            Sys.getenv("SPRINGS_BRISK_BIN", unset = ""),
            file.path(find.package("springs", quiet = TRUE), "..", "..", "gc-rust",
                      "target", "release", "brisk"),
            "/Users/tosku/Sync/Documents/meta-springs/gc-rust/target/release/brisk")
  cand <- cand[nzchar(cand)]
  for (p in cand) {
    pp <- suppressWarnings(normalizePath(p, mustWork = FALSE))
    if (file.exists(pp)) return(pp)
  }
  stop("springs: could not find the 'brisk' engine binary. Build it with\n",
       "  cargo build --release --bin brisk\n",
       "and set options(springs.brisk=) or $SPRINGS_BRISK_BIN.", call. = FALSE)
}

#' Fit a baseline-risk regression (bnma `network.run` analogue)
#'
#' @param ArmLevel Data frame with columns `Study`, `Treat`, `Outcomes`, `N`
#'   (binary) and additionally `SD` (continuous).
#' @param Treat.order Treatment order, reference first.
#' @param response `"binomial"` or `"normal"`.
#' @param type `"fixed"` or `"random"` (random adds τ²/2-per-arm heterogeneity).
#' @param regressor bnma slope structure: `"common"` (shared), `"independent"`
#'   (unrelated), or `"exchangeable"` (b_t ~ N(B, σ_B²)).
#' @param seed RNG seed.
#' @param n.chain Pseudo-chains for the draws (default 4, matching bnma usage).
#' @param method `"gc"` (default) grand-canonical: integrates the variance
#'   components present — τ² under random effects (1-D) and additionally σ_B²
#'   under exchangeable (2-D Ξ(τ²,σ_B²)) — giving calibrated intervals (matches
#'   bnma's posterior mean, lifts σ_B² off the REML boundary). `"em"` maximises
#'   them (point estimate; under-covers when a component is weakly identified).
#'   Fixed-effect shared/unrelated have no component to integrate, so both reduce
#'   to the exact MLE + observed-information Laplace.
#' @return An object of class `springs_brisk_result`.
#' @export
brisk_run <- function(ArmLevel, Treat.order, response = "binomial",
                      type = "fixed", regressor = "common", seed = 123,
                      n.chain = 4, method = "gc") {
  if (!regressor %in% c("common", "independent", "exchangeable")) {
    stop("springs::brisk_run: regressor must be 'common', 'independent', or ",
         "'exchangeable'.", call. = FALSE)
  }
  effects <- if (identical(type, "random")) "random" else "fixed"
  # GC is the default solver: it integrates whichever variance components are present
  # (τ² under RE, σ_B² under exchangeable) instead of fixing them at a point — better
  # calibrated and about as fast. FE shared/unrelated have no component to integrate, so
  # the engine falls through to the exact MLE + observed-information Laplace automatically.
  meth <- if (method == "gc") "gc" else "em"
  ref <- Treat.order[1]
  binary <- response == "binomial"

  recs <- data.frame(study = as.character(ArmLevel$Study),
                     treatment = as.character(ArmLevel$Treat),
                     n = as.numeric(ArmLevel$N), stringsAsFactors = FALSE)
  if (binary) {
    recs$events <- as.numeric(ArmLevel$Outcomes)
  } else {
    recs$mean <- as.numeric(ArmLevel$Outcomes)
    recs$sd   <- as.numeric(ArmLevel$SD)
  }

  inp <- tempfile(fileext = ".json"); out <- tempfile(fileext = ".json")
  on.exit(unlink(c(inp, out)), add = TRUE)
  jsonlite::write_json(recs, inp, dataframe = "rows", auto_unbox = TRUE)

  bin <- brisk_engine_bin()
  st <- suppressWarnings(system2(bin, c("--input", inp, "--output", out,
                                        "--ref", ref, "--samples", "4000",
                                        "--regressor", regressor,
                                        "--effects", effects,
                                        "--method", meth,
                                        "--seed", as.character(as.integer(seed))),
                                 stdout = FALSE, stderr = FALSE))
  if (!identical(st, 0L) || !file.exists(out)) {
    stop("springs::brisk_run: engine failed (status ", st, ").", call. = FALSE)
  }
  rep <- jsonlite::fromJSON(out, simplifyVector = TRUE)

  # The engine sorts treatments lexicographically; reorder its output columns to
  # the caller's Treat.order so d[k]/b_bl[k] indices match bnma (and MetaInsight,
  # which names slopes/intercepts by Treat.order).
  K <- length(Treat.order)
  perm <- match(Treat.order, rep$treatments)
  if (anyNA(perm)) stop("springs::brisk_run: Treat.order/engine treatment mismatch.",
                        call. = FALSE)
  dd <- rep$d_draws; bb <- rep$b_draws   # S x K each, ref col = 0
  if (is.null(dim(dd))) dd <- matrix(dd, ncol = length(rep$treatments))
  if (is.null(dim(bb))) bb <- matrix(bb, ncol = length(rep$treatments))
  dd <- dd[, perm, drop = FALSE]; bb <- bb[, perm, drop = FALSE]
  trts <- Treat.order                    # ref first (as requested)
  rep$d_hat <- rep$d_hat[perm]; rep$b_hat <- rep$b_hat[perm]
  colnames(dd) <- paste0("d[", seq_len(K), "]")
  colnames(bb) <- paste0("b_bl[", seq_len(K), "]")
  mat <- cbind(dd, bb)

  res <- list(
    network = list(Treat.order = trts, type = type, response = response,
                   mx_bl = rep$mx),
    samples = .springs_mcmc_list(mat, n.chain),
    rank.tx = .brisk_rank_tx(dd, trts, response),
    d_hat = rep$d_hat, b_hat = rep$b_hat,
    tau2 = rep$tau2, sigma_b2 = rep$sigma_b2,
    n_data = nrow(ArmLevel),
    n_samples = rep$n_samples
  )
  class(res) <- "springs_brisk_result"
  res
}

# Ranking table in bnma `rank.tx` shape: rows "treatment <name>", cols = ranks,
# entries = P(treatment attains that rank). Larger d = better (rank 1) — the
# direction switch is applied downstream by BnmaSwitchRanking.
.brisk_rank_tx <- function(d_draws, trts, response) {
  K <- length(trts); S <- nrow(d_draws)
  counts <- matrix(0, nrow = K, ncol = K)
  for (s in seq_len(S)) {
    ord <- order(d_draws[s, ], decreasing = TRUE)
    for (r in seq_len(K)) counts[ord[r], r] <- counts[ord[r], r] + 1
  }
  probs <- counts / S
  # bnma `rank.tx` orientation: rows = ranks, columns = "treatment <name>".
  probs <- t(probs)
  rownames(probs) <- as.character(seq_len(K))
  colnames(probs) <- paste0("treatment ", trts)
  probs
}

#' @export
summary.springs_brisk_result <- function(object, ...) {
  sm <- object$samples
  allmat <- do.call(rbind, lapply(sm, function(ch) as.matrix(ch)))
  qs <- t(apply(allmat, 2, function(x)
    stats::quantile(x, c(0.025, 0.25, 0.5, 0.75, 0.975), names = FALSE)))
  colnames(qs) <- c("2.5%", "25%", "50%", "75%", "97.5%")
  st <- cbind(Mean = colMeans(allmat),
              SD = apply(allmat, 2, stats::sd))
  out <- list(
    summary.samples = list(statistics = st, quantiles = qs),
    # gemtc-format deviance placeholder (Dbar, pD, DIC); residual-deviance TODO.
    deviance = stats::setNames(c(NA_real_, NA_real_, NA_real_), c("Dbar", "pD", "DIC")),
    total_n = object$n_data
  )
  class(out) <- "summary.springs_brisk_result"
  out
}

#' All-pairwise relative-effects table (bnma `relative.effects.table` analogue)
#'
#' At the centred baseline (covariate = mx), the relative effects are the d[k]
#' contrasts. Returns a square character matrix of `"[lower,median,upper]"`
#' (bnma's `summary_stat = "ci"` format) so MetaInsight's parser works unchanged.
#'
#' @param model A `springs_brisk_result`.
#' @param summary_stat Only `"ci"` is supported.
#' @param ... Ignored.
#' @export
relative.effects.table <- function(model, summary_stat = "ci", ...) {
  trts <- model$network$Treat.order
  K <- length(trts)
  sm <- model$samples
  dcols <- paste0("d[", seq_len(K), "]")
  D <- do.call(rbind, lapply(sm, function(ch) as.matrix(ch)[, dcols, drop = FALSE]))
  tab <- matrix("", K, K, dimnames = list(trts, trts))
  for (i in seq_len(K)) for (j in seq_len(K)) {
    if (i == j) { tab[i, j] <- trts[i]; next }
    # effect of column j relative to row i = d[j] - d[i]
    diff <- D[, j] - D[, i]
    q <- stats::quantile(diff, c(0.025, 0.5, 0.975), names = FALSE)
    tab[i, j] <- sprintf("[%s,%s,%s]", q[1], q[2], q[3])
  }
  tab
}

#' @export
print.springs_brisk_result <- function(x, ...) {
  cat("springs baseline-risk result (", x$network$response, ", ",
      x$network$type, ")\n", sep = "")
  cat("  ref:", x$network$Treat.order[1], "| mx:", round(x$network$mx_bl, 4),
      "| treatments:", length(x$network$Treat.order), "\n")
  invisible(x)
}
