# Time-profile each stage of CINeMA's runNMA on Fallers and emit
# everything *except* the contribution matrix (which Rust takes over).
#
# Stages timed:
#   1. data + pairwise prep (trivial for IV)
#   2. netmeta() fit
#   3. hatmatrix(method="Davies", type="long")
#   4. decomp.design + netsplit (NMAresults / SIDE / dbt)
#   5. metagen pairwise heterogeneity
#   6. extract IV weights for our Rust port's per-study redistribution
#
# Skipped on purpose:
#   * netcontrib(method="shortestpath", study=TRUE) — quadratic in
#     network size, several hours on Fallers (139 treatments,
#     9591 lower-triangle comparisons).
#
# Outputs:
#   * fallers_nma_response.json — runNMA-shape JSON minus contribMatrix /
#     studyContributions (those will be filled in by Rust).
#   * iv_weights_netmeta.json   — per-(study, comparison) IV weights.
#   * fallers_timings.json      — wall time per stage.

suppressPackageStartupMessages({
  library(jsonlite)
  library(netmeta)
  library(meta)
})

CSV <- "../cinema/webapp/test/Fallers.csv"
OUT_DIR <- "test/contribution/fallers"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

D <- read.csv(CSV, stringsAsFactors = FALSE)
cat(sprintf("Fallers: %d contrasts, %d studies, %d treatments\n",
            nrow(D), length(unique(D$id)),
            length(unique(c(D$t1, D$t2)))))

# ── helper ───────────────────────────────────────────────────────────
norm_pair <- function(t1, t2)
  paste(sort(c(as.character(t1), as.character(t2))), collapse = ":")

stage <- function(label, expr) {
  t0 <- Sys.time()
  result <- expr
  dt <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  cat(sprintf("  %-30s %7.2f s\n", label, dt))
  attr(result, "wall") <- dt
  result
}

timings <- list()

# ── 1. netmeta fit ─────────────────────────────────────────────────
nma <- stage("netmeta()", {
  netmeta(effect, se, t1, t2, id,
          data = D, sm = "OR",
          common = TRUE, random = TRUE,
          tol.multiarm = 0.05)
})
timings$netmeta <- attr(nma, "wall")

# ── 2. hatmatrix ────────────────────────────────────────────────────
hm <- stage("hatmatrix Davies long", {
  hatmatrix(nma, method = "Davies", type = "long")
})
timings$hatmatrix <- attr(hm, "wall")
H <- hm$random
cat(sprintf("    H shape: %d x %d\n", nrow(H), ncol(H)))

# ── 3. design-by-treatment + netsplit ──────────────────────────────
dd <- stage("decomp.design", { decomp.design(nma) })
timings$decomp_design <- attr(dd, "wall")

ss <- stage("netsplit", { netsplit(nma) })
timings$netsplit <- attr(ss, "wall")

# ── 4. pairwise heterogeneity (metagen subgroup) ──────────────────
pw <- stage("metagen pairwise heterogeneity", {
  comp <- paste(D$t1, D$t2, sep = ":")
  metagen(D$effect, D$se, sm = "OR",
          common = FALSE, random = TRUE,
          subgroup = comp)
})
timings$metagen_pw <- attr(pw, "wall")

# ── 5. IV weights for Rust per-study redistribution ───────────────
weights <- stage("extract IV weights", {
  seTE_adj <- if (!is.null(nma$seTE.adj.random)) nma$seTE.adj.random
              else if (!is.null(nma$seTE.adj))   nma$seTE.adj
              else                                D$se
  w <- 1 / (seTE_adj ^ 2 + nma$tau ^ 2)
  recs <- lapply(seq_len(nrow(D)), function(i) {
    list(study      = as.character(D$id[i]),
         comparison = norm_pair(D$t1[i], D$t2[i]),
         weight     = w[i])
  })
  recs
})
timings$iv_weights <- attr(weights, "wall")

# ── 6. Empirical netcontrib estimate via connected sub-networks ──
# Pick the largest connected component, then nested subsets of it, run
# netcontrib on each, and extrapolate to the full network.
treats_full <- unique(c(D$t1, D$t2))
n_comp_full <- length(treats_full) * (length(treats_full) - 1) / 2
cat(sprintf("\nProfiling netcontrib on connected sub-networks (full: %d treatments → %d rows):\n",
            length(treats_full), n_comp_full))

# Largest connected component via netconnection.
nc_info <- netconnection(treat1 = D$t1, treat2 = D$t2, studlab = D$id)
comp_id <- nc_info$subnet
trt_to_comp <- setNames(comp_id, nc_info$treats)
biggest <- as.integer(names(sort(table(comp_id), decreasing = TRUE))[1])
in_big <- trt_to_comp[D$t1] == biggest & trt_to_comp[D$t2] == biggest
D_main <- D[in_big, ]
cat(sprintf("  largest sub-network: %d treatments, %d contrasts\n",
            length(unique(c(D_main$t1, D_main$t2))), nrow(D_main)))

# Run netcontrib on a moderately sized prefix of the largest component.
target_sizes <- c(15, 25, 40)
profile_rows <- list()
treats_main <- unique(c(D_main$t1, D_main$t2))
for (n_t in target_sizes) {
  if (n_t > length(treats_main)) next
  treats_sub <- treats_main[seq_len(n_t)]
  D_sub <- D_main[D_main$t1 %in% treats_sub & D_main$t2 %in% treats_sub, ]
  if (nrow(D_sub) < 3) next
  res <- tryCatch({
    nma_sub <- netmeta(effect, se, t1, t2, id, data = D_sub,
                       sm = "OR", common = TRUE, random = TRUE,
                       tol.multiarm = 0.05)
    t0 <- Sys.time()
    nc_sub <- netcontrib(nma_sub, method = "shortestpath", study = TRUE)
    list(t = as.numeric(difftime(Sys.time(), t0, units = "secs")),
         t_obs = length(unique(c(D_sub$t1, D_sub$t2))))
  }, error = function(e) NULL)
  if (!is.null(res)) {
    rows <- res$t_obs * (res$t_obs - 1) / 2
    cat(sprintf("  %3d treatments → %5d hat-rows : netcontrib = %7.2f s\n",
                res$t_obs, rows, res$t))
    profile_rows[[length(profile_rows) + 1]] <-
      list(treatments = res$t_obs, rows = rows, time_s = res$t)
  }
}

if (length(profile_rows) >= 2) {
  # Fit power-law: log time = α + β log rows.
  lr <- log(sapply(profile_rows, function(p) p$rows))
  lt <- log(sapply(profile_rows, function(p) p$time_s))
  fit <- lm(lt ~ lr)
  beta <- coef(fit)[2]
  est <- exp(coef(fit)[1] + beta * log(n_comp_full))
  cat(sprintf("  → power-law fit: time ≈ rows^%.2f\n", beta))
  cat(sprintf("  → extrapolated full netcontrib: %.0f s = %.1f min = %.2f h\n",
              est, est/60, est/3600))
  timings$netcontrib_profile        <- profile_rows
  timings$netcontrib_power_exponent <- as.numeric(beta)
  timings$netcontrib_estimate_s     <- as.numeric(est)
}

# ── 7. Build runNMA-shape response (without contribMatrix) ─────────

convert_matrix <- function(m) {
  list(data     = unname(lapply(seq_len(nrow(m)),
                                function(i) as.list(m[i, ]))),
       rowNames = rownames(m),
       colNames = colnames(m))
}

pick <- function(field, sub) {
  obj <- if (!is.null(ss[[paste0(field, ".random")]])) ss[[paste0(field, ".random")]] else NULL
  if (is.null(obj)) return(rep(NA, length(ss$comparison)))
  obj[[sub]]
}
side <- data.frame(
  comparison  = ss$comparison,
  Direct      = c(pick("direct",  "TE")),
  DirectL     = c(pick("direct",  "lower")),
  DirectU     = c(pick("direct",  "upper")),
  Indirect    = c(pick("indirect","TE")),
  IndirectL   = c(pick("indirect","lower")),
  IndirectU   = c(pick("indirect","upper")),
  SideIF      = c(pick("compare", "TE")),
  SideIFlower = c(pick("compare", "lower")),
  SideIFupper = c(pick("compare", "upper")),
  SideZ       = c(pick("compare", "z")),
  SidePvalue  = c(pick("compare", "p")),
  PropDir     = c(if (!is.null(ss$prop.random)) ss$prop.random else NA),
  stringsAsFactors = FALSE
)

TE_mat   <- nma$TE.random
seTE_mat <- nma$seTE.random
lower_mat <- nma$lower.random
upper_mat <- nma$upper.random
propD <- if (!is.null(nma$prop.direct.random)) nma$prop.direct.random else NA
TE.nma   <- -TE_mat[lower.tri(TE_mat)]
seTE.nma <- seTE_mat[lower.tri(seTE_mat)]
LCI.nma  <- -upper_mat[lower.tri(upper_mat)]
UCI.nma  <- -lower_mat[lower.tri(lower_mat)]
PrL.nma  <- -nma$upper.predict[lower.tri(nma$upper.predict)]
PrU.nma  <- -nma$lower.predict[lower.tri(nma$lower.predict)]
treatnames <- rownames(TE_mat)

NMAresults <- data.frame(
  TE       = TE.nma,
  seTE     = seTE.nma,
  lowerCI  = LCI.nma,
  upperCI  = UCI.nma,
  lowerPrI = PrL.nma,
  upperPrI = PrU.nma,
  PropDir  = propD,
  `_row`   = (function() {
    ids <- character(0)
    n <- length(treatnames)
    for (j in seq_len(n)) for (i in seq_len(n)) if (i > j)
      ids <- c(ids, paste0(treatnames[j], ":", treatnames[i]))
    ids
  })(),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

# Pairwise heterogeneity rows
Pairwise <- if (is.null(pw)) data.frame() else
  data.frame(comparison = pw$subgroup.levels,
             tau2       = c(pw$tau.w^2),
             I2         = c(pw$I2.w),
             I2lower    = c(pw$lower.I2.w),
             I2upper    = c(pw$upper.I2.w),
             stringsAsFactors = FALSE)

dbt <- {
  if (!is.null(dd$Q.decomp)) as.data.frame(dd$Q.decomp)
  else                       data.frame(Q = 0, df = 0, pval = 1)
}

forleaguetable <- list(
  TE.fixed     = convert_matrix(if (!is.null(nma$TE.common)) nma$TE.common else nma$TE.fixed),
  lower.fixed  = convert_matrix(if (!is.null(nma$lower.common)) nma$lower.common else nma$lower.fixed),
  upper.fixed  = convert_matrix(if (!is.null(nma$upper.common)) nma$upper.common else nma$upper.fixed),
  TE.random    = convert_matrix(nma$TE.random),
  lower.random = convert_matrix(nma$lower.random),
  upper.random = convert_matrix(nma$upper.random),
  treatnames   = treatnames
)

resp <- list(
  H         = convert_matrix(H),
  rowNames  = rownames(H),
  colNames  = colnames(H),
  NMAresults = NMAresults,
  side      = side,
  NMAheter  = list(tau2 = nma$tau^2,
                   Qoverall = nma$Q,
                   Qheterogeneity = nma$Q.heterogeneity,
                   Qinconsistency = nma$Q.inconsistency),
  Pairwise  = Pairwise,
  dbt       = dbt,
  forleaguetable = forleaguetable,
  contribMatrix      = NULL,  # Rust will fill these in
  studyContributions = NULL,
  model     = "random",
  sm        = "OR",
  tau       = nma$tau,
  treatnames = treatnames
)

writeLines(toJSON(resp, auto_unbox = TRUE, null = "null", na = "null"),
           file.path(OUT_DIR, "fallers_nma_response.json"))
writeLines(toJSON(weights, auto_unbox = TRUE, pretty = TRUE),
           file.path(OUT_DIR, "iv_weights_netmeta.json"))
writeLines(toJSON(timings, auto_unbox = TRUE, pretty = TRUE),
           file.path(OUT_DIR, "fallers_timings.json"))
cat("\nWrote fallers_nma_response.json, iv_weights_netmeta.json, fallers_timings.json\n")
