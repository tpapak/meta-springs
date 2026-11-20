# Shared helpers for the springs-vs-multinma showcase.
#
# springs = the spring / grand-canonical (GC) engine via the R facade
#           (r-package/springs, Rust binaries in gc-rust/target/release).
# multinma = Phillippo's Stan-based (M)NMA / ML-NMR package.
#
# Both are fitted to the SAME arm-level data with MATCHED priors:
#   treatment effects  ~ effectively flat  (springs: flat; multinma: N(0, 100^2))
#   study baselines    ~ effectively flat  (springs: flat; multinma: N(0, 100^2))
#   heterogeneity      ~ the prior under study (default HalfNormal(tau; 1))

`%||%` <- function(a, b) if (is.null(a)) b else a

suppressPackageStartupMessages({
  library(springs)
  library(multinma)
  library(jsonlite)
})

# Run all scripts from the repository root:  Rscript examples/vs-multinma/01_speed.R
if (!dir.exists("examples/vs-multinma")) stop("run from the meta-springs repository root")
EX_DIR  <- normalizePath("examples/vs-multinma")
OUT_DIR <- file.path(EX_DIR, "results")
dir.create(OUT_DIR, showWarnings = FALSE)
REPO    <- normalizePath(file.path(EX_DIR, "..", ".."))

MN_CHAINS <- 4L
MN_ITER   <- 2000L          # multinma default: 1000 warm-up + 1000 sampling per chain
MN_CORES  <- 4L             # chains in parallel (generous to multinma)
options(mc.cores = MN_CORES)



# ---- data adaptors ---------------------------------------------------------

# multinma-style arm data -> gemtc-shaped data.ab for springs
to_springs_ab <- function(d, study, trt, r = NULL, n = NULL, y = NULL, se = NULL) {
  out <- data.frame(study = as.character(d[[study]]),
                    treatment = as.character(d[[trt]]), stringsAsFactors = FALSE)
  if (!is.null(r)) {
    out$responders <- d[[r]]; out$sampleSize <- d[[n]]
  } else {
    out$mean <- d[[y]]; out$std.err <- d[[se]]
  }
  out
}

# network spec (list with d, study, trt, r/n or y/se, ref) -> multinma network
mn_network <- function(nt) {
  dd <- data.frame(.s = nt[["d"]][[nt[["study"]]]], .t = as.character(nt[["d"]][[nt[["trt"]]]]))
  if (!is.null(nt[["r"]])) {
    dd$.r <- nt[["d"]][[nt[["r"]]]]; dd$.n <- nt[["d"]][[nt[["n"]]]]
    set_agd_arm(dd, study = .s, trt = .t, r = .r, n = .n, trt_ref = nt$ref)
  } else {
    dd$.y <- nt[["d"]][[nt[["y"]]]]; dd$.se <- nt[["d"]][[nt[["se"]]]]
    set_agd_arm(dd, study = .s, trt = .t, y = .y, se = .se, trt_ref = nt$ref)
  }
}

# network spec -> springs (gemtc-shaped) data.ab
sp_ab <- function(nt) {
  if (!is.null(nt[["r"]])) to_springs_ab(nt[["d"]], nt[["study"]], nt[["trt"]], r = nt[["r"]], n = nt[["n"]])
  else                to_springs_ab(nt[["d"]], nt[["study"]], nt[["trt"]], y = nt[["y"]], se = nt[["se"]])
}

# Engine-native nmadb JSON -> data.frame
read_nmadb <- function(path) {
  d <- fromJSON(path)
  d$study <- as.character(d$study); d$treatment <- as.character(d$treatment)
  d
}

# ---- multinma prior translation --------------------------------------------
# spec strings shared with the springs engine:
#   hn:S  hc:S  exp:R  ln:MU:SIGMA (on tau^2)  unif:U  flat
mn_prior <- function(spec) {
  p <- strsplit(spec, ":")[[1]]
  switch(p[1],
    hn   = list(prior = half_normal(scale = as.numeric(p[2])), type = "sd"),
    hc   = list(prior = half_cauchy(scale = as.numeric(p[2])), type = "sd"),
    exp  = list(prior = exponential(rate = as.numeric(p[2])), type = "sd"),
    ln   = list(prior = log_normal(location = as.numeric(p[2]), scale = as.numeric(p[3])),
                type = "var"),
    NULL)   # unif / flat: not available as a heterogeneity prior in multinma
}

# ---- fitting wrappers --------------------------------------------------------

# multinma RE NMA; returns timing, ESS and posterior summaries vs `ref`
fit_multinma <- function(net, prior_spec = "hn:1", seed = 1, trt_effects = "random") {
  pr <- mn_prior(prior_spec)
  if (is.null(pr)) return(NULL)
  t <- system.time(
    fit <- suppressWarnings(suppressMessages(
      nma(net, trt_effects = trt_effects,
          prior_intercept = normal(scale = 100), prior_trt = normal(scale = 100),
          prior_het = pr$prior, prior_het_type = pr$type,
          chains = MN_CHAINS, iter = MN_ITER, seed = seed,
          refresh = 0, show_messages = FALSE))))
  s <- as.data.frame(summary(fit, pars = c("d", if (trt_effects == "random") "tau")))
  list(fit = fit, wall = unname(t["elapsed"]), summ = s)
}

# springs single-prior fit with draws (the gemtc-shaped path)
fit_springs <- function(ab, prior_spec = "hn:1", n_draws = 4000, fixed = FALSE) {
  t <- system.time({
    net <- springs::mtc.network(ab)
    mod <- springs::mtc.model(net, linearModel = if (fixed) "fixed" else "random",
                              hy.prior = prior_spec)
    res <- springs::mtc.run(mod, n.iter = n_draws)
  })
  list(res = res, wall = unname(t["elapsed"]), engine = res$fit_wall_sec)
}

# springs draws re-expressed against reference `ref`: S x (T-1) matrix d[ref, t]
springs_draws_vs <- function(res, ref) {
  D <- cbind(0, res$draws)
  colnames(D) <- res$treatments
  out <- D[, setdiff(res$treatments, ref), drop = FALSE] - D[, ref]
  out
}

# Side-by-side table of basic effects d[ref, t] and tau
compare_effects <- function(sp, mn, ref) {
  D <- springs_draws_vs(sp$res, ref)
  ms <- mn$summ
  rows <- lapply(colnames(D), function(t) {
    k <- which(ms$parameter == sprintf("d[%s]", t))
    data.frame(parameter = sprintf("d[%s]", t),
               springs_mean = mean(D[, t]), springs_sd = sd(D[, t]),
               multinma_mean = ms$mean[k], multinma_sd = ms$sd[k],
               multinma_ess = ms$Bulk_ESS[k])
  })
  if (!sp$res$fixed) {
    k <- which(ms$parameter == "tau")
    rows[[length(rows) + 1]] <- data.frame(
      parameter = "tau", springs_mean = mean(sp$res$sd_d), springs_sd = sd(sp$res$sd_d),
      multinma_mean = ms$mean[k], multinma_sd = ms$sd[k], multinma_ess = ms$Bulk_ESS[k])
  }
  out <- do.call(rbind, rows)
  # discrepancy in units of the MCMC posterior SD (|z| < 0.1 is MCMC-noise level)
  out$z_diff <- (out$springs_mean - out$multinma_mean) / out$multinma_sd
  out$sd_ratio <- out$springs_sd / out$multinma_sd
  out
}

fmt <- function(x, d = 3) formatC(as.numeric(x), format = "f", digits = d)
md_table <- function(df, digits = 3) {
  paste(knitr::kable(df, format = "pipe", digits = digits, row.names = FALSE), collapse = "\n")
}
