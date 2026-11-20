#!/usr/bin/env Rscript
# Example 9 — Continuous outcomes: the natural spring.
#
# A Gaussian patient is a Hooke spring: linear force law, constant stiffness
# 1/σ². Two consequences that are only approximate for binary data are EXACT
# here:
#   * no aggregation bias — the average of linear force laws is the force law
#     of the average patient, so an AgD arm is exactly the spring at its mean
#     covariates (ML-NMR's integration is not needed for the identity link);
#   * a site's patients are exactly one equivalent spring — its sufficient
#     statistics (XᵀX, Xᵀy, yᵀy, n) reproduce their energy everywhere — so a
#     federated analysis needs ONE round and no correction.
#
# Simulated network (known truth): 4 treatments (A = control; B, C, D one
# active class), 3 IPD trials + 3 AgD trials, covariates x1 (continuous effect
# modifier) and x2 (binary prognostic), random effects τ = 0.4, study-specific σ.
#   (1) springs vs multinma (Stan; normal likelihood, identity link), FE and RE;
#   (2) federated: each IPD site sends its equivalent spring once; = pooled fit.
#
# Output: results/09_continuous.md, results/09_*.csv

source("examples/vs-multinma/common.R")
options(mc.cores = parallel::detectCores())
set.seed(20261114)

# ---------------------------------------------------------------- simulate
truth <- list(d = c(B = -2.0, C = -3.0, D = -2.5), b1 = 1.0, b2 = 0.5, g1 = -0.8, tau = 0.4)
sim_trial <- function(study, trts, n_arm, x1_mean, x2_p, mu, sigma) {
  delta <- c(0, rnorm(length(trts) - 1, 0, truth$tau))        # RE on non-control arms
  names(delta) <- trts
  do.call(rbind, lapply(trts, function(t) {
    x1 <- rnorm(n_arm, x1_mean, 1); x2 <- rbinom(n_arm, 1, x2_p)
    active <- t != "A"
    eff <- if (active) truth$d[[t]] + delta[[t]] + truth$g1 * x1 else 0
    if (!active && trts[1] != "A") eff <- delta[[t]]
    y <- mu + truth$b1 * x1 + truth$b2 * x2 + eff + rnorm(n_arm, 0, sigma)
    data.frame(study = study, trt = t, x1 = x1, x2 = x2, y = y)
  }))
}
ipd <- rbind(sim_trial("IPD-1", c("A", "B"), 150, 0.0, 0.3, 20, 3.0),
             sim_trial("IPD-2", c("A", "C"), 150, 0.4, 0.5, 22, 4.0),
             sim_trial("IPD-3", c("A", "B", "C"), 120, -0.3, 0.4, 19, 3.5))
agd_pat <- rbind(sim_trial("AgD-4", c("A", "D"), 140, 0.8, 0.6, 21, 3.5),
                 sim_trial("AgD-5", c("B", "D"), 140, 1.2, 0.4, 20, 4.0),
                 sim_trial("AgD-6", c("A", "C", "D"), 110, -0.6, 0.5, 23, 3.0))
# AgD trials publish arm means, SEs and covariate summaries only
agd <- do.call(rbind, lapply(split(agd_pat, list(agd_pat$study, agd_pat$trt), drop = TRUE), function(d)
  data.frame(study = d$study[1], trt = d$trt[1], y = mean(d$y), se = sd(d$y) / sqrt(nrow(d)),
             n = nrow(d), x1_mean = mean(d$x1), x1_sd = sd(d$x1), x2 = mean(d$x2))))
cls <- function(t) ifelse(t == "A", "control", "active")
ipd$trtclass <- cls(ipd$trt); agd$trtclass <- cls(agd$trt)
net <- combine_network(
  set_ipd(ipd, study, trt, y = y, trt_class = trtclass),
  set_agd_arm(agd, study, trt, y = y, se = se, sample_size = n, trt_class = trtclass),
  trt_ref = "A")
net <- suppressMessages(add_integration(net, x1 = distr(qnorm, mean = x1_mean, sd = x1_sd),
                                        x2 = distr(qbern, prob = x2), n_int = 64))
reg <- ~(x1 + x2) * .trt
info <- sprintf("%d IPD patients in 3 trials + %d AgD arms in 3 trials; treatments A (control), B, C, D (one active class); `%s`, class-common effect modification; true d = (%s), β1 = %.1f, β2 = %.1f, γ(x1 × active) = %.1f, τ = %.1f.",
                nrow(ipd), nrow(agd), deparse(reg), paste(sprintf("%s %.1f", names(truth$d), truth$d), collapse = ", "),
                truth$b1, truth$b2, truth$g1, truth$tau)
cat(info, "\n")

# ---------------------------------------------------------------- fits
PR <- list(intercept = 100, trt = 10, reg = 10, aux = 5)
sp_fit <- function(te, ...) springs::mlnmr(net, regression = reg, class_interactions = "common",
  likelihood = "normal", trt_effects = te, prior_intercept = PR$intercept, prior_trt = PR$trt,
  prior_reg = PR$reg, prior_aux = PR$aux, ...)
CACHE <- file.path(EX_DIR, ".cache"); dir.create(CACHE, showWarnings = FALSE)
mn_fit <- function(te) {
  key <- file.path(CACHE, sprintf("09_mn_%s.rds", te))
  if (file.exists(key)) return(readRDS(key))
  t <- system.time(fit <- suppressWarnings(nma(net, trt_effects = te, likelihood = "normal",
    link = "identity", regression = reg, class_interactions = "common",
    prior_intercept = normal(scale = PR$intercept), prior_trt = normal(scale = PR$trt),
    prior_reg = normal(scale = PR$reg), prior_aux = half_normal(scale = PR$aux),
    prior_het = half_normal(scale = 2.5), QR = TRUE, chains = MN_CHAINS, iter = MN_ITER,
    seed = 1, refresh = 0, show_messages = FALSE)))
  out <- list(fit = fit, wall = unname(t["elapsed"])); saveRDS(out, key); out
}
zs <- function(a, b) {
  m <- merge(a, b, by = "parameter")
  data.frame(parameter = m$parameter, springs = m$mean.x, multinma = m$mean.y, springs_sd = m$sd.x,
             multinma_sd = m$sd.y, z = (m$mean.x - m$mean.y) / m$sd.y, sd_ratio = m$sd.x / m$sd.y)
}
summ <- function(z) sprintf("max |z| %.3f (median %.3f), SD ratio %.2f–%.2f, %d quantities",
  max(abs(z$z)), median(abs(z$z)), min(z$sd_ratio), max(z$sd_ratio), nrow(z))

md <- c("## Example 9 — Continuous outcomes: the natural spring", "", info, "")
res <- list()
for (te in c("fixed", "random")) {
  cat("==", te, "\n")
  sp <- sp_fit(te, tau_priors = c("hn:2.5", "hn:1", "hn:0.5", "hc:1", "ln:-2.56:1.74"))
  mn <- mn_fit(te)
  ms <- as.data.frame(summary(mn$fit, pars = c("d", "beta", "sigma", if (te == "random") "tau")))
  # multinma indexes residual SDs by IPD arm (sigma[1..]); springs names them sigma[study: trt]
  sg <- grep("^sigma\\[", sp$summary$parameter, value = TRUE)
  ms$parameter[startsWith(ms$parameter, "sigma[")] <- sg[as.integer(sub("sigma\\[(\\d+)\\]", "\\1",
    ms$parameter[startsWith(ms$parameter, "sigma[")]))]
  ss <- sp$summary[, c("parameter", "mean", "sd")]
  if (te == "random") ss <- rbind(ss, data.frame(parameter = "tau", mean = sp$tau$mean[1], sd = sp$tau$sd[1]))
  zp <- zs(ss, ms[, c("parameter", "mean", "sd")])
  re_mn <- as.data.frame(relative_effects(mn$fit)); re_sp <- springs::mlnmr_relative_effects(sp)
  zr <- zs(re_sp[, c("parameter", "mean", "sd")], re_mn[, c("parameter", "mean", "sd")])
  write.csv(rbind(cbind(what = "parameters", zp), cbind(what = "relative effects", zr)),
            file.path(OUT_DIR, sprintf("09_%s_vs_multinma.csv", te)), row.names = FALSE)
  res[[te]] <- list(sp = sp, mn = mn, zp = zp)
  dtab <- zp[startsWith(zp$parameter, "d[") | zp$parameter %in% c("tau", "beta[x1:.trtclassactive]"), ]
  dtab$truth <- c(truth$d, g1 = truth$g1, tau = truth$tau)[match(sub("^d\\[(.*)\\]$", "\\1",
                  sub("beta\\[x1:.trtclassactive\\]", "g1", dtab$parameter)), c(names(truth$d), "g1", "tau"))]
  md <- c(md, sprintf("### (1%s) %s effects", if (te == "fixed") "a" else "b", te), "",
    sprintf("* **springs %.2f s**%s vs **multinma %.0f s**: %.0f× faster. Importance ESS %.0f%s.",
            sp$wall_sec, if (te == "random") sprintf(" (all %d τ priors)", nrow(sp$priors)) else "",
            mn$wall, mn$wall / sp$wall_sec, sp$ess[1],
            if (te == "random") sprintf("; τ springs %.3f vs multinma %.3f (truth %.1f)", sp$tau$mean[1],
                                        ms$mean[ms$parameter == "tau"], truth$tau) else ""),
    sprintf("* d, β, σ%s: %s", if (te == "random") ", τ" else "", summ(zp)),
    sprintf("* population-specific relative effects (6 study populations × 3): %s", summ(zr)), "",
    md_table(dtab[, c("parameter", "truth", "springs", "multinma", "springs_sd", "multinma_sd", "z")], 3), "")
}

# ---------------------------------------------------------------- (2) federated, one round
cat("== federated\n")
SITE_BIN <- sub("mlnmr$", "mlnmr_site", springs:::mlnmr_engine_bin())
work <- file.path(tempdir(), "fed9"); dir.create(work, showWarnings = FALSE)
b <- springs::mlnmr(net, regression = reg, class_interactions = "common", likelihood = "normal",
                    prior_intercept = PR$intercept, prior_trt = PR$trt, prior_reg = PR$reg,
                    prior_aux = PR$aux, .build_only = TRUE)
P <- length(b$par_names); sites <- unique(b$unit_study)
t0 <- Sys.time()
shared <- list(); sent <- setNames(numeric(length(sites)), sites)
for (s in sites) {
  f <- file.path(work, paste0(make.names(s), ".json"))
  jsonlite::write_json(list(link = "identity", p = P, units = b$units[b$unit_study == s]), f,
                       auto_unbox = TRUE, digits = NA)
  if (s %in% ipd$study) {
    qf <- file.path(work, "q.json"); of <- file.path(work, "o.json")
    jsonlite::write_json(list(mode = "equivalent_spring"), qf, auto_unbox = TRUE)
    stopifnot(identical(system2(SITE_BIN, c("--data", f, "--query", qf, "--output", of)), 0L))
    eq <- jsonlite::fromJSON(of, simplifyVector = FALSE)$suff
    for (e in eq) {
      shared[[length(shared) + 1]] <- list(kind = "suff_normal", n = e$n, q = 1L, sig = e$sig,
        xtx = unlist(e$xtx), xty = unlist(e$xty), yty = e$yty)
      sent[[s]] <- sent[[s]] + length(unlist(e$xtx)) + length(unlist(e$xty)) + 2
    }
  } else {                                  # AgD: the published arms, as they are
    for (u in b$units[b$unit_study == s]) { shared[[length(shared) + 1]] <- u; sent[[s]] <- sent[[s]] + 3 }
  }
}
eng_pr <- lapply(b$par_names, function(j) { g <- springs:::.mlnmr_par_group(j)
  pr <- if (is.na(g)) list(dist = "flat") else b$prior_spec[[g]]
  if (pr$dist == "flat") list(dist = "flat") else pr[c("dist", "loc", "scale", "df")] })
inp <- file.path(work, "fed_in.json"); outp <- file.path(work, "fed_out.json")
jsonlite::write_json(list(link = "identity", par_names = b$par_names, priors = eng_pr, units = shared,
                          samples = 4000, seed = 1, init = b$init), inp, auto_unbox = TRUE, digits = NA)
stopifnot(identical(system2(springs:::mlnmr_engine_bin(), c("--input", inp, "--output", outp),
                            stdout = FALSE, stderr = FALSE), 0L))
fed <- jsonlite::fromJSON(outp, simplifyVector = TRUE)
t_fed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
fe <- res$fixed$sp
mode_diff <- max(abs(fed$mode - fe$mode[fed$fixed_names]))
fp <- fed$priors$pars[[1]]; fp <- fp[!startsWith(fp$name, "log_sigma"), ]
pp <- fe$summary[match(fp$name, fe$summary$parameter), ]
post_diff <- max(abs(fp$mean - pp$mean) / pp$sd)
md <- c(md, "### (2) Federated in one round: the equivalent spring is exact", "",
  "Each IPD site runs one local query (`mlnmr_site --mode equivalent_spring`) and shares its sufficient statistics (XᵀX, Xᵀy, yᵀy, n) — aggregate quantities, no patient record. AgD sites share their published arms. Anyone can then fit the network.", "",
  md_table(data.frame(site = sites, data = ifelse(sites %in% ipd$study, "IPD", "AgD"),
    `shared` = ifelse(sites %in% ipd$study, "equivalent spring (XᵀX, Xᵀy, yᵀy, n)", "published arm means, SEs, covariate summaries"),
    `numbers` = sent[sites], `patient records` = 0L, check.names = FALSE, row.names = NULL)), "",
  sprintf("* **One round**, %.2f s. Equilibrium = pooled fit to **%.1e**; posterior (same seed) = pooled to **%.1e** posterior SDs. No energy round and no importance correction are needed for the federation itself: the equivalent spring reproduces the patients' energy exactly (engine test `equivalent_spring_is_exact_for_gaussian_patients`).", t_fed, mode_diff, post_diff),
  "* Contrast with binary data (Examples 7–8): there the equivalent quadratic spring is only the Laplace approximation (0.24–0.36 SD off), and one extra round of energy evaluations restores exactness.",
  "* Residual SDs are arm-specific (as in multinma), so each site's equivalent spring is one block per arm (XᵀX, Xᵀy, yᵀy, n) and is shared individually, like richer AgD. With a common σ, the springs could also be combined by secure summation.", "")
writeLines(md, file.path(OUT_DIR, "09_continuous.md"))
cat(md, sep = "\n")
