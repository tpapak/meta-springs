#!/usr/bin/env Rscript
# Example 8 — The FULL plaque psoriasis ML-NMR network (Phillippo et al. 2020/2023):
#   4 IPD trials (UNCOVER-1/2/3, IXORA-S; ~4 100 patients) + 5 AgD trials
#   (CLEAR, ERASURE, FEATURE, FIXTURE, JUNCTURE); 7 treatments in 3 active
#   classes (IL-17, IL-12/23, TNFα blockers); PASI 75, probit, 5 covariates,
#   class-common effect modification, two-parameter binomial AgD likelihood,
#   64 integration points. Includes a 0/59 placebo arm (FEATURE).
#
# (1) springs vs multinma, FE and RE: parameters, population-specific effects in
#     all 9 study populations, population-average predictions.
# (2) Prior investigation from one fit (FE: 8 priors; RE: 4 x 8 = 32).
# (3) Federated: 9 sites (4 IPD + 5 AgD), ring secure sum, no coordinator.
#
# Output: results/08_full_network.md, results/08_*.csv

source("examples/vs-multinma/common.R")
suppressPackageStartupMessages({ library(dplyr) })
options(mc.cores = parallel::detectCores())

cls <- function(trtn) case_when(trtn == 1 ~ "Placebo", trtn %in% c(2, 3, 5, 6) ~ "IL-17 blocker",
                                trtn == 4 ~ "TNFa blocker", trtn == 7 ~ "IL-12/23 blocker")
f_ipd <- plaque_psoriasis_ipd %>%
  mutate(bsa = bsa / 100, prevsys = as.numeric(prevsys), psa = as.numeric(psa),
         weight = weight / 10, durnpso = durnpso / 10, trtclass = cls(trtn),
         complete = complete.cases(durnpso, prevsys, bsa, weight, psa)) %>% filter(complete)
f_agd <- plaque_psoriasis_agd %>%
  mutate(bsa_mean = bsa_mean / 100, bsa_sd = bsa_sd / 100, prevsys = prevsys / 100,
         psa = psa / 100, weight_mean = weight_mean / 10, weight_sd = weight_sd / 10,
         durnpso_mean = durnpso_mean / 10, durnpso_sd = durnpso_sd / 10, trtclass = cls(trtn))
net <- combine_network(
  set_ipd(f_ipd, study = studyc, trt = trtc, r = pasi75, trt_class = trtclass),
  set_agd_arm(f_agd, study = studyc, trt = trtc, r = pasi75_r, n = pasi75_n,
              trt_class = trtclass, sample_size = sample_size_w0),
  trt_ref = "PBO")
net <- suppressMessages(add_integration(net,
  durnpso = distr(qgamma, mean = durnpso_mean, sd = durnpso_sd),
  prevsys = distr(qbern, prob = prevsys),
  bsa = distr(qlogitnorm, mean = bsa_mean, sd = bsa_sd),
  weight = distr(qgamma, mean = weight_mean, sd = weight_sd),
  psa = distr(qbern, prob = psa), n_int = 64))
reg <- ~(durnpso + prevsys + bsa + weight + psa) * .trt
tau_set <- c("hn:2.5", "hn:1", "hn:0.5", "hc:1", "exp:2", "unif:2", "ln:-2.56:1.74", "ln:-3.95:1.34")
target <- data.frame(durnpso = 2.5, prevsys = 1, bsa = 0.6, weight = 12, psa = 1, row.names = "target")

net_info <- sprintf("%d IPD patients in %d trials (%s) + %d AgD arms in %d trials (%s); %d treatments; %d springs",
  nrow(f_ipd), length(unique(f_ipd$studyc)), paste(unique(f_ipd$studyc), collapse = ", "),
  nrow(f_agd), length(unique(f_agd$studyc)), paste(unique(f_agd$studyc), collapse = ", "),
  nlevels(net$treatments), nrow(f_ipd) + nrow(f_agd))
cat(net_info, "\n")

sp_fit <- function(te, ...) springs::mlnmr(net, regression = reg, class_interactions = "common",
  link = "probit", likelihood = "bernoulli2", trt_effects = te, ...)
CACHE <- file.path(EX_DIR, ".cache"); dir.create(CACHE, showWarnings = FALSE)
mn_fit <- function(te, prior_reg = normal(scale = 10), prior_het = half_normal(scale = 2.5)) {
  key <- file.path(CACHE, sprintf("08_mn_%s_%s_%s.rds", te, prior_reg$scale, prior_het$scale))
  if (file.exists(key)) return(readRDS(key))
  t <- system.time(fit <- suppressWarnings(nma(net, trt_effects = te, link = "probit",
    likelihood = "bernoulli2", regression = reg, class_interactions = "common",
    prior_intercept = normal(scale = 10), prior_trt = normal(scale = 10), prior_reg = prior_reg,
    prior_het = prior_het, init_r = 0.1, QR = TRUE, chains = MN_CHAINS, iter = MN_ITER,
    seed = 1, refresh = 0, show_messages = FALSE)))
  out <- list(fit = fit, wall = unname(t["elapsed"])); saveRDS(out, key); out
}
zs <- function(a, b) {   # a (springs) vs b (multinma) on matching parameter names
  m <- merge(a, b, by = "parameter")
  data.frame(parameter = m$parameter, springs = m$mean.x, multinma = m$mean.y,
             springs_sd = m$sd.x, multinma_sd = m$sd.y, z = (m$mean.x - m$mean.y) / m$sd.y,
             sd_ratio = m$sd.x / m$sd.y)
}
summ <- function(z) sprintf("max |z| %.3f (median %.3f), SD ratio %.2f–%.2f, %d quantities",
  max(abs(z$z)), median(abs(z$z)), min(z$sd_ratio), max(z$sd_ratio), nrow(z))

md <- c("## Example 8 — The full plaque psoriasis network: 4 IPD + 5 AgD trials", "",
  paste0(net_info, ". PASI 75, probit, `", deparse(reg), "`, class-common interactions (IL-17 / IL-12/23 / TNFα blockers), `bernoulli2`, 64 integration points, priors as in the multinma vignette. FEATURE's placebo arm has 0/59 responders."), "")

# ================================================= (1) springs vs multinma
res <- list()
for (te in c("fixed", "random")) {
  cat("==", te, "\n")
  sp <- sp_fit(te, tau_priors = tau_set, is_per_point = 2000)
  mn <- mn_fit(te)
  ms <- as.data.frame(summary(mn$fit, pars = c("d", "beta", if (te == "random") "tau")))
  ss <- sp$summary[!startsWith(sp$summary$parameter, "mu["), c("parameter", "mean", "sd")]
  if (te == "random") ss <- rbind(ss, data.frame(parameter = "tau", mean = sp$tau$mean[1], sd = sp$tau$sd[1]))
  zp <- zs(ss, ms[, c("parameter", "mean", "sd")])
  re_mn <- as.data.frame(relative_effects(mn$fit)); re_sp <- springs::mlnmr_relative_effects(sp)
  zr <- zs(re_sp[, c("parameter", "mean", "sd")], re_mn[, c("parameter", "mean", "sd")])
  pr_mn <- as.data.frame(predict(mn$fit, type = "response")); pr_sp <- springs::mlnmr_predict(sp)
  pr_sp$parameter <- sprintf("pred[%s: %s]", pr_sp$population, pr_sp$treatment)
  zq <- zs(pr_sp[, c("parameter", "mean", "sd")], pr_mn[, c("parameter", "mean", "sd")])
  write.csv(rbind(cbind(what = "parameters", zp), cbind(what = "relative effects", zr),
                  cbind(what = "predictions", zq)),
            file.path(OUT_DIR, sprintf("08_%s_vs_multinma.csv", te)), row.names = FALSE)
  ess_mn <- min(ms$Bulk_ESS, na.rm = TRUE)
  res[[te]] <- list(sp = sp, mn = mn, zp = zp, zr = zr, zq = zq)
  md <- c(md, sprintf("### (1%s) %s effects", if (te == "fixed") "a" else "b", te), "",
    sprintf("* **springs %.2f s** (%s) vs **multinma %.0f s** (4 chains × %d, min bulk ESS %.0f): %.0f× faster%s.",
            sp$wall_sec, if (te == "fixed") sprintf("importance ESS %.0f", sp$ess[1])
                         else sprintf("%d-point τ² grid, all %d τ priors", nrow(sp$grid), length(tau_set)),
            mn$wall, MN_ITER, ess_mn, mn$wall / sp$wall_sec,
            if (te == "random") sprintf("; τ springs %.3f vs multinma %.3f", sp$tau$mean[1], ms$mean[ms$parameter == "tau"]) else ""),
    sprintf("* d, β%s: %s", if (te == "random") ", τ" else "", summ(zp)),
    sprintf("* population-specific relative effects (9 study populations × 6): %s", summ(zr)),
    sprintf("* population-average predicted PASI 75 (9 × 7): %s", summ(zq)), "")
}
tabd <- res$fixed$zp[startsWith(res$fixed$zp$parameter, "d["), ]
tabd <- merge(tabd, res$random$zp[, c("parameter", "springs", "multinma", "z")], by = "parameter",
              suffixes = c(" FE", " RE"))
md <- c(md, "Treatment effects (probit, vs placebo, network-average population):", "",
        md_table(tabd[, c("parameter", "springs FE", "multinma FE", "z FE", "springs RE", "multinma RE", "z RE")], 3), "")

# ================================================= (2) prior investigation
cat("== priors\n")
fe <- res$fixed$sp; re <- res$random$sp
panel_fe <- list("vignette: all N(0, 10²)" = list(), "reg N(0, 1)" = list(reg = 1),
  "reg N(0, 0.5²)" = list(reg = 0.5), "reg N(0, 0.25²)" = list(reg = 0.25),
  "reg Cauchy(0, 0.5)" = list(reg = cauchy(0, 0.5)), "reg t3(0, 0.5)" = list(reg = student_t(0, 0.5, 3)),
  "trt N(0, 1) (sceptical)" = list(trt = 1), "intercept N(−1.5, 0.3²)" = list(intercept = normal(-1.5, 0.3)))
show <- c("d[IXE_Q2W]", "d[SEC_300]", "d[UST]", "d[ETN]")
t_pfe <- system.time(pfe <- springs::mlnmr_prior_panel(fe, panel_fe, pars = show, newdata = target))[["elapsed"]]
panel_re <- panel_fe[c("vignette: all N(0, 10²)", "reg N(0, 1)", "reg N(0, 0.5²)", "reg Cauchy(0, 0.5)")]
t_pre <- system.time(pre <- springs::mlnmr_prior_panel(re, panel_re, pars = show, newdata = target))[["elapsed"]]
write.csv(rbind(cbind(model = "FE", pfe), cbind(model = "RE", pre)),
          file.path(OUT_DIR, "08_prior_panel.csv"), row.names = FALSE)
wide <- function(p, pars) {
  x <- p[p$parameter %in% pars, ]
  x$v <- sprintf("%s (%s)", fmt(x$mean, 2), fmt(x$sd, 2))
  w <- reshape(x[, c("prior", "ess", "refit", "parameter", "v")], idvar = c("prior", "ess", "refit"),
               timevar = "parameter", direction = "wide")
  names(w) <- sub("^v\\.", "", names(w)); w$ess <- round(w$ess); w
}
# spot check vs multinma: FE under reg N(0, 0.5²)
cat("   multinma FE reg N(0, 0.5²)\n")
mn05 <- mn_fit("fixed", prior_reg = normal(scale = 0.5))
rw05 <- springs::mlnmr_reweight(fe, prior_reg = 0.5)
s05 <- rbind(rw05$summary[, c("parameter", "mean", "sd")],
             springs::mlnmr_relative_effects(rw05, newdata = target)[, c("parameter", "mean", "sd")])
m05 <- rbind(as.data.frame(summary(mn05$fit, pars = c("d", "beta")))[, c("parameter", "mean", "sd")],
             { r <- as.data.frame(relative_effects(mn05$fit, newdata = target))
               r$parameter <- sprintf("d[target: %s]", sub(".*: (.*)\\]$", "\\1", r$parameter))
               r[, c("parameter", "mean", "sd")] })
z05 <- zs(s05, m05)
taus_by <- pre[pre$parameter == "tau", ]
md <- c(md, "### (2) Prior investigation from one fit", "",
  sprintf("* FE: %d priors in **%.1f s** after the %.1f s fit; RE: %d interaction priors × %d τ priors = %d posteriors in **%.1f s** after the %.1f s fit. multinma would need %d + %d Stan fits (≈ %.0f min).",
          length(panel_fe), t_pfe, fe$wall_sec, length(panel_re), length(tau_set),
          length(panel_re) * length(tau_set), t_pre, re$wall_sec, length(panel_fe),
          length(panel_re) * length(tau_set),
          (length(panel_fe) * res$fixed$mn$wall + length(panel_re) * length(tau_set) * res$random$mn$wall) / 60),
  sprintf("* Spot check vs a multinma refit (FE, β ~ N(0, 0.5²); d, β and target-population effects): %s; multinma %.0f s.",
          summ(z05), mn05$wall),
  sprintf("* τ across the 8 τ priors (vignette interaction prior): posterior mean %.2f–%.2f.",
          min(taus_by$mean[taus_by$prior == "vignette: all N(0, 10²)"]),
          max(taus_by$mean[taus_by$prior == "vignette: all N(0, 10²)"])), "",
  "FE posterior mean (SD) under each prior; `target` = effect vs placebo in the transport population (25 y disease, 60% BSA, 120 kg, PsA, prior systemic):", "",
  md_table(wide(pfe, c(show, "d[target: IXE_Q2W]", "d[target: UST]", "d[target: ETN]"))), "")

# ================================================= (3) federated, 9 sites
cat("== federated\n")
SITE_BIN <- sub("mlnmr$", "mlnmr_site", springs:::mlnmr_engine_bin())
work <- file.path(tempdir(), "fed8"); dir.create(work, showWarnings = FALSE)
b <- springs::mlnmr(net, regression = reg, .build_only = TRUE)
P <- length(b$par_names); sites <- unique(b$unit_study)
sfile <- setNames(file.path(work, paste0(make.names(sites), ".json")), sites)
for (s in sites) jsonlite::write_json(list(link = b$link, agd_likelihood = b$likelihood, p = P,
  units = b$units[b$unit_study == s]), sfile[[s]], auto_unbox = TRUE, digits = NA)
query <- function(s, q) {
  qf <- tempfile(tmpdir = work, fileext = ".json"); of <- tempfile(tmpdir = work, fileext = ".json")
  jsonlite::write_json(q, qf, auto_unbox = TRUE, digits = NA)
  stopifnot(identical(system2(SITE_BIN, c("--data", sfile[[s]], "--query", qf, "--output", of)), 0L))
  r <- jsonlite::fromJSON(of, simplifyVector = TRUE); unlink(c(qf, of))
  if (!is.null(r$ll)) r$ll[!is.finite(r$ll)] <- -1e12   # impossible configuration (p underflow): weight 0
  r
}
comm <- setNames(numeric(length(sites)), sites)
secure_sum <- function(contrib) {          # ring with a random mask, removed by site 1 at the end
  M <- stats::rnorm(length(contrib[[1]]), sd = 1e3 * (1 + stats::median(abs(unlist(contrib)))))
  run <- contrib[[1]] + M
  for (k in seq_along(contrib)[-1]) run <- run + contrib[[k]]
  for (s in names(contrib)) comm[[s]] <<- comm[[s]] + length(contrib[[s]])
  run - M
}
all_sites <- function(f) lapply(setNames(sites, sites), f)
t0 <- Sys.time()
cv <- b$spec$terms$vars
xs <- secure_sum(all_sites(function(s) { r <- b$rows[b$rows$.study == s, ]; c(colSums(r[, cv] * r$.w), sum(r$.w)) }))
stopifnot(max(abs(xs[seq_along(cv)] / xs[length(xs)] - b$xbar)) < 1e-8)
pp <- rep(1 / 100, P); theta <- rep(0, P); F_old <- -Inf; rounds <- 0L
repeat {
  rounds <- rounds + 1L
  f <- secure_sum(all_sites(function(s) { r <- query(s, list(mode = "force", theta = theta)); c(r$ll, r$grad, as.vector(r$hess)) }))
  Fv <- f[1] - 0.5 * sum(pp * theta^2)
  if (Fv < F_old - 1e-9 * abs(F_old)) { theta <- theta_old + 0.5 * (theta - theta_old); next }
  g <- f[1 + seq_len(P)] - pp * theta; H <- matrix(f[-(1:(P + 1))], P, P) + diag(pp); H <- (H + t(H)) / 2
  ch <- tryCatch(chol(H), error = function(e) chol(H + diag(1e-6 + abs(diag(H)) * 1e-3, P)))
  step <- backsolve(ch, forwardsolve(t(ch), g)); theta_old <- theta; F_old <- Fv
  if (max(abs(step)) < 1e-9 || rounds > 80) break
  theta <- theta + step
}
names(theta) <- b$par_names; cov_lap <- chol2inv(ch)
N <- 16000L; NU <- 8
set.seed(20261115L)
Z <- matrix(stats::rnorm(N * P), N, P); chi <- stats::rchisq(N, NU)
V <- sweep((Z %*% t(t(chol(cov_lap)))) * sqrt(NU / chi), 2, theta, "+"); colnames(V) <- b$par_names
logq <- -0.5 * (NU + P) * log1p(rowSums(Z^2) * NU / chi / NU)
ll <- secure_sum(all_sites(function(s) query(s, list(mode = "energy", thetas = unname(split(V, row(V)))))$ll))
lw <- ll + rowSums(stats::dnorm(V, 0, 10, log = TRUE)) - logq
w <- exp(lw - max(lw)); w <- w / sum(w)
t_fed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
fed_tab <- springs:::.w_tab(V, w)
ms_fe <- as.data.frame(summary(res$fixed$mn$fit, pars = c("d", "beta")))[, c("parameter", "mean", "sd")]
z_lap <- zs(data.frame(parameter = b$par_names, mean = theta, sd = sqrt(diag(cov_lap))), ms_fe)
z_fed <- zs(fed_tab[, c("parameter", "mean", "sd")], ms_fe)
z_pool <- zs(fed_tab[, c("parameter", "mean", "sd")], fe$summary[, c("parameter", "mean", "sd")])
eq <- max(abs(theta - fe$mode[b$par_names]))
fed_sites <- data.frame(site = sites,
  data = ifelse(sites %in% f_ipd$studyc, "IPD", "AgD"),
  springs = sapply(sites, function(s) sum(b$unit_study == s)),
  patients = sapply(sites, function(s) sum(sapply(b$units[b$unit_study == s], `[[`, "n"))),
  `numbers sent` = comm[sites], `patient records sent` = 0L, check.names = FALSE, row.names = NULL)
write.csv(rbind(cbind(stage = "laplace (force rounds only)", z_lap), cbind(stage = "exact (after energy round)", z_fed)),
          file.path(OUT_DIR, "08_federated_vs_multinma.csv"), row.names = FALSE)
md <- c(md, "### (3) Federated: 9 sites, no pooled data, no coordinator", "",
  md_table(fed_sites), "",
  sprintf("* **%d force rounds** relax the joint network: equilibrium = pooled springs fit to **%.1e**. Quadratic equivalent springs only: vs multinma (pooled data) %s.", rounds, eq, summ(z_lap)),
  sprintf("* **One energy round** (%d shared random configurations, one log-likelihood each): vs multinma (pooled data) **%s**; vs pooled springs max |z| %.3f.", N, summ(z_fed), max(abs(z_pool$z))),
  sprintf("* Whole protocol %.1f s (sites simulated on one machine). No patient record, covariate value or per-patient quantity leaves a site.", t_fed), "")

writeLines(md, file.path(OUT_DIR, "08_full_network.md"))
cat(md, sep = "\n")
