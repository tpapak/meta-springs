#!/usr/bin/env Rscript
# Example 6 — Prior investigation for IPD + AgD (ML-NMR), from ONE spring fit.
#
# Why it is cheap in the spring framework: the posterior energy is a sum of
#   data springs (3 854 patients + 64 integration points per AgD arm: expensive)
#   + prior springs (one per parameter: trivially cheap)
#   + random-effect springs of stiffness 1/tau^2 (summed over a tau^2 grid).
# The spring fit keeps its weighted importance pool {theta_i, w_i}. Changing a
# prior changes only the prior springs, so the new posterior is the same pool
# with weights w_i * pi_new(theta_i) / pi_old(theta_i); a tau prior is a
# reweighting of the tau^2 grid. Every patient is evaluated once, ever. The
# effective sample size of the new weights says when a prior has moved the
# posterior too far from the pool; then springs refits (< 1 s), still no MCMC.
#
# Panels:
#   (A) fixed effects: 11 priors on the effect modifiers (the weakly identified
#       part of ML-NMR), the treatment effects and the baselines, including
#       heavy-tailed (Cauchy, t3) and informative ones;
#   (B) random effects: 4 interaction priors x 8 tau priors = 32 posteriors;
#   (C) prior share: how hard each prior spring pulls, from the fit alone.
# Validation: every reweighted posterior vs a direct springs refit; selected
# ones vs multinma (Stan) refits; population-specific effects in a target
# population outside the trials (transportability is where the effect-modifier
# prior matters).
#
# Output: results/06_mlnmr_priors.md, results/06_*.csv

source("examples/vs-multinma/pso_data.R")
options(mc.cores = parallel::detectCores())

# target population for transport: long-standing, severe, heavy, PsA, prior systemic
target <- data.frame(durnpso = 2.5, prevsys = 1, bsa = 0.6, weight = 12, psa = 1,
                     row.names = "target")
TP_LAB <- "25 y disease, 60% BSA, 120 kg, PsA, prior systemic"
tau_set <- c("hn:2.5", "hn:1", "hn:0.5", "hc:1", "exp:2", "unif:2", "ln:-2.56:1.74", "ln:-3.95:1.34")
lab <- springs:::.mlnmr_prior_label

sp_fit <- function(trt_effects, prior_intercept = 10, prior_trt = 10, prior_reg = 10,
                   tau_priors = "hn:2.5", ...)
  springs::mlnmr(pso_net, regression = reg, class_interactions = "common", link = "probit",
                 likelihood = "bernoulli2", trt_effects = trt_effects,
                 prior_intercept = prior_intercept, prior_trt = prior_trt, prior_reg = prior_reg,
                 tau_priors = tau_priors, ...)
mn_fit <- function(trt_effects, prior_intercept = normal(scale = 10), prior_trt = normal(scale = 10),
                   prior_reg = normal(scale = 10), prior_het = half_normal(scale = 2.5)) {
  t <- system.time(fit <- suppressWarnings(nma(pso_net, trt_effects = trt_effects,
    link = "probit", likelihood = "bernoulli2", regression = reg, class_interactions = "common",
    prior_intercept = prior_intercept, prior_trt = prior_trt, prior_reg = prior_reg,
    prior_het = prior_het, init_r = 0.1, QR = TRUE,
    chains = MN_CHAINS, iter = MN_ITER, seed = 1, refresh = 0, show_messages = FALSE)))
  list(fit = fit, wall = unname(t["elapsed"]))
}
mnp <- function(x) {   # springs-side prior -> multinma prior object
  p <- springs:::.mlnmr_prior(x)
  switch(p$dist, flat = flat(), normal = normal(location = p$loc, scale = p$scale),
         t = if (p$df == 1) cauchy(location = p$loc, scale = p$scale)
             else student_t(location = p$loc, scale = p$scale, df = p$df))
}
# d, beta (+ tau) + target-population effects of a springs object, as one table
sp_tab <- function(f) {
  s <- f$summary[!startsWith(f$summary$parameter, "mu["), c("parameter", "mean", "sd")]
  if (!is.null(f$tau)) s <- rbind(s, data.frame(parameter = "tau", mean = f$tau$mean[1], sd = f$tau$sd[1]))
  re <- springs::mlnmr_relative_effects(f, newdata = target)
  rbind(s, data.frame(parameter = re$parameter, mean = re$mean, sd = re$sd))
}
mn_tab <- function(m) {
  s <- as.data.frame(summary(m$fit, pars = c("d", "beta", if (m$fit$trt_effects == "random") "tau")))
  re <- as.data.frame(relative_effects(m$fit, newdata = target))
  re$parameter <- sprintf("d[target: %s]", sub(".*: (.*)\\]$", "\\1", re$parameter))
  rbind(s[, c("parameter", "mean", "sd")], re[, c("parameter", "mean", "sd")])
}
zcmp <- function(a, b) {   # a vs reference b: max |z|, SD ratio range
  m <- merge(a, b, by = "parameter")
  c(max_abs_z = max(abs(m$mean.x - m$mean.y) / m$sd.y),
    sd_ratio_min = min(m$sd.x / m$sd.y), sd_ratio_max = max(m$sd.x / m$sd.y), n = nrow(m))
}

md <- c("## Example 6 — Prior investigation for ML-NMR from one spring fit", "",
  sprintf("The plaque psoriasis ML-NMR of Example 5 (probit, 5 covariates, class-common interactions, `bernoulli2`). Target population for transport: %s.", TP_LAB), "",
  "**Mechanism.** The energy is data springs (every patient and every integration point: the expensive part) + one prior spring per parameter + random-effect springs (1/τ²). The fit keeps its weighted importance pool; a new prior multiplies each weight by π_new(θ)/π_old(θ), and a τ prior reweights the τ² grid. No patient is evaluated again. The effective sample size (ESS) of the new weights flags priors that move the posterior too far from the pool; those are refitted (< 1 s).", "")

# ============================================================== (A) fixed effects
cat("== (A) FE panel\n")
t_base <- system.time(fe <- sp_fit("fixed"))[["elapsed"]]
panel_A <- list(
  "vignette: all N(0, 10²)"        = list(),
  "reg N(0, 2.5²)"                 = list(reg = 2.5),
  "reg N(0, 1)"                    = list(reg = 1),
  "reg N(0, 0.5²)"                 = list(reg = 0.5),
  "reg N(0, 0.25²)"                = list(reg = 0.25),
  "reg N(0, 0.1²) (≈ no EM)"       = list(reg = 0.1),
  "reg Cauchy(0, 0.5)"             = list(reg = cauchy(0, 0.5)),
  "reg t3(0, 0.5)"                 = list(reg = student_t(0, 0.5, 3)),
  "trt N(0, 1) (sceptical)"        = list(trt = 1),
  "trt N(0, 0.5²) (very sceptical)"= list(trt = 0.5),
  "intercept N(−1.5, 0.3²) (placebo ≈ 7%)" = list(intercept = normal(-1.5, 0.3)))
pan_A <- list(); val_A <- list()
t_panel_A <- system.time(for (nm in names(panel_A)) {
  pr <- panel_A[[nm]]
  rw <- springs::mlnmr_reweight(fe, prior_intercept = pr$intercept, prior_trt = pr$trt,
                                prior_reg = pr$reg)
  pan_A[[nm]] <- list(rw = rw, tab = sp_tab(rw))
})[["elapsed"]]
for (nm in names(panel_A)) {            # validation: direct springs refit of every panel member
  pr <- panel_A[[nm]]
  t <- system.time(rf <- sp_fit("fixed", prior_intercept = pr$intercept %||% 10,
                                prior_trt = pr$trt %||% 10, prior_reg = pr$reg %||% 10))[["elapsed"]]
  val_A[[nm]] <- c(zcmp(pan_A[[nm]]$tab, sp_tab(rf)), refit_s = t)
}
key <- c("d[IXE_Q2W]", "d[SEC_300]", "d[ETN]", "beta[bsa:.trtclassIL blocker]",
         "beta[psa:.trtclassIL blocker]", "d[target: IXE_Q2W]", "d[target: SEC_300]", "d[target: ETN]")
tabA <- do.call(rbind, lapply(names(panel_A), function(nm) {
  tb <- pan_A[[nm]]$tab; rw <- pan_A[[nm]]$rw
  v <- setNames(sprintf("%s (%s)", fmt(tb$mean, 2), fmt(tb$sd, 2)), tb$parameter)[key]
  data.frame(prior = nm, ESS = round(rw$reweight$ess), refit = rw$reweight$refit,
             as.list(v), `max |z| vs direct refit` = fmt(val_A[[nm]][["max_abs_z"]], 3),
             check.names = FALSE)
}))
names(tabA)[4:11] <- c("d IXE Q2W", "d SEC 300", "d ETN", "β bsa×IL", "β psa×IL",
                       "target: IXE Q2W", "target: SEC 300", "target: ETN")
write.csv(tabA, file.path(OUT_DIR, "06_panel_fe.csv"), row.names = FALSE)
write.csv(do.call(rbind, lapply(names(pan_A), function(nm) cbind(prior = nm, pan_A[[nm]]$tab))),
          file.path(OUT_DIR, "06_panel_fe_long.csv"), row.names = FALSE)

# multinma refits for a subset
mn_A_set <- c("vignette: all N(0, 10²)", "reg N(0, 0.5²)", "reg N(0, 0.1²) (≈ no EM)",
              "reg Cauchy(0, 0.5)", "trt N(0, 0.5²) (very sceptical)")
mnA <- lapply(setNames(mn_A_set, mn_A_set), function(nm) {
  cat("   multinma FE:", nm, "\n")
  pr <- panel_A[[nm]]
  m <- mn_fit("fixed", prior_intercept = mnp(pr$intercept %||% 10), prior_trt = mnp(pr$trt %||% 10),
              prior_reg = mnp(pr$reg %||% 10))
  c(zcmp(pan_A[[nm]]$tab, mn_tab(m)), multinma_s = m$wall)
})
valA <- data.frame(prior = mn_A_set, ESS = sapply(mn_A_set, function(nm) round(pan_A[[nm]]$rw$reweight$ess)),
                   refit = sapply(mn_A_set, function(nm) pan_A[[nm]]$rw$reweight$refit),
                   `max |z|` = sapply(mnA, `[[`, "max_abs_z"),
                   `SD ratio` = sprintf("%.2f–%.2f", sapply(mnA, `[[`, "sd_ratio_min"), sapply(mnA, `[[`, "sd_ratio_max")),
                   quantities = sapply(mnA, `[[`, "n"), `multinma s` = sapply(mnA, `[[`, "multinma_s"),
                   check.names = FALSE, row.names = NULL)
write.csv(valA, file.path(OUT_DIR, "06_validation_fe.csv"), row.names = FALSE)

rw_sec <- sapply(pan_A, function(x) x$rw$reweight$sec)
rw_refit <- sapply(pan_A, function(x) x$rw$reweight$refit)
md <- c(md, "### (A) Fixed effects: 11 priors from one fit", "",
  sprintf("* Base fit (vignette priors) **%.2f s**; the other 10 posteriors **%.2f s in total**, incl. target-population effects. A reweighting itself takes a median **%.0f ms** (pool of %d importance draws); the %d priors that moved the posterior too far for the pool were refitted automatically (≈ %.1f s each). multinma: one Stan fit per prior (≈ %.0f s each here, ≈ %.0f min for the panel).",
          t_base, t_panel_A, 1000 * median(rw_sec[!rw_refit]), nrow(fe$pool$values),
          sum(rw_refit), mean(rw_sec[rw_refit]),
          mean(valA$`multinma s`), length(panel_A) * mean(valA$`multinma s`) / 60),
  sprintf("* Every reweighted posterior vs a **direct springs refit** under the same prior: max |z| ≤ %.3f over %d quantities (d, β, target-population effects); this is the Monte Carlo floor of 4 000 resampled draws (the unchanged vignette prior shows the same gap). Refits needed (ESS < 1000): %d of %d.",
          max(sapply(val_A, `[[`, "max_abs_z")), val_A[[1]][["n"]], sum(tabA$refit), nrow(tabA)),
  "", "Posterior mean (SD), probit scale; `target` = population-specific effect vs placebo in the target population.", "",
  md_table(tabA), "",
  "**Against multinma** (a Stan refit per prior; d, β and the 5 target-population effects):", "",
  md_table(valA, 3), "")

# ============================================================== (B) random effects
cat("== (B) RE panel\n")
t_re <- system.time(re <- sp_fit("random", tau_priors = tau_set, is_per_point = 2000))[["elapsed"]]
panel_B <- list("reg N(0, 10²)" = list(), "reg N(0, 1)" = list(reg = 1),
                "reg N(0, 0.5²)" = list(reg = 0.5), "reg Cauchy(0, 0.5)" = list(reg = cauchy(0, 0.5)))
t_panel_B <- system.time(
  pB <- springs::mlnmr_prior_panel(re, panel_B, newdata = target,
                                   pars = c("d[IXE_Q2W]", "d[SEC_300]", "d[ETN]")))[["elapsed"]]
write.csv(pB, file.path(OUT_DIR, "06_panel_re_long.csv"), row.names = FALSE)
cell <- function(par) {
  x <- pB[pB$parameter == par, ]
  x$v <- sprintf("%s (%s)", fmt(x$mean, 2), fmt(x$sd, 2))
  w <- reshape(x[, c("prior", "tau_prior", "v")], idvar = "tau_prior", timevar = "prior", direction = "wide")
  names(w) <- sub("^v\\.", "", names(w)); w
}
tau_tab <- cell("tau"); te_tab <- cell("d[target: IXE_Q2W]")
ess_B <- aggregate(ess ~ prior, data = unique(pB[, c("prior", "tau_prior", "ess")]), FUN = min)

# validation cell: reg N(0, 0.5²) x tau HN(0.5): springs refit + multinma
cat("   RE validation refits\n")
rwB <- springs::mlnmr_reweight(re, prior_reg = 0.5, tau_prior = "hn:0.5")
rfB <- sp_fit("random", prior_reg = 0.5, tau_priors = "hn:0.5")
mnB <- mn_fit("random", prior_reg = normal(scale = 0.5), prior_het = half_normal(scale = 0.5))
zB_sp <- zcmp(sp_tab(rwB), sp_tab(rfB)); zB_mn <- zcmp(sp_tab(rwB), mn_tab(mnB))
tau_mn <- as.data.frame(summary(mnB$fit, pars = "tau"))

md <- c(md, "### (B) Random effects: 4 interaction priors × 8 τ priors = 32 posteriors", "",
  sprintf("* One RE fit (40-point τ² grid, 2 000 importance draws per point) **%.2f s**; all 32 posteriors (incl. target-population effects) **%.2f s**. multinma: 32 Stan fits × ≈ %.0f s ≈ **%.0f min**.",
          t_re, t_panel_B, mnB$wall, 32 * mnB$wall / 60),
  sprintf("* Minimum ESS over the τ priors, per interaction prior: %s.",
          paste(sprintf("%s %0.f", ess_B$prior, ess_B$ess), collapse = "; ")),
  sprintf("* Check cell (reg N(0, 0.5²), τ ~ HN(0.5)): reweighted vs direct springs refit max |z| = %.3f; vs **multinma** max |z| = %.3f, SD ratio %.2f–%.2f over %d quantities (τ: springs %.3f vs multinma %.3f; multinma %.0f s).",
          zB_sp[["max_abs_z"]], zB_mn[["max_abs_z"]], zB_mn[["sd_ratio_min"]], zB_mn[["sd_ratio_max"]],
          zB_mn[["n"]], rwB$tau$mean, tau_mn$mean, mnB$wall), "",
  "**τ** (posterior mean (SD)) — rows: τ prior; columns: interaction prior:", "", md_table(tau_tab), "",
  "**IXE Q2W vs placebo in the target population** (probit; mean (SD)):", "", md_table(te_tab), "")

# ============================================================== (C) prior share
sh <- springs::mlnmr_prior_share(fe)
sh05 <- springs::mlnmr_prior_share(pan_A[["reg N(0, 0.5²)"]]$rw)
m05 <- match(sh$parameter, sh05$parameter)
shc <- data.frame(parameter = sh$parameter, `SD, all N(0, 10²)` = sh$posterior_sd,
                  `prior share, all N(0, 10²)` = sh$prior_share,
                  `SD, reg N(0, 0.5²)` = sh05$posterior_sd[m05],
                  `prior share, reg N(0, 0.5²)` = sh05$prior_share[m05],
                  check.names = FALSE)
shc <- shc[startsWith(shc$parameter, "beta[") | startsWith(shc$parameter, "d["), ]
write.csv(shc, file.path(OUT_DIR, "06_prior_share.csv"), row.names = FALSE)
md <- c(md, "### (C) Which priors can matter: prior share of the spring stiffness", "",
  "Prior share = stiffness of the prior spring / total posterior stiffness ≈ Var(θ | data, prior) / prior variance (for a t prior, its expected curvature replaces 1/variance). It needs no refit: ≈ 0 means the data alone hold the parameter; large values mean the prior holds it. The vignette's N(0, 10²) priors hold nothing (≤ 0.3%); a sceptical N(0, 0.5²) prior on the effect modifiers holds the BSA interactions by about a third.", "",
  md_table(shc, 4), "")

writeLines(md, file.path(OUT_DIR, "06_mlnmr_priors.md"))
cat(md, sep = "\n")
