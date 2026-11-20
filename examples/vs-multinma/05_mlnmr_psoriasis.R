#!/usr/bin/env Rscript
# Example 5 — IPD + AgD: multilevel network meta-regression (ML-NMR).
#
# The multinma plaque-psoriasis vignette (Phillippo et al. 2020, JRSS A):
#   3 IPD trials (UNCOVER-1/2/3, ~3 850 patients) + 1 AgD trial (FIXTURE),
#   PASI 75, probit link, 5 covariates (durnpso, prevsys, bsa, weight, psa),
#   effect modification common within treatment class, 64 integration points
#   (Gaussian copula, gamma / logit-normal / Bernoulli marginals).
#
# springs: every patient a Bernoulli spring, every AgD arm ONE spring whose force
# law is the average of individual force laws over its integration points; the
# equilibrium + observed-information Laplace (FE), plus the grand-canonical
# tau^2 sum (RE). multinma: Stan NUTS, 4 chains x 2000 iterations.
#
# Checks: (0) IPD-only logistic/probit FE == glm MLE;  (1) FE ML-NMR;
#         (2) RE ML-NMR;  (3) population-specific relative effects and
#         population-average predictions;  (4) tau-prior sensitivity of the RE
#         model from ONE grid vs a multinma refit.
#
# Output: results/05_mlnmr.md, results/05_*.csv

source("examples/vs-multinma/common.R")
suppressPackageStartupMessages({ library(dplyr) })

# ---- data, exactly as in the multinma vignette --------------------------------
pso_ipd <- filter(plaque_psoriasis_ipd, studyc %in% c("UNCOVER-1", "UNCOVER-2", "UNCOVER-3")) %>%
  mutate(bsa = bsa / 100, prevsys = as.numeric(prevsys), psa = as.numeric(psa),
         weight = weight / 10, durnpso = durnpso / 10,
         trtclass = case_when(trtn == 1 ~ "Placebo", trtn %in% c(2, 3, 5, 6) ~ "IL blocker",
                              trtn == 4 ~ "TNFa blocker"),
         complete = complete.cases(durnpso, prevsys, bsa, weight, psa)) %>%
  filter(complete)
pso_agd <- filter(plaque_psoriasis_agd, studyc == "FIXTURE") %>%
  mutate(bsa_mean = bsa_mean / 100, bsa_sd = bsa_sd / 100, prevsys = prevsys / 100,
         psa = psa / 100, weight_mean = weight_mean / 10, weight_sd = weight_sd / 10,
         durnpso_mean = durnpso_mean / 10, durnpso_sd = durnpso_sd / 10,
         trtclass = case_when(trtn == 1 ~ "Placebo", trtn %in% c(2, 3, 5, 6) ~ "IL blocker",
                              trtn == 4 ~ "TNFa blocker"))
pso_net <- combine_network(
  set_ipd(pso_ipd, study = studyc, trt = trtc, r = pasi75, trt_class = trtclass),
  set_agd_arm(pso_agd, study = studyc, trt = trtc, r = pasi75_r, n = pasi75_n,
              trt_class = trtclass))
pso_net <- suppressMessages(add_integration(pso_net,
  durnpso = distr(qgamma, mean = durnpso_mean, sd = durnpso_sd),
  prevsys = distr(qbern, prob = prevsys),
  bsa = distr(qlogitnorm, mean = bsa_mean, sd = bsa_sd),
  weight = distr(qgamma, mean = weight_mean, sd = weight_sd),
  psa = distr(qbern, prob = psa), n_int = 64))
reg <- ~(durnpso + prevsys + bsa + weight + psa) * .trt

md <- c("## Example 5 — IPD + AgD: ML-NMR on the plaque psoriasis network", "",
  sprintf("%d IPD patients (UNCOVER-1/2/3) + %d AgD arms (FIXTURE) with %d integration points each; probit link, `%s`, class-common interactions, two-parameter binomial AgD likelihood (`bernoulli2`). Priors as in the vignette: N(0, 10²) on intercepts, treatment effects and regression coefficients; τ ~ HN(2.5).",
          nrow(pso_ipd), nrow(pso_agd), 64, deparse(reg)), "")

# ---- (0) exactness: IPD-only fixed effects == glm --------------------------------
net_ipd <- set_ipd(pso_ipd, study = studyc, trt = trtc, r = pasi75, trt_class = trtclass)
covs <- c("durnpso", "prevsys", "bsa", "weight", "psa")
chk <- do.call(rbind, lapply(c("logit", "probit"), function(lk) {
  f <- springs::mlnmr(net_ipd, regression = reg, class_interactions = "independent", link = lk,
                      prior_intercept = Inf, prior_trt = Inf, prior_reg = Inf)
  d <- pso_ipd
  for (v in covs) d[[v]] <- d[[v]] - f$xbar[[v]]
  d$trtc <- factor(d$trtc, levels = levels(net_ipd$treatments))
  g <- glm(pasi75 ~ 0 + studyc + trtc + (durnpso + prevsys + bsa + weight + psa) +
             (durnpso + prevsys + bsa + weight + psa):trtc, family = binomial(lk), data = d,
           control = glm.control(epsilon = 1e-14, maxit = 200))
  nm <- rownames(coef(summary(g)))
  nm <- sub("^studyc(.*)", "mu[\\1]", nm); nm <- sub("^trtc([^:]*)$", "d[\\1]", nm)
  nm <- ifelse(grepl("^trtc.*:", nm), sub("^trtc(.*):(.*)$", "beta[\\2:.trt\\1]", nm), nm)
  nm <- ifelse(nm %in% covs, sprintf("beta[%s]", nm), nm)
  data.frame(link = lk, parameters = length(nm),
             max_abs_diff_vs_glm_MLE = max(abs(f$mode[nm] - coef(g))),
             loglik_springs = f$log_lik_mode, loglik_glm = as.numeric(logLik(g)),
             springs_engine_s = f$engine_sec)
}))
md <- c(md, "### (0) Exactness check: IPD-only network, flat priors, vs `glm()`", "",
        "The spring equilibrium is the MLE. (Probit SEs differ from glm's by ≈1.5% because springs uses the observed information and glm the expected; for the canonical logit link the two coincide, and they agree to 1e-8.)", "",
        md_table(chk, 8), "")

# ---- helpers ----------------------------------------------------------------------
run_mn <- function(trt_effects, likelihood = "bernoulli2", prior_het = half_normal(scale = 2.5)) {
  t <- system.time(fit <- suppressWarnings(nma(pso_net, trt_effects = trt_effects,
    link = "probit", likelihood = likelihood, regression = reg, class_interactions = "common",
    prior_intercept = normal(scale = 10), prior_trt = normal(scale = 10),
    prior_reg = normal(scale = 10), prior_het = prior_het, init_r = 0.1, QR = TRUE,
    chains = MN_CHAINS, iter = MN_ITER, seed = 1, refresh = 0, show_messages = FALSE)))
  list(fit = fit, wall = unname(t["elapsed"]))
}
run_sp <- function(trt_effects, likelihood = "bernoulli2", tau_priors = "hn:2.5")
  springs::mlnmr(pso_net, regression = reg, class_interactions = "common",
                 trt_effects = trt_effects, link = "probit", likelihood = likelihood,
                 tau_priors = tau_priors)
cmp_pars <- function(sp, mn) {
  ms <- as.data.frame(summary(mn$fit, pars = c("d", "beta", if (sp$trt_effects == "random") "tau")))
  ss <- sp$summary
  if (!is.null(sp$tau)) ss <- rbind(ss, data.frame(parameter = "tau", mean = sp$tau$mean[1],
                          sd = sp$tau$sd[1], `2.5%` = sp$tau$q025[1], `50%` = sp$tau$q50[1],
                          `97.5%` = sp$tau$q975[1], check.names = FALSE))
  m <- match(ms$parameter, ss$parameter)
  data.frame(parameter = ms$parameter, springs_mean = ss$mean[m], multinma_mean = ms$mean,
             springs_sd = ss$sd[m], multinma_sd = ms$sd, multinma_ESS = ms$Bulk_ESS,
             z = (ss$mean[m] - ms$mean) / ms$sd, sd_ratio = ss$sd[m] / ms$sd)
}

res <- list()
for (te in c("fixed", "random")) {
  cat("==", te, "\n")
  sp <- run_sp(te, tau_priors = c("hn:2.5", "hn:1", "hn:0.5", "hc:1", "exp:2", "unif:2",
                                  "ln:-2.56:1.74", "ln:-3.95:1.34"))
  mn <- run_mn(te)
  pc <- cmp_pars(sp, mn)
  write.csv(pc, file.path(OUT_DIR, sprintf("05_pars_%s.csv", te)), row.names = FALSE)

  # population-specific relative effects
  re_mn <- as.data.frame(relative_effects(mn$fit))
  re_sp <- springs::mlnmr_relative_effects(sp)
  m <- match(re_mn$parameter, re_sp$parameter)
  rel <- data.frame(parameter = re_mn$parameter, springs = re_sp$mean[m], multinma = re_mn$mean,
                    springs_sd = re_sp$sd[m], multinma_sd = re_mn$sd,
                    z = (re_sp$mean[m] - re_mn$mean) / re_mn$sd)
  write.csv(rel, file.path(OUT_DIR, sprintf("05_releff_%s.csv", te)), row.names = FALSE)

  # population-average predicted probabilities in each study population
  pr_mn <- as.data.frame(predict(mn$fit, type = "response"))
  pr_sp <- springs::mlnmr_predict(sp)
  pr_sp$parameter <- sprintf("pred[%s: %s]", pr_sp$population, pr_sp$treatment)
  m <- match(pr_mn$parameter, pr_sp$parameter)
  pred <- data.frame(parameter = pr_mn$parameter, springs = pr_sp$mean[m], multinma = pr_mn$mean,
                     springs_sd = pr_sp$sd[m], multinma_sd = pr_mn$sd,
                     z = (pr_sp$mean[m] - pr_mn$mean) / pr_mn$sd)
  write.csv(pred, file.path(OUT_DIR, sprintf("05_pred_%s.csv", te)), row.names = FALSE)

  res[[te]] <- list(sp = sp, mn = mn, pc = pc, rel = rel, pred = pred)
  md <- c(md, sprintf("### (%d) %s-effects ML-NMR", if (te == "fixed") 1 else 2, te), "",
    sprintf("* **springs: %.2f s** (engine %.2f s; %d springs, %d parameters%s). multinma: **%.0f s** (min bulk ESS %.0f%s).",
            sp$wall_sec, sp$engine_sec, sp$n_units, sp$n_par,
            if (te == "random") sprintf(", %d random effects, %d-point τ² grid, 8 τ priors", sp$n_re, nrow(sp$grid)) else "",
            mn$wall, min(pc$multinma_ESS, na.rm = TRUE),
            if (te == "random") ", single prior" else ""),
    sprintf("* Parameters (d, β%s): max |z| = %.2f, median |z| = %.2f; SD ratio %.2f–%.2f.",
            if (te == "random") ", τ" else "", max(abs(pc$z), na.rm = TRUE),
            median(abs(pc$z), na.rm = TRUE), min(pc$sd_ratio, na.rm = TRUE),
            max(pc$sd_ratio, na.rm = TRUE)),
    sprintf("* Population-specific relative effects (%d): max |z| = %.2f. Population-average predictions (%d): max |z| = %.2f.",
            nrow(rel), max(abs(rel$z)), nrow(pred), max(abs(pred$z))),
    "", md_table(pc, 3), "")
}

# ---- (3) relative effects table (FE), as in the vignette --------------------------
md <- c(md, "### (3) Population-specific relative effects (FE), vs PBO", "",
        md_table(res$fixed$rel, 3), "",
        "Population-average PASI 75 probabilities (FE):", "", md_table(res$fixed$pred, 3), "")

# ---- (4) tau-prior sensitivity of the RE model: one grid vs a multinma refit -------
sens <- springs::mlnmr_sensitivity(res$random$sp)
write.csv(sens, file.path(OUT_DIR, "05_tau_sensitivity.csv"), row.names = FALSE)
mn05 <- run_mn("random", prior_het = half_normal(scale = 0.5))
pc05 <- cmp_pars(run_sp("random", tau_priors = "hn:0.5"), mn05)
wide <- reshape(sens[sens$parameter %in% c("tau", "d[IXE_Q2W]", "d[SEC_300]", "d[ETN]"),
                     c("prior", "parameter", "mean", "log_bf_re_fe")],
                idvar = c("prior", "log_bf_re_fe"), timevar = "parameter", direction = "wide")
md <- c(md, "### (4) Heterogeneity-prior sensitivity (RE) — 8 priors from one grid", "",
  sprintf("All 8 priors come from the single RE fit above (%.2f s). One multinma RE fit costs ~%.0f s, so the panel would take ≈ %.0f min. Spot check, multinma refit under HN(0.5): max |z| = %.2f over d, β, τ (τ: springs %.3f vs multinma %.3f).",
          res$random$sp$wall_sec, res$random$mn$wall, 8 * res$random$mn$wall / 60,
          max(abs(pc05$z), na.rm = TRUE), pc05$springs_mean[pc05$parameter == "tau"],
          pc05$multinma_mean[pc05$parameter == "tau"]), "",
  md_table(wide, 3), "",
  "`log_bf_re_fe`: log Bayes factor for random vs fixed effects (importance-corrected partition functions), free from the same grid.", "")

writeLines(md, file.path(OUT_DIR, "05_mlnmr.md"))
cat(md, sep = "\n")
