#!/usr/bin/env Rscript
# Example 4 — MODELS multinma does not offer.
#
# (a) Bayes factors / posterior model probabilities for random vs fixed
#     effects, for every prior, straight from the partition function.
#     multinma (Stan) provides DIC but no marginal likelihood; one would
#     need bridge sampling on a refit per model and prior.
#     Validation: on a Gaussian pairwise MA the spring partition function is
#     exact, so the BF must equal bayesmeta's numerically integrated one.
#     Data: Crins et al. 2014 (bayesmeta), acute rejection, log OR.
#
# (b) Baseline-risk network meta-regression (Achana et al. 2013; bnma) —
#     a regression on the LATENT control-arm risk. multinma regresses on
#     observed covariates only. Data: certolizumab network (bnma example).
#     springs integrates tau^2 (and sigma_B^2 for exchangeable slopes).
#
# Output: results/04_models.md, results/04_bf_validation.csv

source("examples/vs-multinma/common.R")
source("examples/vs-multinma/panel.R")
suppressPackageStartupMessages({ library(bayesmeta); library(metafor) })

md <- c("## Example 4 — models beyond multinma", "")

# ---- (a) Bayes factors RE vs FE: validation against bayesmeta ------------------
data(CrinsEtAl2014, package = "bayesmeta")
es <- escalc(measure = "OR", ai = exp.AR.events, n1i = exp.total,
             ci = cont.AR.events, n2i = cont.total, data = CrinsEtAl2014)
y <- as.numeric(es$yi); s <- sqrt(as.numeric(es$vi)); k <- length(y)

# contrast-level Gaussian data as two-arm springs studies: control arm known
# exactly (se 1e-4), treatment arm (y_i, s_i) -> contrast (y_i, s_i^2 + 1e-8)
ab <- data.frame(study = rep(seq_len(k), each = 2), treatment = rep(c("ctrl", "trt"), k),
                 mean = as.vector(rbind(0, y)), std.err = as.vector(rbind(1e-4, s)))
bf_priors <- list(
  "hn:0.5"        = function(t) dhalfnormal(t, scale = 0.5),
  "hn:1"          = function(t) dhalfnormal(t, scale = 1),
  "hc:0.5"        = function(t) dhalfcauchy(t, scale = 0.5),
  "exp:2"         = function(t) dexp(t, rate = 2),
  "unif:2"        = function(t) dunif(t, 0, 2),
  # tau^2 ~ LN(mu, sigma^2)  <=>  tau ~ LN(mu/2, (sigma/2)^2)
  "ln:-2.56:1.74" = function(t) dlnorm(t, -2.56 / 2, 1.74 / 2))
sens <- springs::mtc.sensitivity(springs::mtc.model(springs::mtc.network(ab)),
                                 priors = names(bf_priors))
t_bm <- 0
val <- do.call(rbind, lapply(names(bf_priors), function(p) {
  tt <- system.time(b <- bayesmeta(y = y, sigma = s, mu.prior.mean = 0, mu.prior.sd = 100,
                                   tau.prior = bf_priors[[p]]))["elapsed"]
  t_bm <<- t_bm + tt
  data.frame(prior = label[[p]] %||% p,
             springs_logBF = sens$log_bf_re_fe[sens$prior == p][1],
             bayesmeta_logBF = -log(b$bayesfactor["actual", "tau=0"]),
             springs_tau_med = sqrt(sens$tau2_median[sens$prior == p][1]),
             bayesmeta_tau_med = b$summary["median", "tau"])
}))
val$diff <- val$springs_logBF - val$bayesmeta_logBF
write.csv(val, file.path(OUT_DIR, "04_bf_validation.csv"), row.names = FALSE)
md <- c(md, "### (a) Random vs fixed effects: Bayes factors from the partition function", "",
  sprintf("Validation on Crins et al. 2014 (k = %d, log OR, Gaussian likelihood — the spring partition function is exact here). bayesmeta integrates numerically, one call per prior (%.2f s total); springs gives all %d priors in one pass (%.3f s).",
          k, t_bm, length(bf_priors), attr(sens, "fit_wall_sec")), "",
  md_table(val, 3), "")

# applied: posterior P(RE) on the multinma example networks (equal prior odds)
nets <- list(
  smoking = list(d = multinma::smoking, study = "studyn", trt = "trtc", r = "r", n = "n"),
  thrombolytics = list(d = multinma::thrombolytics, study = "studyn", trt = "trtc", r = "r", n = "n"),
  atrial_fibrillation = list(d = multinma::atrial_fibrillation, study = "studyc", trt = "trtc",
                             r = "r", n = "n"))
ap <- c("hn:0.5", "hn:1", "hc:0.5", "ln:-2.56:1.74", "ln:-4.18:1.41")
pre <- do.call(rbind, lapply(names(nets), function(nm) {
  s <- springs::mtc.sensitivity(springs::mtc.model(springs::mtc.network(sp_ab(nets[[nm]]))),
                                priors = ap)
  s <- s[!duplicated(s$prior), ]
  data.frame(network = nm, prior = vapply(s$prior, function(p) label[[p]] %||% p, ""),
             log_BF_RE_FE = s$log_bf_re_fe, P_RE = plogis(s$log_bf_re_fe),
             engine_s = attr(s, "fit_wall_sec"))
}))
md <- c(md, "Applied to the multinma example networks (binary: Laplace-per-τ² partition function), equal prior model odds:", "",
        md_table(pre, 3), "",
        "multinma would report DIC for an FE and an RE fit (two Stan runs per prior) — not a posterior model probability.", "")

# ---- (b) baseline-risk network meta-regression ----------------------------------
cz <- fromJSON(file.path(REPO, "test", "nmr_certolizumab_arms.json"))
arm <- data.frame(Study = cz$study, Treat = cz$treatment, Outcomes = cz$events, N = cz$n)
trts <- c("Placebo", setdiff(sort(unique(arm$Treat)), "Placebo"))
br <- list()
for (cfg in list(c("common", "random"), c("independent", "random"), c("exchangeable", "random"))) {
  tt <- system.time(f <- springs::brisk_run(arm, trts, response = "binomial", type = cfg[2],
                                            regressor = cfg[1], method = "gc"))["elapsed"]
  q <- summary(f)$summary.samples$quantiles
  slope_rows <- grep("^b_bl\\[", rownames(q))[-1]          # drop reference (0)
  br[[paste(cfg, collapse = "/")]] <- data.frame(
    slopes = cfg[1], effects = cfg[2], time_s = tt,
    tau2 = f$tau2 %||% NA, sigma_B2 = f$sigma_b2 %||% NA,
    slope_median_range = sprintf("%.2f to %.2f", min(q[slope_rows, "50%"]), max(q[slope_rows, "50%"])),
    CZP_slope = sprintf("%.2f (%.2f, %.2f)", q[slope_rows[which(trts[-1] == "CZP")], "50%"],
                        q[slope_rows[which(trts[-1] == "CZP")], "2.5%"],
                        q[slope_rows[which(trts[-1] == "CZP")], "97.5%"]))
}
br <- do.call(rbind, br)
md <- c(md, "### (b) Baseline-risk network meta-regression (not available in multinma)", "",
  sprintf("Certolizumab network (%d studies, %d treatments; ACR50, log OR). Treatment effects regress on the latent baseline log-odds η_i: logit p_ik = η_i + d_k + b_k(η_i − η̄). springs integrates τ² (and σ_B² for exchangeable slopes) over the grand-canonical grid; previously validated against bnma and the exact MLE (test/NMR.md).",
          length(unique(arm$Study)), length(trts)), "",
  md_table(br, 3), "")

writeLines(md, file.path(OUT_DIR, "04_models.md"))
cat(md, sep = "\n")
