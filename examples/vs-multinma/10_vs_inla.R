#!/usr/bin/env Rscript
# Example 10 — Where springs work and INLA / nmaINLA cannot: non-linear aggregation.
#
# INLA's model class: every observation depends on ONE linear predictor. A
# non-linear link is fine (binomial GLMMs), aggregated data with a linear link
# are fine (mean of η = η at the mean). The ML-NMR aggregate arm is neither: its
# likelihood depends on the population average of a non-linear inverse link,
#     p̄ = mean_q Φ(η_q),  η_q = μ + x_qᵀβ + d + x_qᵀγ  (64 integration points),
# a non-linear function of many linear predictors. That is exactly where
# aggregation bias lives, and it is outside INLA's class.
#
# Full psoriasis network (Example 8: 4 IPD + 5 AgD trials, probit, 5 covariates):
#   (A) springs ML-NMR — the averaged force law (as multinma);
#   (B) the nearest INLA-representable model — the same IPD, but each AgD arm
#       at its MEAN covariates, Φ(η̄): one linear predictor per observation;
#   (C) what nmaINLA fits — aggregate NMA on arm totals (IPD collapsed), no
#       individual covariates.
# Reference: multinma ML-NMR (Stan), cached from Example 8.
#
# Output: results/10_vs_inla.md, results/10_vs_inla.csv

source("examples/vs-multinma/common.R")
suppressPackageStartupMessages(library(dplyr))
n8 <- grep("^reg <- ", readLines("examples/vs-multinma/08_full_network.R"))
eval(parse(text = readLines("examples/vs-multinma/08_full_network.R")[16:n8]))   # net, f_ipd, f_agd, reg
target <- data.frame(durnpso = 2.5, prevsys = 1, bsa = 0.6, weight = 12, psa = 1, row.names = "target")
mn <- readRDS(file.path(EX_DIR, ".cache", "08_mn_fixed_10_2.5.rds"))   # multinma FE ML-NMR (Example 8)
stopifnot(!is.null(mn$fit))

tab_of <- function(f, newdata = NULL) {
  s <- f$summary[startsWith(f$summary$parameter, "d["), c("parameter", "mean", "sd")]
  if (is.null(newdata)) return(s)
  re <- springs::mlnmr_relative_effects(f, newdata = newdata)
  rbind(s, re[, c("parameter", "mean", "sd")])
}
mn_tab <- function(newdata = NULL) {
  s <- as.data.frame(summary(mn$fit, pars = "d"))[, c("parameter", "mean", "sd")]
  if (is.null(newdata)) return(s)
  r <- as.data.frame(relative_effects(mn$fit, newdata = newdata))
  r$parameter <- sprintf("d[target: %s]", sub(".*: (.*)\\]$", "\\1", r$parameter))
  rbind(s, r[, c("parameter", "mean", "sd")])
}
pops <- function(f) {                      # effects in each study population
  re <- springs::mlnmr_relative_effects(f); re[, c("parameter", "mean", "sd")]
}
mn_pops <- as.data.frame(relative_effects(mn$fit))[, c("parameter", "mean", "sd")]
zs <- function(a, b) { m <- merge(a, b, by = "parameter")
  data.frame(parameter = m$parameter, est = m$mean.x, ref = m$mean.y, z = (m$mean.x - m$mean.y) / m$sd.y) }

# (A) springs ML-NMR
A <- springs::mlnmr(net, regression = reg, class_interactions = "common", link = "probit",
                    likelihood = "bernoulli2")

# (B) INLA-representable: each AgD arm at its mean covariates (all integration points = their mean)
netB <- net
for (v in c("durnpso", "prevsys", "bsa", "weight", "psa")) {
  col <- paste0(".int_", v)
  netB$agd_arm[[col]] <- lapply(netB$agd_arm[[col]], function(x) rep(mean(x), length(x)))
}
B <- springs::mlnmr(netB, regression = reg, class_interactions = "common", link = "probit",
                    likelihood = "bernoulli")

# (C) nmaINLA's model: aggregate NMA on arm totals, no individual covariates
arms <- rbind(
  f_ipd %>% group_by(studyc, trtc, trtclass) %>% summarise(r = sum(pasi75), n = n(), .groups = "drop"),
  f_agd %>% transmute(studyc, trtc, trtclass, r = pasi75_r, n = pasi75_n))
netC <- set_agd_arm(as.data.frame(arms), study = studyc, trt = trtc, r = r, n = n,
                    trt_class = trtclass, trt_ref = "PBO")
C <- springs::mlnmr(netC, regression = NULL, link = "probit", likelihood = "bernoulli")

# aggregation bias at the (A) equilibrium: mean_q Φ(η_q) vs Φ(mean_q η_q), per AgD arm
bA <- springs::mlnmr(net, regression = reg, class_interactions = "common", link = "probit",
                     likelihood = "bernoulli2", .build_only = TRUE)
th <- A$mode[bA$par_names]
agg <- do.call(rbind, lapply(which(sapply(bA$units, `[[`, "q") > 1), function(i) {
  u <- bA$units[[i]]; X <- matrix(u$x, nrow = u$q, byrow = TRUE); eta <- X %*% th
  data.frame(arm = sprintf("%s: %s", bA$unit_study[i],
                           bA$rows$.trt[match(i, bA$rows$.unit)]),
             observed = u$r / u$n, ml_nmr = mean(pnorm(eta)), at_mean = pnorm(mean(eta)))
}))
agg$bias_pp <- 100 * (agg$at_mean - agg$ml_nmr)

zA <- zs(tab_of(A, target), mn_tab(target)); zB <- zs(tab_of(B, target), mn_tab(target))
zC <- zs(tab_of(C), mn_tab())
pA <- zs(pops(A), mn_pops); pB <- zs(pops(B), mn_pops)
out <- merge(merge(zA[, c("parameter", "ref", "est", "z")], zB[, c("parameter", "est", "z")],
                   by = "parameter", suffixes = c(" A", " B")),
             zC[, c("parameter", "est", "z")], by = "parameter", all.x = TRUE)
names(out) <- c("parameter", "multinma", "A: springs ML-NMR", "z A", "B: AgD at mean covariates", "z B",
                "C: arm-total NMA", "z C")
write.csv(out, file.path(OUT_DIR, "10_vs_inla.csv"), row.names = FALSE)

md <- c("## Example 10 — Where springs work and INLA / nmaINLA cannot: non-linear aggregation", "",
  "INLA requires every observation to depend on **one** linear predictor. A non-linear link alone is fine (binomial GLMMs); aggregation with a linear link is fine (the mean of η is η at the mean, Example 9). The ML-NMR aggregate arm combines both: its likelihood depends on p̄ = mean_q Φ(η_q) over 64 integration points, a non-linear function of many linear predictors. That is exactly where aggregation bias arises, and it is outside INLA's model class.", "",
  "Full psoriasis network (Example 8), fixed effects. Reference: multinma's ML-NMR (Stan).", "",
  "| model | what it is | can INLA / nmaINLA fit it? |", "|---|---|---|",
  "| **A. springs ML-NMR** | IPD patients + AgD arms as averaged non-linear springs | **no** (non-linear aggregation) |",
  "| B. AgD at mean covariates | same IPD, each AgD arm as Φ(η at its mean covariates) | INLA: yes (one η per observation) — but it ignores aggregation bias |",
  "| C. arm-total NMA | all data as arm totals, no individual covariates | nmaINLA: yes |", "",
  "### Aggregation bias, arm by arm", "",
  "At the ML-NMR equilibrium: the arm's predicted PASI 75 rate under the correct averaged force law vs the rate at the arm's mean covariates (what model B uses).", "",
  md_table(agg, 3), "",
  sprintf("Largest gap: %.1f percentage points (%s). The gaps are largest where Φ is most curved — response rates far from 50%%.",
          max(abs(agg$bias_pp)), agg$arm[which.max(abs(agg$bias_pp))]), "",
  "### Consequences for the estimates", "",
  sprintf("* **A (springs ML-NMR) vs multinma:** max |z| %.3f over treatment effects and target-population effects; %.3f over effects in the 9 study populations.",
          max(abs(zA$z)), max(abs(pA$z))),
  sprintf("* **B (INLA-representable) vs multinma:** max |z| %.2f over treatment effects and target-population effects; %.2f over the 9 study populations — the aggregation bias propagates into the estimates.",
          max(abs(zB$z)), max(abs(pB$z))),
  sprintf("* **C (nmaINLA's model):** no individual covariates, so no population-specific or target-population effects at all; its treatment effects are population-averaged over the trial mix and differ from the ML-NMR effects by up to %.1f posterior SDs (a different estimand, not only a bias).",
          max(abs(zC$z), na.rm = TRUE)), "",
  "Probit scale, mean; z = (estimate − multinma) / multinma SD:", "",
  md_table(out, 3), "",
  "**Why springs can:** an AgD arm is a single spring whose force law is the average of its patients' non-linear force laws. Its energy, force and stiffness are computed directly (with the two-parameter binomial and its exact derivatives), so the same equilibrium / curvature / importance machinery applies — and with it the prior–likelihood separation and the federated protocol.")
writeLines(md, file.path(OUT_DIR, "10_vs_inla.md"))
cat(md, sep = "\n")
