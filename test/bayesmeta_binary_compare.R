#!/usr/bin/env Rscript
# Compare springGrandCanonicalBin against bayesmeta on the pairwise binary dataset.
#
# Spring model: exact binomial likelihood, arm-space iid REs (tau2_arm), reports
# tau2_contrast = 2 * tau2_arm.
# bayesmeta:    normal-normal pairwise on (logOR, SE) with 0.5 continuity correction
#               for zeros; tau here is SD of contrast RE -> matches tau_contrast.
#
# Prior: flat in tau2 to match the spring grand-canonical construction
# (sum over chain lengths N gives uniform density in tau2 on [0, nMax/kTau]).
# In tau-space, flat-in-tau2 -> density 2*tau.

suppressPackageStartupMessages({
  library(jsonlite)
  library(bayesmeta)
})

d <- fromJSON("test/binary.json")

# Woolf log-OR with 0.5 continuity correction for zeros
cc <- 0.5
eA <- d$eventsA; nA <- d$nA
eB <- d$eventsB; nB <- d$nB
hasZero <- (eA == 0 | eB == 0 | eA == nA | eB == nB)
eA_c <- ifelse(hasZero, eA + cc, eA)
eB_c <- ifelse(hasZero, eB + cc, eB)
nA_c <- ifelse(hasZero, nA + 2*cc, nA)
nB_c <- ifelse(hasZero, nB + 2*cc, nB)

# Convention: treatment A is reference -> logOR of B vs A
pA <- eA_c / nA_c; pB <- eB_c / nB_c
logOR <- log((pB/(1-pB)) / (pA/(1-pA)))
se <- sqrt(1/eA_c + 1/(nA_c - eA_c) + 1/eB_c + 1/(nB_c - eB_c))

cat("=== test/binary.json (A vs B) ===\n")
cat(sprintf("%-12s %6s/%-6s %6s/%-6s   logOR=%+.3f se=%.3f\n",
            d$study, eA, nA, eB, nB, logOR, se))

# bayesmeta with flat-in-tau2 prior
fit <- bayesmeta(y = logOR, sigma = se,
                 labels = d$study,
                 mu.prior.mean = 0, mu.prior.sd = 100,
                 tau.prior = function(t) 2 * t)

tau2_mode   <- fit$summary["mode", "tau"]^2
tau2_median <- fit$summary["median", "tau"]^2
tau2_mean   <- fit$summary["mean", "tau"]^2
# bayesmeta summary has (tau) marginal; compute tau2 CI from tau quantiles
tau_q025 <- fit$summary["95% lower", "tau"]
tau_q975 <- fit$summary["95% upper", "tau"]
tau2_lo <- tau_q025^2
tau2_hi <- tau_q975^2

mu_mean <- fit$summary["mean", "mu"]
mu_lo   <- fit$summary["95% lower", "mu"]
mu_hi   <- fit$summary["95% upper", "mu"]

cat("\n--- bayesmeta (flat-in-tau2 prior, normal-normal on logOR) ---\n")
cat(sprintf("  tau2 mode   : %.5f\n", tau2_mode))
cat(sprintf("  tau2 median : %.5f\n", tau2_median))
cat(sprintf("  tau2 mean   : %.5f\n", tau2_mean))
cat(sprintf("  tau2 95%% CI : (%.5f, %.5f)\n", tau2_lo, tau2_hi))
cat(sprintf("  mu  mean    : %+.4f  95%% CI (%+.4f, %+.4f)\n", mu_mean, mu_lo, mu_hi))

# Write for downstream comparison
out <- list(
  dataset = "test/binary.json",
  prior = "flat-in-tau2 (density 2*tau in tau-space)",
  model = "bayesmeta normal-normal on logOR (0.5 cc for zeros)",
  tau2 = list(mode = tau2_mode, median = tau2_median, mean = tau2_mean,
              ci95 = c(tau2_lo, tau2_hi)),
  mu   = list(mean = mu_mean, ci95 = c(mu_lo, mu_hi))
)
write_json(out, "test/bayesmeta_binary_result.json", auto_unbox = TRUE, pretty = TRUE)
cat("\nSaved: test/bayesmeta_binary_result.json\n")
