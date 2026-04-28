# Fit the synthetic IPD meta-analysis with multinma (Phillippo).
# Same data as test/Test/MixedModel/IPDSimpleTest.hs.
#
# Run:  Rscript test/ipd_multinma_compare.R
#
# Requires:  install.packages("multinma")
# (multinma pulls in rstan; first run may take a while to compile Stan model.)

suppressPackageStartupMessages({
  library(multinma)
  library(dplyr)
})

# --- Synthetic IPD: 3 trials × 4 patients, y ~ age + trt + (1|study) ---
ipd <- data.frame(
  study = factor(rep(c("S1","S2","S3"), each = 4)),
  age   = c(40, 50, 40, 50,  30, 50, 30, 50,  45, 55, 45, 55),
  trt   = c( 0,  0,  1,  1,   0,  0,  1,  1,   0,  0,  1,  1),
  y     = c(10.3, 11.8, 16.1, 17.8,
             3.3,  6.8,  9.1, 12.8,
            11.3, 12.8, 17.1, 18.8)
)
ipd$trt_name <- factor(ifelse(ipd$trt == 0, "Ctrl", "Active"),
                        levels = c("Ctrl", "Active"))

cat("=== IPD data (12 rows, 3 studies) ===\n")
print(ipd)

# --- Set up the multinma network ---
net <- set_ipd(ipd,
               study   = study,
               trt     = trt_name,
               y       = y,
               trt_ref = "Ctrl")

cat("\n=== Network summary ===\n")
print(net)

# --- Fit: continuous outcome, random treatment effects, age as regression ---
fit <- nma(net,
           trt_effects = "random",
           regression  = ~ age,
           likelihood  = "normal",
           prior_intercept = normal(scale = 100),
           prior_trt       = normal(scale = 100),
           prior_reg       = normal(scale = 10),
           prior_het       = half_normal(scale = 5),
           chains = 4, iter = 2000, seed = 42)

cat("\n=== multinma fit ===\n")
print(fit)

cat("\n=== Parameters of interest (posterior mean ± sd) ===\n")
post <- as.array(fit)
params <- c("mu[S1]", "mu[S2]", "mu[S3]",     # study baselines = β_Int + u_study
            "d[Active]",                       # treatment effect = β_trt
            "beta[age]",                       # covariate slope
            "tau")                             # between-study sd
for (nm in params) {
  if (nm %in% dimnames(post)$parameters) {
    v <- as.vector(post[, , nm])
    cat(sprintf("  %-12s  %8.4f  ±  %.4f\n", nm, mean(v), sd(v)))
  }
}

cat("\n=== Comparison with spring-EM (Test.MixedModel.IPDSimpleTest) ===\n")
cat("                      truth   spring-EM     multinma (post. mean)\n")
cat(sprintf("  β_Int           %5.2f   %9.4f     (mu baseline)\n", 2.00, 1.58))
cat(sprintf("  β_age           %5.3f   %9.4f     (see beta[age])\n", 0.20, 0.1734))
cat(sprintf("  β_trt           %5.2f   %9.4f     (see d[Active])\n", 6.00, 5.90))
cat(sprintf("  τ               %5.2f   %9.4f     (see tau)\n",
            sqrt(8.333), sqrt(9.014)))

cat("\nNote: multinma is Bayesian (posterior means/CIs),\n")
cat("      spring-EM is REML-style point estimates.\n")
cat("      For n=12 rows across 3 studies, posterior uncertainty is large.\n")

# --- Direct apples-to-apples: lme4::lmer REML on the same LMM ---
# Model: y ~ age + trt + (1|study)  --- fixed β, random study intercepts.
# This is the exact model spring-EM fits.
cat("\n=== Direct REML comparison: lme4::lmer ===\n")
if ("lme4" %in% installed.packages()[, "Package"]) {
  suppressPackageStartupMessages(library(lme4))
  fit_lme4 <- lmer(y ~ age + trt + (1 | study), data = ipd, REML = TRUE)
  vc  <- as.data.frame(VarCorr(fit_lme4))
  tau2_lme4  <- vc$vcov[vc$grp == "study"]
  s2e_lme4   <- vc$vcov[vc$grp == "Residual"]
  fe_lme4    <- fixef(fit_lme4)
  u_lme4     <- ranef(fit_lme4)$study[[1]]
  cat(sprintf("  β_Int   = %9.4f\n", fe_lme4[["(Intercept)"]]))
  cat(sprintf("  β_age   = %9.4f\n", fe_lme4[["age"]]))
  cat(sprintf("  β_trt   = %9.4f\n", fe_lme4[["trt"]]))
  cat(sprintf("  τ²      = %9.4f\n", tau2_lme4))
  cat(sprintf("  σ²_ε    = %9.4f\n", s2e_lme4))
  cat(sprintf("  BLUPs   = (%.3f, %.3f, %.3f)\n", u_lme4[1], u_lme4[2], u_lme4[3]))

  cat("\n=== Comparison table (truth / spring-EM / lme4 REML / multinma posterior) ===\n")
  cat(sprintf("              truth   spring-EM    lme4 REML    multinma (post. mean)\n"))
  cat(sprintf("  β_Int    %8.4f   %9.4f   %9.4f   %9s\n", 2.00, 1.5807, fe_lme4[["(Intercept)"]],
              sprintf("%.2f (see mu)", mean(as.vector(post[,,"mu[S1]"])) - 45 * mean(as.vector(post[,,"beta[age]"])))))
  cat(sprintf("  β_age    %8.4f   %9.4f   %9.4f   %9.4f\n", 0.20, 0.1734, fe_lme4[["age"]],
              mean(as.vector(post[,,"beta[age]"]))))
  cat(sprintf("  β_trt    %8.4f   %9.4f   %9.4f   %9.4f\n", 6.00, 5.90, fe_lme4[["trt"]],
              mean(as.vector(post[,,"d[Active]"]))))
  cat(sprintf("  τ²       %8.4f   %9.4f   %9.4f       (n/a — different variance)\n",
              25/3, 9.0138, tau2_lme4))
  cat(sprintf("  σ²_ε     %8.4f   %9.4f   %9.4f       (n/a)\n",
              0.0, 0.0119, s2e_lme4))
} else {
  cat("  (lme4 not installed — skip)\n")
}
