# Binary IPD meta-analysis: compare glmer (logistic GLMM) vs multinma (Phillippo).
#
# Run:  Rscript test/ipd_binary_compare.R
# Requires:  lme4, multinma  (install.packages(c("lme4","multinma")))

suppressPackageStartupMessages({
  library(lme4)
  library(multinma)
  library(dplyr)
})

# --- Synthetic binary IPD: 3 trials × 6 patients, logistic outcome.
# True model:  logit(p) = 0 + 0.5·(age-50)/10 + 1.0·trt + u_study
# True u     = (0, -0.8, +0.8)  → τ²_intercept ≈ 0.43
# Outcomes hand-assigned consistent with these p's.
ipd <- data.frame(
  study    = factor(rep(c("S1","S2","S3"), each = 6)),
  age      = c(40,50,60,40,50,60,  45,55,35,45,55,35,  50,60,40,50,60,40),
  trt      = c( 0, 0, 0, 1, 1, 1,   0, 0, 0, 1, 1, 1,   0, 0, 0, 1, 1, 1),
  y        = c( 0, 1, 1, 0, 1, 1,   0, 0, 0, 1, 1, 0,   1, 1, 0, 1, 1, 1)
)
ipd$trt_name <- factor(ifelse(ipd$trt == 0, "Ctrl", "Active"),
                       levels = c("Ctrl","Active"))
ipd$ageC <- (ipd$age - 50) / 10   # centred & scaled per 10 years

cat("=== IPD binary data (18 rows, 3 studies) ===\n")
print(ipd)
cat("\n  successes by arm:\n")
print(aggregate(y ~ study + trt_name, data = ipd, FUN = sum))

# ------------------------------------------------------------------
# (1) lme4::glmer  —  logistic GLMM with random intercept per study
# ------------------------------------------------------------------
cat("\n=== (1) lme4::glmer — logit(p) ~ ageC + trt + (1|study) ===\n")
fit_glmer <- glmer(y ~ ageC + trt + (1 | study),
                   data = ipd, family = binomial,
                   control = glmerControl(optimizer = "bobyqa"))
print(summary(fit_glmer)$coefficients)
vc_g  <- as.data.frame(VarCorr(fit_glmer))
tau2_g <- vc_g$vcov[vc_g$grp == "study"]
u_g    <- ranef(fit_glmer)$study[[1]]
cat(sprintf("\n  τ²_intercept  = %.4f   (truth ≈ 0.43)\n", tau2_g))
cat(sprintf("  BLUPs         = (%.3f, %.3f, %.3f)   (truth: (0, -0.8, +0.8))\n",
            u_g[1], u_g[2], u_g[3]))

# ------------------------------------------------------------------
# (2) multinma  —  Bayesian NMA on the same IPD
# ------------------------------------------------------------------
cat("\n=== (2) multinma — IPD binomial NMA, random treatment effects ===\n")
net <- set_ipd(ipd,
               study   = study,
               trt     = trt_name,
               r       = y,
               trt_ref = "Ctrl")

fit_mnm <- nma(net,
               trt_effects = "random",
               regression  = ~ ageC,
               likelihood  = "bernoulli",
               link        = "logit",
               prior_intercept = normal(scale = 5),
               prior_trt       = normal(scale = 5),
               prior_reg       = normal(scale = 2),
               prior_het       = half_normal(scale = 2.5),
               chains = 4, iter = 2000, seed = 42,
               refresh = 0)

post <- as.array(fit_mnm)
cat("\nPosterior (mean ± sd):\n")
for (nm in c("mu[S1]","mu[S2]","mu[S3]",
             "d[Active]","beta[ageC]","tau")) {
  if (nm %in% dimnames(post)$parameters) {
    v <- as.vector(post[,,nm])
    cat(sprintf("  %-12s  %8.4f  ±  %.4f\n", nm, mean(v), sd(v)))
  }
}

# ------------------------------------------------------------------
# Side-by-side
# ------------------------------------------------------------------
cat("\n=== Side-by-side (apples to apples) ===\n")
cat("                truth     glmer        multinma (post. mean ± sd)\n")
cat(sprintf("  β_age_scaled  0.50    %7.4f      %.4f ± %.4f\n",
            fixef(fit_glmer)[["ageC"]],
            mean(as.vector(post[,,"beta[ageC]"])),
            sd(as.vector(post[,,"beta[ageC]"]))))
cat(sprintf("  β_trt         1.00    %7.4f      %.4f ± %.4f\n",
            fixef(fit_glmer)[["trt"]],
            mean(as.vector(post[,,"d[Active]"])),
            sd(as.vector(post[,,"d[Active]"]))))
cat(sprintf("  α (intercept) 0.00    %7.4f      (embedded in mu[S])\n",
            fixef(fit_glmer)[["(Intercept)"]]))
cat(sprintf("\n  Between-study heterogeneity:\n"))
cat(sprintf("    glmer  τ²_intercept = %.4f   (variance of study baselines)\n", tau2_g))
cat(sprintf("    multinma  tau      = %.4f   (SD of random treatment effects — different quantity)\n",
            mean(as.vector(post[,,"tau"]))))
cat("\nNotes:\n")
cat("  • glmer puts random effects on the INTERCEPT per study.\n")
cat("  • multinma (with trt_effects='random') puts them on the TREATMENT EFFECT.\n")
cat("  • The fixed effects (age slope, average trt effect) should agree\n")
cat("    closely despite the different random-effect placement.\n")
