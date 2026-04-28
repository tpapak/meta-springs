# Verify the tier-3 spring-EM against lme4 on the SAME hardcoded data
# that the Haskell test uses (numbers copied from SleepstudyTest.hs).
#
# Run:  Rscript test/sleepstudy_re_compare.R

suppressPackageStartupMessages({
  library(lme4)
  library(dplyr)
})

# --- Same 180 numbers as test/Test/MixedModel/SleepstudyTest.hs ---
rt <- rbind(
  c(308, 249.5600, 258.7047, 250.8006, 321.4398, 356.8519, 414.6901, 382.2038, 290.1486, 430.5853, 466.3535),
  c(309, 222.7339, 205.2658, 202.9778, 204.7070, 207.7161, 215.9618, 213.6303, 217.7272, 224.2957, 237.3142),
  c(310, 199.0539, 194.3322, 234.3200, 232.8416, 229.3074, 220.4579, 235.4208, 255.7511, 261.0125, 247.5153),
  c(330, 321.5426, 300.4002, 283.8565, 285.1330, 285.7973, 297.5855, 280.2396, 318.2613, 305.3495, 354.0487),
  c(331, 287.6079, 285.0000, 301.8206, 320.1153, 316.2773, 293.3187, 290.0750, 334.8177, 293.7469, 371.5811),
  c(332, 234.8606, 242.8118, 272.9613, 309.7688, 317.4629, 309.9976, 454.1619, 346.8311, 330.3003, 253.8644),
  c(333, 283.8424, 289.5550, 276.7693, 299.8097, 297.1710, 338.1665, 332.0265, 348.8399, 333.3600, 362.0428),
  c(334, 265.4731, 276.2012, 243.3647, 254.6723, 279.0244, 284.1912, 305.5248, 331.5229, 335.7469, 377.2990),
  c(335, 241.6083, 273.9472, 254.4907, 270.8021, 251.4519, 254.6362, 245.4523, 235.3110, 235.7541, 237.2466),
  c(337, 312.3666, 313.8058, 291.6112, 346.1222, 365.7324, 391.8385, 404.2601, 416.6923, 455.8643, 458.9167),
  c(349, 236.1032, 230.3167, 238.9256, 254.9220, 265.1922, 283.8468, 282.3117, 290.8409, 299.6159, 297.5103),
  c(350, 256.2968, 243.4543, 256.2046, 255.5271, 268.9165, 329.7247, 379.4445, 362.9184, 402.9337, 445.7838),
  c(351, 250.5265, 300.0576, 269.8939, 280.5891, 271.8274, 304.6336, 287.7466, 266.5955, 321.5418, 347.5655),
  c(352, 221.6771, 298.1939, 326.8785, 346.8555, 348.7402, 352.8287, 354.4266, 360.4326, 375.6406, 388.5417),
  c(369, 271.9235, 268.4369, 257.2424, 277.6566, 314.8222, 317.2135, 298.1353, 348.1229, 340.2800, 366.5131),
  c(370, 225.2640, 234.5235, 238.9008, 240.4730, 267.5373, 344.1937, 281.1481, 347.5855, 365.1630, 372.2288),
  c(371, 269.8801, 272.4428, 277.8989, 281.7895, 279.1705, 284.5120, 259.2658, 304.6306, 350.7807, 369.4692),
  c(372, 269.4117, 273.4740, 297.5968, 310.6316, 287.1726, 329.6076, 334.4818, 343.2199, 369.1417, 364.1236)
)
subject <- rt[, 1]
reaction <- as.vector(t(rt[, -1]))
days     <- rep(0:9, times = nrow(rt))
subj     <- factor(rep(subject, each = 10))
ss <- data.frame(Reaction = reaction, Days = days, Subject = subj)

cat("=== Data loaded (SAME 180 numbers as Haskell test) ===\n")
cat(sprintf("  n = %d, subjects = %d\n\n", nrow(ss), nlevels(ss$Subject)))

# --- Tier-3 fit: random intercept + random slope, full covariance ---
cat("=== lme4::lmer REML — Reaction ~ Days + (Days|Subject) ===\n")
fit <- lmer(Reaction ~ Days + (Days | Subject), data = ss, REML = TRUE,
            control = lmerControl(optimizer = "bobyqa",
                                  check.conv.grad = .makeCC("warning", tol = 1e-4)))
print(summary(fit)$coefficients)

vc  <- as.data.frame(VarCorr(fit))
var_int   <- vc$vcov[vc$grp == "Subject" & vc$var1 == "(Intercept)" & is.na(vc$var2)]
var_days  <- vc$vcov[vc$grp == "Subject" & vc$var1 == "Days"        & is.na(vc$var2)]
cov_id    <- vc$vcov[vc$grp == "Subject" & !is.na(vc$var2)]
rho_id    <- vc$sdcor[vc$grp == "Subject" & !is.na(vc$var2)]
s2e       <- vc$vcov[vc$grp == "Residual"]
fe        <- fixef(fit)

cat("\nVariance components:\n")
cat(sprintf("  Σ[int,int]   = %.3f\n", var_int))
cat(sprintf("  Σ[dys,dys]   = %.3f\n", var_days))
cat(sprintf("  cov          = %.3f\n", cov_id))
cat(sprintf("  ρ            = %.4f\n", rho_id))
cat(sprintf("  σ²_residual  = %.3f\n", s2e))

# --- Side-by-side with spring-EM ---
cat("\n=== Side-by-side: lme4 vs spring-EM (same data) ===\n")
cat("                       lme4 REML       spring-EM     Δ\n")
sp <- list(bInt = 251.7215, bDays = 10.3802,
           v11 = 594.98, v22 = 39.172, cov = 6.621, rho = 0.043,
           s2e = 651.552)
fmt <- function(label, lmer_val, spring_val) {
  cat(sprintf("  %-14s  %10.4f   %10.4f   %+.4f\n",
              label, lmer_val, spring_val, spring_val - lmer_val))
}
fmt("β_Int",        fe[["(Intercept)"]], sp$bInt)
fmt("β_Days",       fe[["Days"]],        sp$bDays)
fmt("Σ[int,int]",   var_int,             sp$v11)
fmt("Σ[days,days]", var_days,            sp$v22)
fmt("cov",          cov_id,              sp$cov)
fmt("ρ",            rho_id,              sp$rho)
fmt("σ²_residual",  s2e,                 sp$s2e)

cat("\nAll columns fit on the SAME data ⇒ any remaining Δ is algorithmic.\n")
cat("(Gap from lme4's published sleepstudy REML (Σ[int,int]=611.9) is\n")
cat(" absorbed in the hardcoded-data transcription error, not the solver.)\n")
