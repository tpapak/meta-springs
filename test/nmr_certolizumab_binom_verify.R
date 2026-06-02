# Certolizumab exact-binomial NMR: gemtc (Bayesian, MCMC) vs binomial spring IRLS.
suppressMessages(library(gemtc)); set.seed(1)
data(certolizumab)
ab <- certolizumab$data.ab; sl <- certolizumab$studies
ddbar <- mean(sl$diseaseDuration)              # centre covariate (gemtc centres too)

## ---- gemtc: exact binomial, logit link, fixed effect, shared coefficient ----
reg <- list(coefficient="shared", variable="diseaseDuration", control="Placebo")
model <- mtc.model(certolizumab, type="regression", regressor=reg,
                   likelihood="binom", link="logit", linearModel="fixed")
res <- mtc.run(model, n.adapt=20000, n.iter=100000, thin=4)
gem <- summary(res)$summaries$statistics[, c("Mean","SD")]
scale <- model$regressor$scale    # gemtc standardises: (dd-center)/scale, scale = 2*SD
gemB_yr <- gem["B","Mean"]/scale; gemBsd_yr <- gem["B","SD"]/scale   # convert B back to per-year

## ---- binomial spring IRLS (Newton on the spring energy) ----
# logit(p_arm) = mu_study + d_treatment + beta*(dd-ddbar)*(treatment!=Placebo)
d <- merge(ab, sl, by="study")
d$dd_c <- d$diseaseDuration - ddbar
d$active <- d$treatment != "Placebo"
trt <- setdiff(sort(unique(d$treatment)), "Placebo")
d$treatment <- factor(d$treatment, levels=c("Placebo", trt))
X <- model.matrix(~ 0 + factor(study) + treatment + I(dd_c*active), data=d)
X <- X[, !grepl("treatmentPlacebo", colnames(X)), drop=FALSE]   # placebo is the reference
y <- d$responders; n <- d$sampleSize
beta <- rep(0, ncol(X))
for (it in 1:100) {                              # IRLS = binomial spring Newton steps
  eta <- as.vector(X %*% beta); p <- plogis(eta)
  w <- n*p*(1-p) + 1e-9                          # spring hardness (Fisher info)
  z <- eta + (y - n*p)/w                         # working response (natural length)
  WX <- X * w
  beta_new <- solve(t(WX) %*% X, t(WX) %*% z)
  if (max(abs(beta_new-beta)) < 1e-10) { beta <- beta_new; break }
  beta <- beta_new
}
names(beta) <- colnames(X)
spr_beta <- beta["I(dd_c * active)"]
spr_d <- beta[grep("^treatment", names(beta))]
names(spr_d) <- sub("treatment","d.Placebo.", names(spr_d))

cat("\n================ CERTOLIZUMAB: exact binomial NMR ================\n")
cat(sprintf("%-22s %10s %10s %10s\n","parameter","gemtc","spring","glm-check"))
glmfit <- glm(cbind(y, n-y) ~ X-1, family=binomial)   # independent confirmation
gcoef <- coef(glmfit); names(gcoef) <- colnames(X)
prn <- function(lbl, g, s, gl) cat(sprintf("%-22s %10.3f %10.3f %10.3f\n", lbl, g, s, gl))
for (tt in trt) { nm <- paste0("d.Placebo.",tt)
  prn(nm, gem[nm,"Mean"], spr_d[nm], gcoef[paste0("treatment",tt)]) }
prn("B (beta, /yr)", gemB_yr, spr_beta, gcoef["I(dd_c * active)"])
cat(sprintf("\nNote: gemtc reports B per scale unit (=2*SD=%.3f yr); B/scale = %.3f /yr (SD %.3f).\n",
            scale, gemB_yr, gemBsd_yr))
cat(sprintf("Raw gemtc B (per 2 SD) = %.3f.\n", gem["B","Mean"]))
