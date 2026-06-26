# Aggregate the Monte-Carlo baseline-risk fits: bias, RMSE, 95% coverage vs truth.
suppressMessages(library(jsonlite))
H <- fromJSON("/tmp/brmc/haskell.json")
truth <- fromJSON("/tmp/brmc/truth.json")
Bt <- truth$B; dBt <- truth$d$B; dCt <- truth$d$C
n <- nrow(H)
cat(sprintf("\n=== Monte-Carlo verification: FE shared baseline-risk (%d reps) ===\n", n))
cat(sprintf("truth: B=%.2f, d[B]=%.2f, d[C]=%.2f\n\n", Bt, dBt, dCt))

summ <- function(est, se, tru, name) {
  bias <- mean(est) - tru
  rmse <- sqrt(mean((est - tru)^2))
  cov  <- mean(abs(est - tru) <= 1.96*se)        # 95% Wald coverage
  sdEmp<- sd(est); seMean <- mean(se)
  cat(sprintf("%-6s  mean=%+.4f  bias=%+.4f  RMSE=%.4f  emp.sd=%.4f  mean.se=%.4f  cover95=%.3f\n",
              name, mean(est), bias, rmse, sdEmp, seMean, cov))
}
summ(H$B,  H$Bse,  Bt,  "B")
summ(H$dB, H$dBse, dBt, "d[B]")
summ(H$dC, H$dCse, dCt, "d[C]")
cat(sprintf("\nmean tau2 (FE DGP, should be ~0): %.4f\n", mean(H$tau2)))
cat("\nInterpretation: bias~0, RMSE small, mean.se ~ emp.sd, coverage ~0.95 => the\n")
cat("Fisher-scoring baseline-risk MLE is ~unbiased and its observed-information SE is calibrated.\n")
