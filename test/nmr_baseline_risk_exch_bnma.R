# bnma exchangeable baseline-risk on certolizumab: the Bayesian posterior of the slope
# dispersion sdB (σ_B = sqrt σ_B²), to validate the 2-D GC σ_B² mean.
suppressMessages({ library(bnma) }); set.seed(1)
data(certolizumab)
res <- network.run(
  network.data(Outcomes=certolizumab$Outcomes, Study=certolizumab$Study,
               Treat=certolizumab$Treat, N=certolizumab$N, response="binomial",
               Treat.order=certolizumab$Treat.order, type="fixed",
               baseline="exchangeable", baseline.risk="independent", prec.Eta=1e-4),
  n.chains=4, max.run=600000, setsize=20000, n.run=120000)
ss <- summary(res)$summary.samples$statistics
rn <- rownames(ss)
cat("\n=== bnma exchangeable baseline-risk, certolizumab ===\n")
print(round(ss[grep("^B$|^b_bl\\[|^sd", rn), c("Mean","SD"), drop=FALSE], 4))
sdrow <- grep("^sdB$|^sd$", rn, value=TRUE)[1]
if (!is.na(sdrow)) {
  m <- ss[sdrow,"Mean"]; s <- ss[sdrow,"SD"]
  cat(sprintf("\nsdB posterior: mean=%.4f sd=%.4f  =>  sigma_B^2 ~ mean(sdB)^2 = %.4f (+E[var]) \n", m, s, m*m))
}
