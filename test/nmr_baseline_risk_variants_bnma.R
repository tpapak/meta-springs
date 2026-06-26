# bnma reference for baseline-risk SLOPE variants (shared/unrelated/exchangeable) on the
# simulation (DGP = shared B=-0.60). Validates nmaRegressionBaselineRiskBinB.
#   common -> shared B ; independent -> per-treatment b_bl[t] ; exchangeable -> b_bl~N(B,sd^2)
suppressMessages({ library(bnma); library(jsonlite) })
set.seed(1)

j <- fromJSON("test/nmr_br_sim.json")
O <- c(); N <- c(); Study <- c(); Treat <- c()
for (i in seq_len(nrow(j))) {
  O <- c(O, j$eventsA[i], j$eventsB[i]); N <- c(N, j$nA[i], j$nB[i])
  Study <- c(Study, j$study[i], j$study[i]); Treat <- c(Treat, j$treatmentA[i], j$treatmentB[i])
}
trts <- c("A","B","C","D")

runv <- function(base) {
  net <- network.data(Outcomes=O, Study=Study, Treat=Treat, N=N, response="binomial",
                      Treat.order=trts, type="fixed", baseline=base, baseline.risk="independent",
                      prec.Eta=1e-4)
  res <- network.run(net, n.chains=3, max.run=400000, setsize=20000, n.run=100000)
  ss <- summary(res)$summary.samples$statistics
  rn <- rownames(ss)
  cat(sprintf("\n--- bnma baseline='%s' ---\n", base))
  print(round(ss[grep("^B$|^b_bl\\[|^sd", rn), c("Mean","SD"), drop=FALSE], 4))
}
cat("###### bnma slope variants on the simulation (truth: shared B=-0.60) ######\n")
runv("common")        # shared
runv("independent")   # unrelated: b_bl[2..4] per treatment (B,C,D)
runv("exchangeable")  # exchangeable: b_bl[k]~N(B, sd^2)
