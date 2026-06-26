# Known-truth simulation for binary baseline-risk NMR (Achana/bnma model).
# A denser network than certolizumab so MLE and Bayes both concentrate on truth.
#   logit control = eta_i ;  logit active_t = eta_i + d_t + B*(eta_i - mx_true)
# Writes the dataset (readStudies format), the truth, the optim MLE, and runs bnma.

suppressMessages({ library(bnma); library(jsonlite) })
set.seed(20260603)

## ---- truth ----
nA <- 16; nB <- 16; nD <- 16            # studies per active-vs-control comparison
actives <- c("B","C","D")
nper <- c(B = 16, C = 16, D = 16)
d_true <- c(B = 0.8, C = 1.2, D = 0.5)  # contrasts at baseline = mx_true
B_true <- -0.60                          # shared baseline-risk slope
narm <- 200                              # arm size (large => MLE concentrates)
eta_sd <- 0.70                           # spread of study baselines (identifies the slope)

studies <- list(); k <- 0
etas <- c()
for (a in actives) for (j in 1:nper[a]) {
  k <- k + 1
  eta <- rnorm(1, -1.0, eta_sd); etas <- c(etas, eta)
  studies[[k]] <- list(id = sprintf("S%02d", k), active = a, eta = eta)
}
mx_true <- mean(etas)

rows <- lapply(studies, function(s) {
  pc <- plogis(s$eta)
  pa <- plogis(s$eta + d_true[s$active] + B_true * (s$eta - mx_true))
  list(study = s$id,
       treatmentA = "A", eventsA = rbinom(1, narm, pc), nA = narm,
       treatmentB = s$active, eventsB = rbinom(1, narm, pa), nB = narm)
})
write_json(rows, "test/nmr_br_sim.json", auto_unbox = TRUE, pretty = TRUE)

## ---- flatten to arm-level for MLE/bnma ----
O <- c(); N <- c(); Study <- c(); Treat <- c()
for (r in rows) {
  O <- c(O, r$eventsA, r$eventsB); N <- c(N, r$nA, r$nB)
  Study <- c(Study, r$study, r$study); Treat <- c(Treat, "A", r$treatmentB)
}
trts <- c("A", actives); ref <- "A"; nonref <- actives
sidx <- setNames(seq_along(unique(Study)), unique(Study)); nS <- length(sidx)
tidx <- setNames(seq_along(nonref), nonref)

ic <- Treat == ref; rc <- O[ic]; nc <- N[ic]
rd <- rc; rd[rc == 0] <- 0.5; nd <- nc; nd[rc == 0] <- nc[rc == 0] + 1
mx <- mean(log((rd/nd)/(1 - rd/nd)))

si <- sidx[Study]; active <- Treat != ref; ti <- ifelse(active, tidx[Treat], 0L)
negll <- function(par) {
  eta_s <- par[1:nS]; d <- par[nS + (1:length(nonref))]; B <- par[nS + length(nonref) + 1]
  lp <- eta_s[si]; ai <- which(active)
  lp[ai] <- lp[ai] + d[ti[ai]] + B * (eta_s[si[ai]] - mx)
  -sum(dbinom(O, N, plogis(lp), log = TRUE))
}
init <- c(log((rd/nd)/(1 - rd/nd)), rep(0, length(nonref)), 0)
fit <- optim(init, negll, method = "BFGS", hessian = TRUE, control = list(maxit = 5000, reltol = 1e-12))
se <- sqrt(diag(solve(fit$hessian))); nd_ <- length(nonref)
dhat <- setNames(fit$par[nS + (1:nd_)], nonref); dse <- setNames(se[nS + (1:nd_)], nonref)
Bhat <- fit$par[nS + nd_ + 1]; Bse <- se[nS + nd_ + 1]

cat(sprintf("\n=== SIM truth: B=%.3f  d=[B %.2f, C %.2f, D %.2f]  mx_true=%.3f  mx_data=%.3f ===\n",
            B_true, d_true["B"], d_true["C"], d_true["D"], mx_true, mx))
cat("\n--- FE baseline-risk MLE (optim) ---\n")
cat(sprintf("B = %.4f (se %.4f)\n", Bhat, Bse))
for (t in nonref) cat(sprintf("d[%s] = %.4f (se %.4f)\n", t, dhat[t], dse[t]))

## ---- bnma Bayesian (the MetaInsight engine) ----
cat("\n--- bnma FIXED-effect, shared slope ---\n")
net <- network.data(Outcomes = O, Study = Study, Treat = Treat, N = N,
                    response = "binomial", Treat.order = trts,
                    type = "fixed", baseline = "common", baseline.risk = "independent",
                    prec.Eta = 1e-4)
res <- network.run(net, n.chains = 4, max.run = 600000, setsize = 20000, n.run = 100000)
ss <- summary(res)$summary.samples$statistics
print(round(ss[grep("^B$|^d\\[|^b_bl\\[2\\]", rownames(ss)), , drop = FALSE], 4))
B_bnma <- ss[if ("B" %in% rownames(ss)) "B" else "b_bl[2]", "Mean"]

dir.create("test/nmr_baseline_risk_results", showWarnings = FALSE)
write_json(list(
  truth = list(B = B_true, d = as.list(d_true), mx_true = mx_true),
  mx_data = mx,
  mle = list(B = list(est = Bhat, se = Bse),
             d = setNames(lapply(nonref, function(t) list(est = unname(dhat[t]), se = unname(dse[t]))), nonref)),
  bnma_B = B_bnma),
  "test/nmr_baseline_risk_results/sim.json", auto_unbox = TRUE, pretty = TRUE, digits = 8)
cat("\nWrote test/nmr_br_sim.json and test/nmr_baseline_risk_results/sim.json\n")
