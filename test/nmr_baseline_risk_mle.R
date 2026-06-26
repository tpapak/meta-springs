# Exact MLE reference for the FIXED-effect binary baseline-risk model (Achana/bnma),
# computed by direct optim on the identical nonlinear likelihood. This is the
# unambiguous point-estimate target for the Haskell Fisher-scoring solver.
# Also exports bnma::certolizumab to test/nmr_certolizumab.json (readStudies format).

suppressMessages({ library(bnma); library(jsonlite) })
data(certolizumab)
O <- certolizumab$Outcomes; N <- certolizumab$N
Study <- certolizumab$Study; Treat <- certolizumab$Treat
trts  <- certolizumab$Treat.order            # Placebo first
ref   <- "Placebo"
nonref <- trts[trts != ref]

studies <- unique(Study); nS <- length(studies)
sidx <- setNames(seq_along(studies), studies)
tidx <- setNames(seq_along(nonref), nonref)   # 1..6

# centering mx = mean cc control log-odds
ic <- Treat == ref; rc <- O[ic]; nc <- N[ic]
rd <- rc; rd[rc == 0] <- 0.5; nd <- nc; nd[rc == 0] <- nc[rc == 0] + 1
mx <- mean(log((rd/nd)/(1 - rd/nd)))
cat(sprintf("mx = %.6f\n", mx))

si <- sidx[Study]; active <- Treat != ref
ti <- ifelse(active, tidx[Treat], 0)

negll <- function(par) {
  eta_s <- par[1:nS]; d <- par[nS + (1:length(nonref))]; B <- par[nS + length(nonref) + 1]
  lp <- eta_s[si]                                   # control arms: lp = eta_i
  ai <- which(active)                               # active arms only (avoid d[0])
  lp[ai] <- lp[ai] + d[ti[ai]] + B * (eta_s[si[ai]] - mx)
  p <- plogis(lp)
  -sum(dbinom(O, N, p, log = TRUE))
}
init <- c(log((rd/nd)/(1 - rd/nd)), rep(0, length(nonref)), 0)
fit <- optim(init, negll, method = "BFGS", hessian = TRUE,
             control = list(maxit = 5000, reltol = 1e-12))
se <- sqrt(diag(solve(fit$hessian)))
nd_ <- length(nonref)
dhat <- fit$par[nS + (1:nd_)]; dse <- se[nS + (1:nd_)]
Bhat <- fit$par[nS + nd_ + 1]; Bse <- se[nS + nd_ + 1]
names(dhat) <- nonref; names(dse) <- nonref

cat("\n=== FE baseline-risk MLE (optim) ===\n")
cat(sprintf("B = %.5f (se %.5f)\n", Bhat, Bse))
for (t in nonref) cat(sprintf("d[%s] = %.5f (se %.5f)\n", t, dhat[t], dse[t]))
cat(sprintf("convergence=%d  -logL=%.4f\n", fit$convergence, fit$value))

write_json(list(mx = mx, B = list(est = Bhat, se = Bse),
                d = setNames(lapply(nonref, function(t) list(est = unname(dhat[t]), se = unname(dse[t]))), nonref)),
           "test/nmr_baseline_risk_results/mle.json", auto_unbox = TRUE, pretty = TRUE, digits = 8)

# ---- export certolizumab in readStudies (pairwise binary.json) format ----
# every study is 2-arm: arm A = Placebo (reference), arm B = the active drug
rows <- lapply(studies, function(s) {
  idx <- which(Study == s)
  ctrl <- idx[Treat[idx] == ref][1]; act <- idx[Treat[idx] != ref][1]
  list(study = s,
       treatmentA = Treat[ctrl], eventsA = O[ctrl], nA = N[ctrl],
       treatmentB = Treat[act],  eventsB = O[act],  nB = N[act])
})
write_json(rows, "test/nmr_certolizumab.json", auto_unbox = TRUE, pretty = TRUE)
cat("\nWrote test/nmr_certolizumab.json and test/nmr_baseline_risk_results/mle.json\n")
