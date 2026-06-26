# Known-truth CONTINUOUS (mean-difference) baseline-risk NMR (Achana/bnma, identity link).
#   y_control = eta_i + eps ;  y_active_t = eta_i + d_t + B*(eta_i - mx_true) + eps
# Writes the dataset (readStudies continuous format), truth, optim MLE, and runs bnma.
suppressMessages({ library(bnma); library(jsonlite) })
set.seed(20260603)

actives <- c("B","C","D"); nper <- c(B=16, C=16, D=16)
d_true  <- c(B=2.0, C=3.0, D=1.0)        # contrasts at baseline = mx_true
B_true  <- -0.40                          # baseline-risk slope (identity scale)
sigma   <- 5; narm <- 100                 # within-arm SD, arm size  => arm var = 0.25
eta_sd  <- 3.0                            # baseline spread (identifies the slope)

studies <- list(); k <- 0; etas <- c()
for (a in actives) for (j in 1:nper[a]) {
  k <- k + 1; eta <- rnorm(1, 10, eta_sd); etas <- c(etas, eta)
  studies[[k]] <- list(id=sprintf("S%02d",k), active=a, eta=eta)
}
mx_true <- mean(etas)
se <- sigma / sqrt(narm)                   # SE of an arm mean
rows <- lapply(studies, function(s) {
  yc <- rnorm(1, s$eta, se)
  ya <- rnorm(1, s$eta + d_true[s$active] + B_true*(s$eta - mx_true), se)
  list(study=s$id, treatmentA="A", meanA=yc, sdA=sigma, nA=narm,
       treatmentB=s$active, meanB=ya, sdB=sigma, nB=narm)
})
write_json(rows, "test/nmr_br_md_sim.json", auto_unbox=TRUE, pretty=TRUE)

## flatten
Y <- c(); SE <- c(); Study <- c(); Treat <- c()
for (r in rows) { Y<-c(Y,r$meanA,r$meanB); SE<-c(SE,r$sdA/sqrt(r$nA),r$sdB/sqrt(r$nB))
  Study<-c(Study,r$study,r$study); Treat<-c(Treat,"A",r$treatmentB) }
trts<-c("A",actives); ref<-"A"; nonref<-actives
sidx<-setNames(seq_along(unique(Study)),unique(Study)); nS<-length(sidx)
tidx<-setNames(seq_along(nonref),nonref)
mx <- mean(Y[Treat==ref])                  # centering = mean control mean
si<-sidx[Study]; active<-Treat!=ref; ti<-ifelse(active,tidx[Treat],0L); V<-SE^2

negll <- function(par){ eta<-par[1:nS]; d<-par[nS+(1:length(nonref))]; B<-par[nS+length(nonref)+1]
  mu<-eta[si]; ai<-which(active); mu[ai]<-mu[ai]+d[ti[ai]]+B*(eta[si[ai]]-mx)
  0.5*sum((Y-mu)^2/V) }
init<-c(tapply(Y[Treat==ref],si[Treat==ref],mean)[as.character(1:nS)], rep(0,length(nonref)),0)
fit<-optim(init,negll,method="BFGS",hessian=TRUE,control=list(maxit=5000,reltol=1e-12))
seE<-sqrt(diag(solve(fit$hessian))); nd_<-length(nonref)
dhat<-setNames(fit$par[nS+(1:nd_)],nonref); dse<-setNames(seE[nS+(1:nd_)],nonref)
Bhat<-fit$par[nS+nd_+1]; Bse<-seE[nS+nd_+1]
cat(sprintf("\n=== SIM truth: B=%.3f d=[B %.2f,C %.2f,D %.2f] mx_true=%.3f mx_data=%.3f ===\n",
            B_true,d_true["B"],d_true["C"],d_true["D"],mx_true,mx))
cat("\n--- MD baseline-risk MLE (optim, weighted LS) ---\n")
cat(sprintf("B = %.4f (se %.4f)\n",Bhat,Bse))
for (t in nonref) cat(sprintf("d[%s] = %.4f (se %.4f)\n",t,dhat[t],dse[t]))

cat("\n--- bnma FIXED-effect, shared slope (response=normal) ---\n")
net<-network.data(Outcomes=Y, Study=Study, Treat=Treat, SE=SE, response="normal",
                  Treat.order=trts, type="fixed", baseline="common",
                  baseline.risk="independent", prec.Eta=1e-4)
res<-network.run(net, n.chains=4, max.run=400000, setsize=20000, n.run=100000)
ss<-summary(res)$summary.samples$statistics
print(round(ss[grep("^B$|^d\\[|^b_bl\\[2\\]",rownames(ss)),c("Mean","SD"),drop=FALSE],4))

dir.create("test/nmr_baseline_risk_results", showWarnings=FALSE)
write_json(list(truth=list(B=B_true,d=as.list(d_true),mx_true=mx_true), mx=mx,
                mle=list(B=list(est=Bhat,se=Bse),
                         d=setNames(lapply(nonref,function(t) list(est=unname(dhat[t]),se=unname(dse[t]))),nonref))),
           "test/nmr_baseline_risk_results/md_sim.json", auto_unbox=TRUE, pretty=TRUE, digits=8)
cat("\nWrote test/nmr_br_md_sim.json and md_sim.json\n")
