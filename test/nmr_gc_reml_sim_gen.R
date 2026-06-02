#!/usr/bin/env Rscript
# Honest Monte Carlo for network meta-regression tau2 estimation:
#   GC (grand canonical) vs REML vs truth.
#
# Generative model = EXACTLY the spring RE model. Treatments A,B,C.
# Every arm (incl. reference) carries an iid random effect u ~ N(0, tau2_arm),
# tau2_arm = tau2_contrast / 2, so any within-study contrast has heterogeneity
# variance 2*tau2_arm = tau2_contrast (the standard contrast-scale tau2).
# Shared covariate slope beta shifts non-reference arms by beta*x_study.
#
# Topology + covariate are FIXED across reps (a design); only outcomes vary,
# so bias is well defined. Outputs:
#   /tmp/nmrgc/rep_NN.json  (wide 2-arm arm summaries)
#   /tmp/nmrgc/cov.json     ({study: x})
#   /tmp/nmrgc/truth.json
#   /tmp/nmrgc/ext.csv      (per-rep EXTERNAL REML: beta, tau2 via metafor/netmeta)
suppressPackageStartupMessages({ library(jsonlite); library(metafor); library(netmeta) })

dir.create("/tmp/nmrgc", showWarnings = FALSE, recursive = TRUE)

## ---- truth ----
dB    <- 0.5      # A->B at x=0
dC    <- 1.2      # A->C at x=0
beta  <- 0.3      # shared covariate slope (non-ref arms)
tau2c <- 0.10     # CONTRAST-scale heterogeneity variance (the estimand)
tau2a <- tau2c/2  # arm-scale (spring convention)
sigma <- 1.0      # within-arm SD
nArm  <- 100L     # per-arm sample size  => arm sampling var = 0.01
nReps <- 200L

## ---- topology (fixed): 10 A-B, 6 A-C, 4 B-C ----
set.seed(7)
design <- c(rep("AB",10), rep("AC",6), rep("BC",4))
k      <- length(design)
studyId<- sprintf("S%02d", 1:k)
xcov   <- round(rnorm(k, 0, 1.2), 3)           # study covariate
names(xcov) <- studyId
write_json(as.list(xcov), "/tmp/nmrgc/cov.json", auto_unbox = TRUE, digits = 6)
write_json(list(dB=dB,dC=dC,beta=beta,tau2_contrast=tau2c,tau2_arm=tau2a,
                sigma=sigma,nArm=nArm,k=k,nReps=nReps),
           "/tmp/nmrgc/truth.json", auto_unbox = TRUE, digits = 8)

armMean <- function(mu, trt, x, u) {
  d <- c(A=0, B=dB, C=dC)[trt]
  shift <- if (trt == "A") 0 else beta * x
  mu + d + shift + u
}

ext <- data.frame(rep=integer(), beta=double(), tau2=double())

for (r in seq_len(nReps)) {
  set.seed(1000 + r)
  recs <- vector("list", k)
  # collect anchored contrasts for the external regression
  yAnch <- c(); xAnch <- c(); cmpAnch <- c()
  yAll  <- c(); seAll <- c(); t1All <- c(); t2All <- c(); stAll <- c(); xAll <- c()
  for (i in seq_len(k)) {
    cmp <- design[i]; t1 <- substr(cmp,1,1); t2 <- substr(cmp,2,2)
    mu  <- rnorm(1, 0, 5); x <- xcov[i]
    u1  <- rnorm(1, 0, sqrt(tau2a)); u2 <- rnorm(1, 0, sqrt(tau2a))
    m1  <- armMean(mu, t1, x, u1) + rnorm(1, 0, sigma/sqrt(nArm))
    m2  <- armMean(mu, t2, x, u2) + rnorm(1, 0, sigma/sqrt(nArm))
    recs[[i]] <- list(study=studyId[i],
                      treatmentA=t1, meanA=m1, sdA=sigma, nA=nArm,
                      treatmentB=t2, meanB=m2, sdB=sigma, nB=nArm)
    ctr <- m2 - m1; se <- sqrt(2*sigma^2/nArm)
    yAll <- c(yAll, ctr); seAll <- c(seAll, se)
    t1All<- c(t1All,t1); t2All<- c(t2All,t2); stAll<- c(stAll,studyId[i]); xAll<-c(xAll,x)
    if (cmp %in% c("AB","AC")) { yAnch<-c(yAnch,ctr); xAnch<-c(xAnch,x); cmpAnch<-c(cmpAnch,cmp) }
  }
  write_json(recs, sprintf("/tmp/nmrgc/rep_%03d.json", r), auto_unbox=TRUE, digits=10)

  # EXTERNAL REML: shared-slope meta-regression on anchored contrasts
  # yi ~ cmp (two intercepts dB,dC) + x (shared beta); REML residual tau2.
  mr <- tryCatch(
    rma(yi=yAnch, vi=rep(2*sigma^2/nArm, length(yAnch)),
        mods=~factor(cmpAnch)+xAnch-1, method="REML"),
    error=function(e) NULL)
  bhat <- if (is.null(mr)) NA else as.numeric(coef(mr)["xAnch"])
  # full-network tau2 (REML) on covariate-adjusted contrasts
  adj <- yAll - ifelse(t2All!="A", ifelse(!is.na(bhat),bhat,0)*xAll, 0)  # subtract beta*x on non-ref second arm
  nm  <- tryCatch(netmeta(adj, seAll, t1All, t2All, stAll, sm="MD",
                          reference.group="A", method.tau="REML"),
                  error=function(e) NULL)
  t2hat <- if (is.null(nm)) NA else nm$tau2
  ext <- rbind(ext, data.frame(rep=r, beta=bhat, tau2=t2hat))
}
write.csv(ext, "/tmp/nmrgc/ext.csv", row.names=FALSE)
cat(sprintf("generated %d reps; truth: beta=%.2f tau2_contrast=%.3f (arm %.3f)\n",
            nReps, beta, tau2c, tau2a))
cat(sprintf("external REML (metafor/netmeta): beta mean=%.4f sd=%.4f ; tau2 mean=%.4f\n",
            mean(ext$beta,na.rm=TRUE), sd(ext$beta,na.rm=TRUE), mean(ext$tau2,na.rm=TRUE)))
