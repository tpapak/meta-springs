#!/usr/bin/env Rscript
# Bayesian subset comparison for the GC-vs-REML simulation: fit RANDOM-effects
# network meta-regression with gemtc and multinma on the same reps, extract the
# shared covariate slope beta and the heterogeneity tau2 (contrast scale).
# Subset of reps (MCMC is slow). Output: /tmp/nmrgc/bayes.csv
suppressMessages({ library(jsonlite); library(gemtc); library(multinma) })
options(mc.cores = 2)

cov  <- unlist(fromJSON("/tmp/nmrgc/cov.json"))
REPS <- 1:25

armsOf <- function(rep) {
  j <- fromJSON(sprintf("/tmp/nmrgc/rep_%03d.json", rep))
  do.call(rbind, lapply(seq_len(nrow(j)), function(i) {
    r <- j[i,]
    rbind(
      data.frame(study=r$study, treatment=r$treatmentA, mean=r$meanA,
                 std.err=r$sdA/sqrt(r$nA), x=cov[[r$study]], stringsAsFactors=FALSE),
      data.frame(study=r$study, treatment=r$treatmentB, mean=r$meanB,
                 std.err=r$sdB/sqrt(r$nB), x=cov[[r$study]], stringsAsFactors=FALSE))
  }))
}

fitGemtc <- function(ab) tryCatch({
  studyData <- unique(ab[,c("study","x")])
  net <- mtc.network(data.ab=ab[,c("study","treatment","mean","std.err")], studies=studyData)
  gm  <- mtc.model(net, type="regression",
                   regressor=list(coefficient="shared", variable="x", control="A"),
                   likelihood="normal", link="identity", linearModel="random")
  s   <- summary(mtc.run(gm, n.adapt=5000, n.iter=20000, thin=2))
  st  <- s$summaries$statistics; qq <- s$summaries$quantiles
  beta <- qq["B","50%"] / gm$regressor$scale
  tau  <- qq["sd.d","50%"]                       # heterogeneity SD (median)
  c(beta=beta, tau2=tau^2)
}, error=function(e) c(beta=NA, tau2=NA))

fitMultinma <- function(ab) tryCatch({
  ab$cls <- ifelse(ab$treatment=="A","ref","act")
  net <- set_agd_arm(ab, study=study, trt=treatment, y=mean, se=std.err, trt_class=cls)
  fit <- nma(net, trt_effects="random", regression=~x, class_interactions="common",
             prior_intercept=normal(0,100), prior_trt=normal(0,100),
             prior_reg=normal(0,100), prior_het=half_normal(0.5),
             iter=2000, chains=2, refresh=0)
  sm  <- as.data.frame(summary(fit))
  rn  <- rownames(summary(fit));
  ss  <- summary(fit)
  df  <- as.data.frame(ss)
  getp <- function(pat) { i<-grep(pat, df$parameter); if(length(i)) df$`50%`[i[1]] else NA }
  beta <- getp("^beta\\[|:x|\\.x")
  tau  <- getp("^tau$|^tau\\[")
  c(beta=beta, tau2=if(is.na(tau)) NA else tau^2)
}, error=function(e) { c(beta=NA, tau2=NA) })

out <- data.frame()
for (r in REPS) {
  ab <- armsOf(r)
  g  <- fitGemtc(ab); m <- fitMultinma(ab)
  out <- rbind(out, data.frame(rep=r, gemtc_beta=g["beta"], gemtc_tau2=g["tau2"],
                               mn_beta=m["beta"], mn_tau2=m["tau2"]))
  cat(sprintf("rep %2d: gemtc beta=%.3f tau2=%.4f | multinma beta=%s tau2=%s\n",
      r, g["beta"], g["tau2"],
      ifelse(is.na(m["beta"]),"NA",sprintf("%.3f",m["beta"])),
      ifelse(is.na(m["tau2"]),"NA",sprintf("%.4f",m["tau2"]))))
}
write.csv(out, "/tmp/nmrgc/bayes.csv", row.names=FALSE)
cat("\nDONE. gemtc: beta mean=", round(mean(out$gemtc_beta,na.rm=TRUE),4),
    " tau2 mean=", round(mean(out$gemtc_tau2,na.rm=TRUE),4),
    "\nmultinma: beta mean=", round(mean(out$mn_beta,na.rm=TRUE),4),
    " tau2 mean=", round(mean(out$mn_tau2,na.rm=TRUE),4), "\n")
