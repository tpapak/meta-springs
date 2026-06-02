#!/usr/bin/env Rscript
# Proper netmeta network meta-regression (netmetareg, REML, common slope) over
# all reps -> /tmp/nmrgc/netmetareg.csv  (rep, beta, tau2)
suppressMessages({library(jsonlite); library(netmeta)})
cov <- unlist(fromJSON("/tmp/nmrgc/cov.json"))
out <- data.frame()
for (r in 1:200) {
  j <- fromJSON(sprintf("/tmp/nmrgc/rep_%03d.json", r))
  TE <- j$meanB - j$meanA; seTE <- sqrt(j$sdA^2/j$nA + j$sdB^2/j$nB)
  x <- cov[j$study]
  res <- tryCatch({
    nm <- netmeta(TE,seTE,j$treatmentA,j$treatmentB,j$study,sm="MD",
                  reference.group="A", random=TRUE, common=FALSE)
    rg <- netmetareg(nm, covar=x, assumption="common", method.tau="REML")
    c(beta = -as.numeric(tail(coef(rg),1)),  # slope is last coef; netmetareg uses opposite sign
      tau2 = rg$tau2)
  }, error=function(e) c(beta=NA, tau2=NA))
  out <- rbind(out, data.frame(rep=r, beta=res["beta"], tau2=res["tau2"]))
}
write.csv(out, "/tmp/nmrgc/netmetareg.csv", row.names=FALSE)
cat(sprintf("netmetareg over %d reps: beta mean=%.4f (sd %.4f) ; tau2 mean=%.4f\n",
    sum(complete.cases(out)), mean(out$beta,na.rm=TRUE), sd(out$beta,na.rm=TRUE),
    mean(out$tau2,na.rm=TRUE)))
