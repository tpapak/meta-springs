#!/usr/bin/env Rscript
# netmetareg (REML, common slope) reference: per-rep beta, SE, beta 95% CI (Wald),
# tau2, tau2 95% CI (profile-likelihood via confint).
suppressMessages({library(jsonlite); library(netmeta)})
cov <- unlist(fromJSON("/tmp/nmrgc/cov.json")); out <- data.frame()
for (r in 1:20) {
  j <- fromJSON(sprintf("/tmp/nmrgc/rep_%03d.json", r))
  TE<-j$meanB-j$meanA; seTE<-sqrt(j$sdA^2/j$nA+j$sdB^2/j$nB); x<-cov[j$study]
  res <- tryCatch({
    nm<-netmeta(TE,seTE,j$treatmentA,j$treatmentB,j$study,sm="MD",reference.group="A",random=TRUE,common=FALSE)
    rg<-netmetareg(nm,covar=x,assumption="common",method.tau="REML")
    k<-length(rg$se); b<- -as.numeric(coef(rg)[k]); se<-rg$se[k]
    ci<-confint(rg)$random
    c(beta=b, se=se, blo=b-1.96*se, bhi=b+1.96*se,
      tau2=rg$tau2, t2lo=ci["tau^2","ci.lb"], t2hi=ci["tau^2","ci.ub"])
  }, error=function(e){cat("rep",r,"ERR",conditionMessage(e),"\n"); rep(NA,7)})
  out<-rbind(out,data.frame(rep=r,beta=res[1],se=res[2],blo=res[3],bhi=res[4],
                            tau2=res[5],t2lo=res[6],t2hi=res[7]))
}
write.csv(out,"/tmp/nmrgc/nmr_ref.csv",row.names=FALSE); cat("done\n"); print(round(head(out,2),4))
