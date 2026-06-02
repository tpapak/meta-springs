suppressMessages({library(jsonlite); library(multinma)}); options(mc.cores=2)
cov <- unlist(fromJSON("/tmp/nmrgc/cov.json"))
out <- data.frame()
for (r in 1:8) {
 j <- fromJSON(sprintf("/tmp/nmrgc/rep_%03d.json", r))
 ab <- do.call(rbind, lapply(1:nrow(j), function(i){rr<-j[i,]; rbind(
   data.frame(study=rr$study,treatment=rr$treatmentA,mean=rr$meanA,se=rr$sdA/sqrt(rr$nA),x=cov[[rr$study]]),
   data.frame(study=rr$study,treatment=rr$treatmentB,mean=rr$meanB,se=rr$sdB/sqrt(rr$nB),x=cov[[rr$study]]))}))
 ab$cls <- ifelse(ab$treatment=="A","ref","act")
 res <- tryCatch({
   net<-set_agd_arm(ab,study=study,trt=treatment,y=mean,se=se,trt_class=cls)
   fit<-nma(net,trt_effects="random",regression=~x,class_interactions="common",
            prior_intercept=normal(0,100),prior_trt=normal(0,100),
            prior_reg=normal(0,100),prior_het=half_normal(0.5),
            iter=3000,chains=3,refresh=0,control=list(adapt_delta=0.99,max_treedepth=12))
   m<-as.matrix(fit); rh<-max(summary(fit)$Rhat,na.rm=TRUE)
   c(beta=median(m[,"beta[x]"]), tau2=median(m[,"tau"])^2, rhat=rh)
 }, error=function(e) c(beta=NA,tau2=NA,rhat=NA))
 out<-rbind(out,data.frame(rep=r,mn_beta=res["beta"],mn_tau2=res["tau2"],rhat=res["rhat"]))
 cat(sprintf("rep %d: multinma beta[x]=%.3f tau2=%.4f maxRhat=%.2f\n",r,res["beta"],res["tau2"],res["rhat"]))
}
write.csv(out,"/tmp/nmrgc/multinma.csv",row.names=FALSE)
cat(sprintf("\nmultinma(%d): beta[x] mean=%.4f  tau2 mean=%.4f  (truth beta=0.30 tau2=0.10)\n",
    sum(complete.cases(out)),mean(out$mn_beta,na.rm=TRUE),mean(out$mn_tau2,na.rm=TRUE)))
