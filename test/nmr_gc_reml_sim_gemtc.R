suppressMessages({library(jsonlite); library(gemtc)})
cov <- unlist(fromJSON("/tmp/nmrgc/cov.json"))
out <- data.frame()
for (r in 1:25) {
  j <- fromJSON(sprintf("/tmp/nmrgc/rep_%03d.json", r))
  ab <- do.call(rbind, lapply(1:nrow(j), function(i){rr<-j[i,]; rbind(
    data.frame(study=rr$study,treatment=rr$treatmentA,mean=rr$meanA,std.err=rr$sdA/sqrt(rr$nA)),
    data.frame(study=rr$study,treatment=rr$treatmentB,mean=rr$meanB,std.err=rr$sdB/sqrt(rr$nB)))}))
  sd_ <- unique(data.frame(study=j$study, x=cov[j$study]))
  res <- tryCatch({
    net<-mtc.network(data.ab=ab, studies=sd_)
    gm <-mtc.model(net,type="regression",
                   regressor=list(coefficient="shared",variable="x",control="A"),
                   likelihood="normal",link="identity",linearModel="random")
    s<-summary(mtc.run(gm,n.adapt=8000,n.iter=40000,thin=2))
    q<-s$summaries$quantiles
    c(beta=q["B","50%"]/gm$regressor$scale, tau2=q["sd.d","50%"]^2)
  }, error=function(e) c(beta=NA,tau2=NA))
  out<-rbind(out,data.frame(rep=r,gemtc_beta=res["beta"],gemtc_tau2=res["tau2"]))
  cat(sprintf("rep %2d: gemtc beta=%.4f tau2=%.4f\n", r, res["beta"], res["tau2"]))
}
write.csv(out,"/tmp/nmrgc/gemtc.csv",row.names=FALSE)
cat(sprintf("\ngemtc(%d reps): beta mean=%.4f  tau2 mean=%.4f\n",
    sum(complete.cases(out)), mean(out$gemtc_beta,na.rm=TRUE), mean(out$gemtc_tau2,na.rm=TRUE)))
