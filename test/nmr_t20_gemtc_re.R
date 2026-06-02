suppressMessages({library(jsonlite); library(gemtc)})
recs<-fromJSON("/tmp/t20nmr/data.json",simplifyDataFrame=FALSE); cov<-unlist(fromJSON("/tmp/t20nmr/cov.json"))
ab<-do.call(rbind,lapply(recs,function(r)data.frame(study=r$study,treatment=r$treatment,mean=r$mean,std.err=r$sd/sqrt(r$n))))
sid<-unique(ab$study)
net<-mtc.network(data.ab=ab,studies=data.frame(study=sid,x=as.numeric(cov[sid])))
gm<-mtc.model(net,type="regression",regressor=list(coefficient="shared",variable="x",control="T01"),
              likelihood="normal",link="identity",linearModel="random",hy.prior=mtc.hy.prior("std.dev","dhnorm",0,1/0.5^2))
t0<-Sys.time(); s<-summary(mtc.run(gm,n.adapt=5000,n.iter=20000,thin=2)); el<-as.numeric(Sys.time()-t0,units="secs")
q<-s$summaries$quantiles
cat(sprintf("gemtc RE T20: beta=%.4f tau2=%.5f  time=%.2f s\n", q["B","50%"]/gm$regressor$scale, q["sd.d","50%"]^2, el))
