suppressMessages({library(jsonlite); library(gemtc)})
set.seed(11)
dB<-0.6; dC<-1.0; beta<-0.25      # logOR vs A; shared logit-scale slope
designs<-c(rep("AB",5),rep("AC",4),rep("BC",3)); k<-length(designs)
sid<-sprintf("S%02d",1:k); x<-round(rnorm(k,0,1.3),3); names(x)<-sid; nArm<-300L
recs<-list()
for(i in 1:k){cmp<-designs[i]; t1<-substr(cmp,1,1); t2<-substr(cmp,2,2)
  mu<-rnorm(1,-0.4,0.3); d<-c(A=0,B=dB,C=dC)
  lp<-function(t) mu + d[[t]] + (if(t!="A") beta*x[i] else 0)
  e1<-rbinom(1,nArm,plogis(lp(t1))); e2<-rbinom(1,nArm,plogis(lp(t2)))
  recs[[i]]<-list(study=sid[i],treatmentA=t1,eventsA=e1,nA=nArm,treatmentB=t2,eventsB=e2,nB=nArm)}
write_json(recs,"/tmp/binnmr/data.json",auto_unbox=TRUE)
write_json(as.list(x),"/tmp/binnmr/cov.json",auto_unbox=TRUE,digits=6)
# gemtc exact binomial FE shared-beta regression
ab<-do.call(rbind,lapply(recs,function(r)rbind(
  data.frame(study=r$study,treatment=r$treatmentA,responders=r$eventsA,sampleSize=r$nA),
  data.frame(study=r$study,treatment=r$treatmentB,responders=r$eventsB,sampleSize=r$nB))))
net<-mtc.network(data.ab=ab,studies=data.frame(study=sid,x=as.numeric(x)))
gm<-mtc.model(net,type="regression",regressor=list(coefficient="shared",variable="x",control="A"),
              likelihood="binom",link="logit",linearModel="fixed")
s<-summary(mtc.run(gm,n.adapt=6000,n.iter=30000,thin=2)); sc<-gm$regressor$scale
q<-s$summaries$quantiles
cat(sprintf("TRUTH beta=%.3f dB=%.2f dC=%.2f\n",beta,dB,dC))
cat(sprintf("gemtc: beta=%.4f  A->B=%.4f  A->C=%.4f\n",
    q["B","50%"]/sc, q["d.A.B","50%"], q["d.A.C","50%"]))
