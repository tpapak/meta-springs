suppressMessages({library(jsonlite); library(gemtc)})
set.seed(42)
T<-20; treats<-sprintf("T%02d",1:T); ref<-"T01"; beta<-0.30
d<-c(0, rnorm(T-1,0,0.6)); names(d)<-treats        # d[ref]=0
# 76 two-arm anchored studies (4 reps of ref vs each non-ref) -> connected, beta identified
designs<-list(); for(rep in 1:4) for(j in 2:T) designs<-c(designs,list(c(ref,treats[j])))
k<-length(designs); sid<-sprintf("S%03d",1:k); x<-round(rnorm(k,0,1.3),3); names(x)<-sid
sigma<-2; nArm<-200L; recs<-list(); ab<-data.frame()
for(i in 1:k){arms<-designs[[i]]; mu<-rnorm(1,0,5)
  for(t in arms){m<-mu+d[[t]]+(if(t!=ref)beta*x[i] else 0)+rnorm(1,0,sigma/sqrt(nArm))
    recs<-c(recs,list(list(study=sid[i],treatment=t,mean=m,sd=sigma,n=nArm)))
    ab<-rbind(ab,data.frame(study=sid[i],treatment=t,mean=m,std.err=sigma/sqrt(nArm)))}}
write_json(recs,"/tmp/t20nmr/data.json",auto_unbox=TRUE,digits=10)
write_json(as.list(x),"/tmp/t20nmr/cov.json",auto_unbox=TRUE,digits=6)
cat(sprintf("T=%d studies=%d arms=%d truth beta=%.2f\n",T,k,nrow(ab),beta))
# gemtc FE shared beta + timing
net<-mtc.network(data.ab=ab,studies=data.frame(study=sid,x=as.numeric(x)))
gm<-mtc.model(net,type="regression",regressor=list(coefficient="shared",variable="x",control="T01"),
              likelihood="normal",link="identity",linearModel="fixed")
t0<-Sys.time(); s<-summary(mtc.run(gm,n.adapt=5000,n.iter=20000,thin=2)); el<-as.numeric(Sys.time()-t0,units="secs")
cat(sprintf("gemtc FE shared: beta=%.4f  time=%.2f s\n", s$summaries$quantiles["B","50%"]/gm$regressor$scale, el))
