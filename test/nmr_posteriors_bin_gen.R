suppressMessages({library(jsonlite); library(gemtc)})
dens <- function(z){d<-density(z); list(x=round(d$x,5),y=round(d$y,6))}
set.seed(31)
dB<-0.5; dC<-0.9; beta<-0.25; tau2a<-0.05; nArm<-300L
designs<-c(rep("AB",5),rep("AC",5),rep("BC",4)); k<-length(designs)
sid<-sprintf("S%02d",1:k); x<-round(rnorm(k,0,1.3),3); names(x)<-sid
recs<-list(); ab<-data.frame()
for(i in 1:k){cmp<-designs[i]; t1<-substr(cmp,1,1); t2<-substr(cmp,2,2); mu<-rnorm(1,-0.4,0.3)
  d<-c(A=0,B=dB,C=dC)
  lp<-function(t){u<-rnorm(1,0,sqrt(tau2a)); mu+d[[t]]+(if(t!="A")beta*x[i] else 0)+u}
  e1<-rbinom(1,nArm,plogis(lp(t1))); e2<-rbinom(1,nArm,plogis(lp(t2)))
  recs<-c(recs,list(list(study=sid[i],treatment=t1,events=e1,n=nArm),
                    list(study=sid[i],treatment=t2,events=e2,n=nArm)))
  ab<-rbind(ab,data.frame(study=sid[i],treatment=c(t1,t2),responders=c(e1,e2),sampleSize=nArm))}
write_json(recs,"/tmp/binhtml/data.json",auto_unbox=TRUE)
write_json(as.list(x),"/tmp/binhtml/cov.json",auto_unbox=TRUE,digits=6)
net<-mtc.network(data.ab=ab,studies=data.frame(study=sid,x=as.numeric(x)))
gm<-mtc.model(net,type="regression",regressor=list(coefficient="shared",variable="x",control="A"),
              likelihood="binom",link="logit",linearModel="random",hy.prior=mtc.hy.prior("std.dev","dhnorm",0,1/0.5^2))
M<-as.matrix(mtc.run(gm,n.adapt=8000,n.iter=40000,thin=4)$samples); sc<-gm$regressor$scale
bs<-M[,"B"]/sc; ts<-M[,"sd.d"]^2
write_json(list(beta=list(mean=mean(bs),sd=sd(bs),lo=quantile(bs,.025),hi=quantile(bs,.975),dens=dens(bs)),
                tau2=list(median=median(ts),mean=mean(ts),lo=quantile(ts,.025),hi=quantile(ts,.975),dens=dens(ts))),
           "/tmp/nmrhtml/gemtc_bin.json",auto_unbox=TRUE,digits=6)
cat(sprintf("TRUTH beta=%.2f tau2=%.2f | gemtc beta=%.4f tau2med=%.4f\n",beta,2*tau2a,mean(bs),median(ts)))
