suppressMessages({library(jsonlite); library(gemtc)})
set.seed(23)
d<-c(A=0,B=0.5,C=0.9,D=1.2,E=0.7); beta<-0.25; tau2c<-0.10; tau2a<-tau2c/2; nArm<-300L
designs<-list(c("A","B"),c("A","B"),c("A","C"),c("A","C"),c("A","D"),c("A","D"),c("A","E"),c("A","E"),
              c("A","B","C"),c("A","C","D"),c("A","D","E"),c("A","B","E"),
              c("B","C"),c("C","D"),c("D","E"),c("B","D"))
k<-length(designs); sid<-sprintf("S%02d",1:k); x<-round(rnorm(k,0,1.3),3); names(x)<-sid
ab<-data.frame(); recs<-list()
for(i in 1:k){arms<-designs[[i]]; mu<-rnorm(1,-0.3,0.3)
  rows<-lapply(arms,function(t){u<-rnorm(1,0,sqrt(tau2a)); lp<-mu+d[[t]]+(if(t!="A")beta*x[i] else 0)+u
    e<-rbinom(1,nArm,plogis(lp)); data.frame(study=sid[i],treatment=t,responders=e,sampleSize=nArm)})
  df<-do.call(rbind,rows); ab<-rbind(ab,df)
  recs<-c(recs,lapply(1:nrow(df),function(j) list(study=df$study[j],treatment=df$treatment[j],events=df$responders[j],n=df$sampleSize[j])))}
write_json(recs,"/tmp/binnmr/data5.json",auto_unbox=TRUE); write_json(as.list(x),"/tmp/binnmr/cov5.json",auto_unbox=TRUE,digits=6)
net<-mtc.network(data.ab=ab,studies=data.frame(study=sid,x=as.numeric(x)))
fitR<-function(coef){gm<-mtc.model(net,type="regression",regressor=list(coefficient=coef,variable="x",control="A"),
   likelihood="binom",link="logit",linearModel="random",hy.prior=mtc.hy.prior("std.dev","dhnorm",0,1/0.5^2))
   M<-as.matrix(mtc.run(gm,n.adapt=6000,n.iter=30000,thin=2)$samples); sc<-gm$regressor$scale; list(M=M,sc=sc)}
r<-fitR("shared"); cat(sprintf("TRUTH beta=%.2f tau2=%.2f\n",beta,tau2c))
cat(sprintf("gemtc RANDOM shared: beta=%.4f tau2=%.4f A->B=%.4f A->C=%.4f\n",
   median(r$M[,"B"])/r$sc, median(r$M[,"sd.d"])^2, median(r$M[,"d.A.B"]), median(r$M[,"d.A.C"])))
e<-fitR("exchangeable"); M<-e$M; sc<-e$sc
cat(sprintf("gemtc EXCH: B=%.4f tau2=%.4f sigmaB2(mean)=%.4f beta=[%.3f,%.3f,%.3f,%.3f]\n",
   median(M[,"B"])/sc, median(M[,"sd.d"])^2, mean((M[,"reg.sd"]/sc)^2),
   median(M[,"beta[2]"])/sc,median(M[,"beta[3]"])/sc,median(M[,"beta[4]"])/sc,median(M[,"beta[5]"])/sc))
