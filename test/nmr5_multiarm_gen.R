#!/usr/bin/env Rscript
# 5-treatment NMA with multi-arm studies + a shared covariate slope. Generative
# model = exact spring RE (per-arm u ~ N(0, tau2/2); shared beta on non-ref arms).
suppressPackageStartupMessages(library(jsonlite))
dir.create("/tmp/nmr5", showWarnings=FALSE, recursive=TRUE)
d <- c(A=0, B=0.40, C=0.80, D=1.10, E=0.60); beta<-0.30
tau2c<-0.08; tau2a<-tau2c/2; sigma<-1; nArm<-80L
designs <- list(c("A","B"),c("A","B"),c("A","C"),c("A","C"),c("A","D"),c("A","D"),
                c("A","E"),c("A","E"),
                c("A","B","C"),c("A","C","D"),c("A","D","E"),c("A","B","E"),
                c("A","B","C","D"),c("A","C","D","E"),
                c("B","C"),c("C","D"),c("D","E"),c("B","E"),c("B","D"))
set.seed(20260602)
k <- length(designs); studyId<-sprintf("S%02d",1:k)
xcov <- round(rnorm(k,0,1.2),3); names(xcov)<-studyId
rows <- list(); ri<-1
for (i in seq_len(k)) {
  arms<-designs[[i]]; mu<-rnorm(1,0,5); x<-xcov[i]
  for (t in arms) {
    u<-rnorm(1,0,sqrt(tau2a))
    shift<- if (t=="A") 0 else beta*x
    m <- mu + d[[t]] + shift + u + rnorm(1,0,sigma/sqrt(nArm))
    rows[[ri]]<-list(study=studyId[i],treatment=t,mean=m,sd=sigma,n=nArm); ri<-ri+1
  }
}
write_json(rows, "/tmp/nmr5/data.json", auto_unbox=TRUE, digits=10)
write_json(as.list(xcov), "/tmp/nmr5/cov.json", auto_unbox=TRUE, digits=6)
write_json(list(d=as.list(d),beta=beta,tau2_contrast=tau2c,k=k,
                narms=sapply(designs,length)), "/tmp/nmr5/truth.json", auto_unbox=TRUE)
cat(sprintf("5-trt multiarm: %d studies (%d multi-arm), truth beta=%.2f tau2=%.3f\n",
            k, sum(sapply(designs,length)>2), beta, tau2c))
