#!/usr/bin/env Rscript
# Emit gemtc (matched half-normal(0.5) tau prior) posterior densities + netmetareg
# (REML, common slope; the MODE + CIs) for the HTML overlay.
suppressMessages({library(jsonlite); library(gemtc); library(netmeta); library(meta)})
dens <- function(z) { d<-density(z); list(x=round(d$x,5), y=round(d$y,5)) }

run <- function(ab, cov, tag) {
  ab$std.err <- ab$sd/sqrt(ab$n)
  sd_ <- unique(data.frame(study=ab$study, x=as.numeric(cov[ab$study])))
  net <- mtc.network(data.ab=ab[,c("study","treatment","mean","std.err")], studies=sd_)
  gm  <- mtc.model(net, type="regression",
                   regressor=list(coefficient="shared", variable="x", control="A"),
                   likelihood="normal", link="identity", linearModel="random",
                   hy.prior=mtc.hy.prior("std.dev","dhnorm", 0, 1/0.5^2))
  M <- as.matrix(mtc.run(gm, n.adapt=10000, n.iter=60000, thin=4)$samples)
  sc<-gm$regressor$scale; bs<-M[,"B"]/sc; ts<-M[,"sd.d"]^2
  write_json(list(beta=list(mean=mean(bs),sd=sd(bs),lo=quantile(bs,.025),hi=quantile(bs,.975),dens=dens(bs)),
                  tau2=list(median=median(ts),mean=mean(ts),lo=quantile(ts,.025),hi=quantile(ts,.975),dens=dens(ts))),
             sprintf("/tmp/nmrhtml/gemtc_%s.json",tag), auto_unbox=TRUE, digits=6)
  pw<-pairwise(treat=treatment,mean=mean,sd=sd,n=n,studlab=study,data=ab,sm="MD")
  nm<-netmeta(pw,reference.group="A",random=TRUE,common=FALSE)
  xs<-as.numeric(cov[nm$studlab]); rg<-netmetareg(nm,covar=xs,assumption="common",method.tau="REML")
  k<-length(rg$se); b<-abs(as.numeric(coef(rg)[k])); se<-rg$se[k]; ci<-confint(rg)$random
  write_json(list(beta=b,se=se,blo=b-1.96*se,bhi=b+1.96*se,
                  tau2=rg$tau2,t2lo=ci["tau^2","ci.lb"],t2hi=ci["tau^2","ci.ub"]),
             sprintf("/tmp/nmrhtml/nmr_%s.json",tag), auto_unbox=TRUE, digits=6)
  cat(sprintf("[%s] gemtc beta=%.3f tau2med=%.4f | netmetareg |beta|=%.3f tau2=%.4f\n",
              tag,mean(bs),median(ts),b,rg$tau2))
}
ab5<-fromJSON("/tmp/nmr5/data.json"); run(ab5, unlist(fromJSON("/tmp/nmr5/cov.json")),"5t")
j<-fromJSON("/tmp/nmrgc/rep_001.json")
ab3<-do.call(rbind,lapply(1:nrow(j),function(i){r<-j[i,];rbind(
 data.frame(study=r$study,treatment=r$treatmentA,mean=r$meanA,sd=r$sdA,n=r$nA),
 data.frame(study=r$study,treatment=r$treatmentB,mean=r$meanB,sd=r$sdB,n=r$nB))}))
run(ab3, unlist(fromJSON("/tmp/nmrgc/cov.json")),"3t")
