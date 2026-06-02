suppressMessages({library(jsonlite); library(netmeta); library(meta)})
recs<-fromJSON("/tmp/binhtml/data.json",simplifyDataFrame=FALSE); cov<-unlist(fromJSON("/tmp/binhtml/cov.json"))
ab<-do.call(rbind,lapply(recs,function(r)data.frame(study=r$study,treatment=r$treatment,event=r$events,n=r$n)))
pw<-pairwise(treat=treatment,event=event,n=n,studlab=study,data=ab,sm="OR")
nm<-netmeta(pw,reference.group="A",random=TRUE,common=FALSE)
xs<-as.numeric(cov[nm$studlab])
rg<-netmetareg(nm,covar=xs,assumption="common",method.tau="REML")
k<-length(rg$se); b<-abs(as.numeric(coef(rg)[k])); se<-rg$se[k]; ci<-confint(rg)$random
write_json(list(beta=b,se=se,blo=b-1.96*se,bhi=b+1.96*se,
                tau2=rg$tau2,t2lo=ci["tau^2","ci.lb"],t2hi=ci["tau^2","ci.ub"]),
           "/tmp/nmrhtml/nmr_bin.json",auto_unbox=TRUE,digits=6)
cat(sprintf("netmetareg binary: |beta|=%.4f se=%.4f tau2=%.4f CI[%.4f,%.4f]\n",b,se,rg$tau2,ci["tau^2","ci.lb"],ci["tau^2","ci.ub"]))
