suppressMessages(library(gemtc)); set.seed(42)
dtrue <- c(A=0,B=0.5,C=1.0,D=1.5,E=0.8); beta<-0.6; se<-0.3
S <- list(
 list(id="S1",x=0,a=c("A","B")), list(id="S2",x=1,a=c("A","B")),
 list(id="S3",x=0,a=c("A","C")), list(id="S4",x=1,a=c("B","C")),
 list(id="S5",x=0,a=c("A","D")), list(id="S6",x=1,a=c("C","D")),
 list(id="S7",x=0,a=c("B","D")), list(id="S8",x=1,a=c("A","B","C")),
 list(id="S9",x=0,a=c("C","E")), list(id="S10",x=1,a=c("D","E")),
 list(id="S11",x=0,a=c("A","E")),list(id="S12",x=1,a=c("B","E")),
 list(id="S13",x=0,a=c("A","C","D")), list(id="S14",x=1,a=c("B","D")))
rows<-list(); xs<-list()
for(s in S){ mu<-rnorm(1,0,0.5); xs[[s$id]]<-s$x
  for(t in s$a){ y<-mu+dtrue[[t]]+beta*s$x*(t!="A")+rnorm(1,0,se)
    rows[[length(rows)+1]]<-data.frame(study=s$id,treatment=t,mean=y,std.err=se,x=s$x,stringsAsFactors=FALSE) } }
ab<-do.call(rbind,rows)
write.csv(ab, "test/nmr_bignet_arms.csv", row.names=FALSE)
studyData<-data.frame(study=names(xs),x=unlist(xs),stringsAsFactors=FALSE)
net<-mtc.network(data.ab=ab[,c("study","treatment","mean","std.err")], studies=studyData)
model<-mtc.model(net,type="regression",
  regressor=list(coefficient="shared",variable="x",control="A"),
  likelihood="normal",link="identity",linearModel="fixed")
res<-mtc.run(model,n.adapt=20000,n.iter=80000,thin=2)
st<-summary(res)$summaries$statistics
cat("\n===== gemtc (normal, fixed, shared beta) — 5 treatments, 14 studies =====\n")
print(round(st[,c("Mean","SD")],3))
