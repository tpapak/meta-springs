# Three-way correctness check on WELL-IDENTIFIED continuous-covariate NMR:
#   spring solver (normal-equations equilibrium = WLS/MLE)  vs  gemtc  vs  multinma.
# Both Bayesian packages standardise the covariate (~2*SD); we de-standardise to per-unit.
# Informative design (A-heavy network, strong beta, low noise) so prior shrinkage is small
# and all three should land on the same slope.
suppressMessages({library(gemtc); library(multinma)}); set.seed(2026); options(mc.cores=2)
REPS <- 30
treats <- c("A","B","C","D")
edges <- list(c("A","B"),c("A","C"),c("A","D"),c("A","B"),c("A","C"),c("A","D"),
              c("A","B"),c("A","C"),c("A","D"),c("B","C"),c("B","D"),c("C","D"))  # A-heavy + a few loops
nstud <- length(edges)

# spring equilibrium = WLS, solved via normal equations on [mu_1..S, d_B,d_C,d_D, beta]
spring_beta <- function(ab){
  S <- length(unique(ab$study)); sid <- match(ab$study, unique(ab$study))
  tc <- match(ab$treatment, treats)-1                # 0=A
  xbar <- mean(sapply(split(ab$x,ab$study),`[`,1)); xc <- ab$x - xbar
  P <- S+3+1; iMu<-function(i)i; iD<-function(t)S+t; iB<-S+4
  A <- matrix(0,P,P); b <- numeric(P); w <- 1/ab$std.err^2
  for(k in seq_len(nrow(ab))){ row<-numeric(P); row[iMu(sid[k])]<-1
    if(tc[k]>0){ row[iD(tc[k])]<-1; row[iB]<-xc[k] }
    A <- A + w[k]*(row%o%row); b <- b + w[k]*row*ab$mean[k] }
  solve(A,b)[iB]
}

res <- data.frame(); t0 <- Sys.time()
for (r in 1:REPS) {
  d_t <- c(A=0,B=rnorm(1,0,0.6),C=rnorm(1,0,0.6),D=rnorm(1,0,0.6)); beta <- rnorm(1,0,0.4); rows<-list()
  for(i in 1:nstud){mu<-rnorm(1,0,0.5);x<-runif(1,0,10)
    for(t in edges[[i]]) rows[[length(rows)+1]]<-data.frame(study=paste0("S",i),treatment=t,
      mean=mu+d_t[[t]]+beta*x*(t!="A")+rnorm(1,0,0.10),std.err=0.10,x=x,stringsAsFactors=FALSE)}
  ab<-do.call(rbind,rows); sx<-sapply(split(ab$x,ab$study),`[`,1); sd2<-2*sd(sx)
  bsp <- tryCatch(spring_beta(ab), error=function(e) NA)
  bg <- tryCatch({gnet<-mtc.network(data.ab=ab[,c("study","treatment","mean","std.err")],studies=data.frame(study=paste0("S",1:nstud),x=sx))
    gm<-mtc.model(gnet,type="regression",regressor=list(coefficient="shared",variable="x",control="A"),likelihood="normal",link="identity",linearModel="fixed")
    summary(mtc.run(gm,n.adapt=2000,n.iter=10000,thin=1))$summaries$statistics["B","Mean"]/gm$regressor$scale}, error=function(e) NA)
  bm <- tryCatch({ab$trt<-factor(ab$treatment,levels=treats); ab$cls<-factor("active")
    mnet<-set_agd_arm(ab,study=study,trt=trt,y=mean,se=std.err,trt_class=cls)
    fit<-nma(mnet,trt_effects="fixed",regression=~x,class_interactions="common",
             prior_intercept=normal(0,100),prior_trt=normal(0,100),prior_reg=normal(0,100),iter=1500,chains=2,refresh=0)
    mean(as.matrix(fit)[,"beta[x]"])/sd2}, error=function(e) NA)
  res<-rbind(res,data.frame(rep=r,beta_true=beta,spring=bsp,gemtc=bg,multinma=bm))
  cat(sprintf("rep %2d: true=%+.3f spring=%+.3f gemtc=%+.3f multinma=%+.3f\n",r,beta,bsp,bg,bm))
}
saveRDS(res,"test/nmr_multipkg_results.rds")
ok<-complete.cases(res)
cat(sprintf("\n==== THREE-WAY (continuous covariate, %d reps) ====\n",sum(ok)))
pr<-function(a,b,la,lb){d<-res[[a]][ok]-res[[b]][ok];cat(sprintf("%-18s cor=%+.4f  RMSD=%.4f  lm slope=%+.3f\n",paste0(la," vs ",lb),cor(res[[a]][ok],res[[b]][ok]),sqrt(mean(d^2)),coef(lm(res[[b]][ok]~res[[a]][ok]))[2]))}
pr("spring","gemtc","spring","gemtc"); pr("spring","multinma","spring","multinma"); pr("gemtc","multinma","gemtc","multinma")
cat(sprintf("(elapsed %.1f min; spring = exact WLS/MLE; gemtc & multinma Bayesian, covariate de-standardised)\n",as.numeric(Sys.time()-t0,units="mins")))
