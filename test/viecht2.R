meanA <- c(0,0,0,0,0,0,0,0,0,0)
meanB <- c(1.30, 1.94, 0.70, 0.36, 1.31, 0.46, 1.24, 0.71, 0.35, 0.77)
vABi <- c(0.640, 0.421, 0.992, 0.058, 0.756, 0.634, 0.79, 0.596, 0.457, 0.935)
vi <- vABi/2
sdA <- sqrt (vi)
sdB <- sdA
treatmentA <- rep("A", times=10)
treatmentB <- rep("B", times=10)
nA <- rep(1, times=10)
nB <- nA
study <- 1:10
ex1 <- data.frame(meanA,sdA,treatmentA,nA
                 ,meanB,sdB,treatmentB,nB
                 ,study)
ex1
write.csv(ex1, "viecht2.csv")

conts <- ex1
library(meta)
library(metafor)
#mdsREML = metacont(mean.e=conts$meanA, n.e=conts$nA, sd.e=conts$sdA
              #, mean.c=conts$meanB, n.c=conts$nB, sd.c=conts$sdB
              #, studlab=conts$study, sm="MD", method.tau="REML")
mdsREML <- rma(meanB, vABi, verbose=TRUE, digits=5, control=list(stepadj=0.5))
print("REML tau ")
print(mdsREML)
mdsPM = metacont( mean.e=conts$meanA, n.e=conts$nA, sd.e=conts$sdA
              , mean.c=conts$meanB, n.c=conts$nB, sd.c=conts$sdB
              , studlab=conts$study, sm="MD", method.tau="PM")
print("PM")
print(mdsPM$tau2)
mdsDL = metacont( mean.e=conts$meanA, n.e=conts$nA, sd.e=conts$sdA
              , mean.c=conts$meanB, n.c=conts$nB, sd.c=conts$sdB
              , studlab=conts$study, sm="MD", method.tau="DL")
print("DL")
print(mdsDL$tau2)

mdsML = metacont(mean.e=conts$meanA, n.e=conts$nA, sd.e=conts$sdA
              , mean.c=conts$meanB, n.c=conts$nB, sd.c=conts$sdB
              , studlab=conts$study, sm="MD", method.tau="ML")
print("ML tau ")
print(mdsML$tau2)
