meanA <- c(0,0,0,0,0,0,0,0,0,0)
meanB <- c(-0.47, -1.56, 0.18, 0.88, 0.74, 0.89, -0.05, 0.52, 2.08, 0.81)
vABi <- c(0.663, 0.660, 0.125, 0.068, 0.971, 0.094, 0.509, 0.887, 0.704, 0.556)
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
write.csv(ex1, "viecht1.csv")

conts <- ex1
library(meta)
mdsREML = metacont(mean.e=conts$meanA, n.e=conts$nA, sd.e=conts$sdA
              , mean.c=conts$meanB, n.c=conts$nB, sd.c=conts$sdB
              , studlab=conts$study, sm="MD", method.tau="REML")
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
