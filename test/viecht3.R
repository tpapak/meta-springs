meanA <- c(0,0,0,0,0,0,0,0,0,0)
meanB <- c(0.38, 0.58, -0.90, 0.32, -0.15, -0.29, 1.13, 0.39, 0.45, 0.11)
vABi  <- c(0.599, 0.431, 0.793, 0.599, 0.483, 0.478, 0.054, 0.453, 0.772, 0.216)
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
write.csv(ex1, "viecht3.csv")

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
