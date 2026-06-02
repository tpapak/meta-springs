suppressMessages(library(metafor))
yi <- c(0.60,0.70,1.25,1.25,1.75,1.95)   # contrast B-A
vi <- rep(0.08,6)                          # 0.04+0.04
x  <- 0:5
m <- rma(yi, vi, mods=~x, method="REML")
cat(sprintf("metafor REML: intercept=%.5f  beta=%.5f  tau2=%.6f\n",
    coef(m)[1], coef(m)[2], m$tau2))
mml <- rma(yi, vi, mods=~x, method="ML")
cat(sprintf("metafor ML  : intercept=%.5f  beta=%.5f  tau2=%.6f\n",
    coef(mml)[1], coef(mml)[2], mml$tau2))
