library(devtools)
install(pkg="./metafor", reload=T, build_vignettes = FALSE,force=T,quick = T)
yi <- c(-0.47, -1.56, 0.18, 0.88, 0.74, 0.89, -0.05, 0.52, 2.08, 0.81)
vi2 <- c(0.663, 0.660, 0.125, 0.068, 0.971, 0.094, 0.509, 0.887, 0.704, 0.556)
vi <- vi2 / 2
round(rma(yi, vi, method="HE")$tau2, 5)
res <- rma(yi, vi, verbose=TRUE, digits=5)
tmp <- capture.output(rma(yi, vi, verbose=TRUE, digits=5))
tmp <- tmp[grepl("Iteration", tmp)]
 
tau2s <- as.numeric(sapply(strsplit(tmp, "="), function(x) x[2]))
lls   <- sapply(tau2s, function(x) logLik(rma(yi, vi, tau2=x)))
lls
 
profile(res, xlim=c(0,0.5))
points(tau2s, lls, pch=19, cex=1.5, col="red")

