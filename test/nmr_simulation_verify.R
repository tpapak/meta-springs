# Monte-Carlo verification: do gemtc (Bayesian binomial NMR) and the binomial
# spring IRLS agree across many simulated binary networks with a covariate?
# Per rep: draw truth, simulate binary arm data on a fixed 4-treatment network
# with closed loops, fit both, record beta (covariate slope) and d.A.B.
suppressMessages(library(gemtc)); set.seed(2026)
REPS <- 120; NARM <- 100
treats <- c("A","B","C","D")
edges <- list(c("A","B"),c("A","B"),c("A","C"),c("A","C"),c("A","D"),c("A","D"),
              c("B","C"),c("B","C"),c("B","D"),c("B","D"),c("C","D"),c("C","D"))
xlev <- c(0,1,0,1,0,1, 0,1,0,1,0,1); nstud <- length(edges)
ddbar <- mean(xlev)

fit_spring <- function(ab){                       # binomial spring IRLS == glm MLE
  ab$xc <- xlev[as.integer(sub("S","",ab$study))] - ddbar
  ab$active <- ab$treatment!="A"
  ab$treatment <- factor(ab$treatment, levels=treats)
  X <- model.matrix(~0+factor(study)+treatment+I(xc*active), data=ab)
  X <- X[,!grepl("treatmentA$",colnames(X)),drop=FALSE]
  co <- coef(glm(cbind(ab$responders, ab$sampleSize-ab$responders) ~ X-1, family=binomial))
  names(co) <- colnames(X)
  c(beta=unname(co["I(xc * active)"]), dAB=unname(co["treatmentB"]))
}

res <- data.frame()
t0 <- Sys.time()
for (r in 1:REPS) {
  d_t <- c(A=0, B=rnorm(1,0,0.5), C=rnorm(1,0,0.5), D=rnorm(1,0,0.5)); beta <- rnorm(1,0,0.4)
  rows <- list()
  for (i in 1:nstud){ mu <- rnorm(1,0,0.6); x <- xlev[i]
    for (t in edges[[i]]){ p <- plogis(mu + d_t[[t]] + beta*x*(t!="A"))
      rows[[length(rows)+1]] <- data.frame(study=paste0("S",i), treatment=t,
        responders=rbinom(1,NARM,p), sampleSize=NARM, stringsAsFactors=FALSE) } }
  ab <- do.call(rbind, rows)
  sp <- tryCatch(fit_spring(ab), error=function(e) NULL); if (is.null(sp)) next
  gg <- tryCatch({
    net <- mtc.network(data.ab=ab, studies=data.frame(study=paste0("S",1:nstud), x=xlev))
    m <- mtc.model(net, type="regression",
                   regressor=list(coefficient="shared", variable="x", control="A"),
                   likelihood="binom", link="logit", linearModel="fixed")
    st <- summary(mtc.run(m, n.adapt=2000, n.iter=10000, thin=1))$summaries$statistics
    c(beta=st["B","Mean"]/m$regressor$scale, dAB=st["d.A.B","Mean"])
  }, error=function(e) NULL); if (is.null(gg)) next
  res <- rbind(res, data.frame(rep=r, beta_true=beta,
    beta_spring=sp["beta"], beta_gemtc=gg["beta"], dAB_spring=sp["dAB"], dAB_gemtc=gg["dAB"]))
  if (nrow(res) %% 10 == 0) cat(sprintf("[%s] %d reps done\n", format(Sys.time()), nrow(res)))
}
saveRDS(res, "test/nmr_sim_results.rds")

summ <- function(a,b,lbl){ d<-a-b
  cat(sprintf("%-7s n=%d  mean(spring-gemtc)=%+.4f  SD=%.4f  RMSD=%.4f  cor=%.5f  lm slope=%.3f int=%+.3f\n",
    lbl, length(d), mean(d), sd(d), sqrt(mean(d^2)), cor(a,b), coef(lm(b~a))[2], coef(lm(b~a))[1])) }
cat(sprintf("\n==== SIMULATION RESULT (%d reps, %.1f min) ====\n", nrow(res), as.numeric(Sys.time()-t0, units="mins")))
summ(res$beta_spring, res$beta_gemtc, "beta")
summ(res$dAB_spring,  res$dAB_gemtc,  "d.A.B")
cat("\nInterpretation: slope~1, intercept~0, cor~1, RMSD ~ MCMC noise + vague-prior\n")
cat("shrinkage (gemtc slightly toward 0) => the two methods estimate the same thing.\n")
