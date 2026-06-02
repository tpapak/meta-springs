# Monte-Carlo: continuous-covariate NMR. Does the spring system (relaxation to
# force/torque balance = WLS) match gemtc's continuous meta-regression?
# Per rep: random truth, simulate a 4-treatment network (with loops) where each
# study has its own continuous covariate value; fit BOTH and compare the slope beta.
suppressMessages(library(gemtc)); set.seed(2026)
REPS <- 120
treats <- c("A","B","C","D")
# a connected network with loops: pairs compared, covariate drawn per study
edges <- list(c("A","B"),c("A","C"),c("A","D"),c("B","C"),c("B","D"),c("C","D"),
              c("A","B"),c("A","C"),c("A","D"),c("B","C"),c("B","D"),c("C","D"))
nstud <- length(edges)

# spring fit = damped-Jacobi relaxation of {mu, d, beta} to force/torque balance.
# (Linear-Gaussian, so this equilibrium is the GLS/MLE; we relax it rather than call lm,
#  to show beta emerges mechanically.)
spring_relax <- function(ab, xc){
  S <- length(unique(ab$study)); sid <- match(ab$study, unique(ab$study))
  tcode <- match(ab$treatment, treats) - 1            # 0 = ref A
  w <- 1/(ab$std.err^2); y <- ab$mean
  # diagonal stiffnesses
  Hmu <- tapply(w, sid, sum)
  Hd  <- sapply(1:3, function(t) sum(w[tcode==t]))
  Hb  <- sum(w[tcode>0]*(ab$xc[tcode>0])^2)
  mu <- rep(0,S); d <- c(0,0,0,0); beta <- 0
  for (it in 1:4000){
    pred <- mu[sid] + ifelse(tcode>0, d[tcode+1] + beta*ab$xc, 0)
    r <- pred - y
    gmu <- as.numeric(tapply(w*r, sid, sum))
    gd  <- sapply(1:3, function(t) sum((w*r)[tcode==t]))
    gb  <- sum((w*r*ab$xc)[tcode>0])
    mu <- mu - 0.6*gmu/Hmu
    for (t in 1:3) d[t+1] <- d[t+1] - 0.6*gd[t]/Hd[t]
    beta <- beta - 0.6*gb/Hb
    if (max(abs(c(gmu,gd,gb))) < 1e-9) break
  }
  list(beta=beta, dB=d[2])
}

res <- data.frame()
t0 <- Sys.time()
for (r in 1:REPS) {
  d_t <- c(A=0, B=rnorm(1,0,0.6), C=rnorm(1,0,0.6), D=rnorm(1,0,0.6)); beta <- rnorm(1,0,0.15)
  rows <- list()
  for (i in 1:nstud){ mu <- rnorm(1,0,0.5); x <- runif(1,0,10)
    for (t in edges[[i]]){ val <- mu + d_t[[t]] + beta*x*(t!="A") + rnorm(1,0,0.30)
      rows[[length(rows)+1]] <- data.frame(study=paste0("S",i), treatment=t, mean=val, std.err=0.30, x=x, stringsAsFactors=FALSE) } }
  ab <- do.call(rbind, rows); xbar <- mean(sapply(split(ab$x,ab$study), `[`, 1))
  ab$xc <- ab$x - xbar
  sp <- tryCatch(spring_relax(ab, xbar), error=function(e) NULL); if (is.null(sp)) next
  gg <- tryCatch({
    net <- mtc.network(data.ab=ab[,c("study","treatment","mean","std.err")],
                       studies=data.frame(study=paste0("S",1:nstud), x=sapply(split(ab$x,ab$study),`[`,1)))
    m <- mtc.model(net, type="regression", regressor=list(coefficient="shared",variable="x",control="A"),
                   likelihood="normal", link="identity", linearModel="fixed")
    st <- summary(mtc.run(m, n.adapt=2000, n.iter=10000, thin=1))$summaries$statistics
    st["B","Mean"]/m$regressor$scale     # gemtc standardises the covariate -> back to per-unit
  }, error=function(e) NULL); if (is.null(gg)) next
  res <- rbind(res, data.frame(rep=r, beta_true=beta, beta_spring=sp$beta, beta_gemtc=gg))
  if (nrow(res) %% 10 == 0) cat(sprintf("[%s] %d reps\n", format(Sys.time()), nrow(res)))
}
saveRDS(res, "test/nmr_continuous_sim_results.rds")
d <- res$beta_spring - res$beta_gemtc
cat(sprintf("\n==== CONTINUOUS-COVARIATE SIM (%d reps, %.1f min) ====\n", nrow(res), as.numeric(Sys.time()-t0, units="mins")))
cat(sprintf("beta: mean(spring-gemtc)=%+.4f  SD=%.4f  RMSD=%.4f  cor=%.5f  lm(gemtc~spring): slope=%.3f int=%+.3f\n",
    mean(d), sd(d), sqrt(mean(d^2)), cor(res$beta_spring,res$beta_gemtc), coef(lm(beta_gemtc~beta_spring,res))[2], coef(lm(beta_gemtc~beta_spring,res))[1]))
cat("spring = damped-Jacobi force/torque relaxation; gemtc = Bayesian normal regression (MCMC).\n")
