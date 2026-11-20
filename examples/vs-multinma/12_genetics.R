#!/usr/bin/env Rscript
# Example 12 — Genetic association: individual genotypes + genotype-count tables
#              + summary statistics in one spring network, with the effect-size
#              prior (the contested choice in Bayesian fine-mapping) applied afterwards.
#
# One locus, six case-control cohorts, three data types:
#   IPD       (C1, C2): case status, genotype g (0/1/2), ancestry PC, sex;
#   counts    (C3, C4): cases / totals per genotype group + covariate summaries
#                       per group (PC mean, SD; proportion male) — AgD "arms";
#   summary   (C5, C6): adjusted log-OR per allele and its SE only.
# Population stratification: allele frequency rises with the ancestry PC, and the
# PC raises disease risk, so genotype tables are confounded unless the logistic
# model is averaged over each group's covariate distribution (the AgD spring).
#
# Model: logit P(case) = μ_cohort + β_g g + β_pc PC + β_sex sex.
#   individual -> Bernoulli spring; genotype group -> one spring with the
#   averaged force law p̄ = mean_q logistic(η_q); summary statistic -> a Gaussian
#   spring on β_g.
# (1) springs vs Stan (same likelihood) and vs the gold standard (logistic
#     regression on all individuals, as if every cohort had shared genotypes);
# (2) shortcuts: tables at mean covariates (INLA-representable), crude tables;
# (3) effect-size prior sweep from ONE fit: posterior of β_g and the Bayes factor
#     for association under each prior, vs direct refits and Wakefield's ABF.
#
# Output: results/12_genetics.md, results/12_*.csv

source("examples/vs-multinma/common.R")
set.seed(20261114)
truth <- c(beta_g = log(1.2), beta_pc = 0.6, beta_sex = 0.3)

# ------------------------------------------------------------------ simulate
coh <- data.frame(name = paste0("C", 1:6), type = rep(c("IPD", "counts", "summary"), each = 2),
                  n = c(3000, 3000, 4000, 4000, 5000, 5000), pc_mean = c(-0.5, 0.5, 1.0, -1.0, 0.0, 1.5),
                  mu = c(-1.2, -1.0, -1.1, -1.3, -1.0, -1.2))
people <- do.call(rbind, lapply(seq_len(nrow(coh)), function(i) {
  n <- coh$n[i]; pc <- rnorm(n, coh$pc_mean[i], 1); sex <- rbinom(n, 1, 0.5)
  g <- rbinom(n, 2, plogis(qlogis(0.3) + 0.8 * pc))                    # stratification
  y <- rbinom(n, 1, plogis(coh$mu[i] + truth[["beta_g"]] * g + truth[["beta_pc"]] * pc + truth[["beta_sex"]] * sex))
  data.frame(cohort = coh$name[i], type = coh$type[i], y = y, g = g, pc = pc, sex = sex)
}))
info <- sprintf("%s individuals in 6 cohorts (2 IPD, 2 genotype-count tables, 2 summary statistics); true per-allele log-OR %.3f (OR 1.2), β_pc %.1f, β_sex %.1f; allele frequency logit-linear in the ancestry PC (stratification).",
                format(nrow(people), big.mark = ","), truth[["beta_g"]], truth[["beta_pc"]], truth[["beta_sex"]])
cat(info, "\n")

# ------------------------------------------------------------------ spring units
mu_c <- coh$name[coh$type != "summary"]
pn <- c(sprintf("mu[%s]", mu_c), "beta_g", "beta_pc", "beta_sex")
P <- length(pn); jb <- match("beta_g", pn)
row_x <- function(c, g, pc, sex) { x <- numeric(P); x[match(sprintf("mu[%s]", c), pn)] <- 1
  x[jb] <- g; x[P - 1] <- pc; x[P] <- sex; x }
Q <- 64
tables <- do.call(rbind, lapply(split(people[people$type == "counts", ], ~ cohort + g, drop = TRUE), function(d)
  data.frame(cohort = d$cohort[1], g = d$g[1], cases = sum(d$y), n = nrow(d),
             pc_mean = mean(d$pc), pc_sd = sd(d$pc), male = mean(d$sex))))
summ <- do.call(rbind, lapply(coh$name[coh$type == "summary"], function(c) {
  f <- glm(y ~ g + pc + sex, binomial, data = people[people$cohort == c, ])
  data.frame(cohort = c, b = coef(f)[["g"]], se = sqrt(vcov(f)["g", "g"]))
}))
pts <- (seq_len(Q) - 0.5) / Q
build_units <- function(table_mode = c("averaged", "at_mean", "crude"), drop_beta = FALSE) {
  table_mode <- match.arg(table_mode)
  ipd <- people[people$type == "IPD", ]
  u_ipd <- lapply(seq_len(nrow(ipd)), function(i)
    list(r = ipd$y[i], n = 1, q = 1L, x = row_x(ipd$cohort[i], ipd$g[i], ipd$pc[i], ipd$sex[i])))
  u_tab <- lapply(seq_len(nrow(tables)), function(k) {
    t <- tables[k, ]
    if (table_mode == "averaged") {                     # the averaged force law over the group's covariates
      pcq <- t$pc_mean + t$pc_sd * qnorm(pts)
      sexq <- as.numeric((seq_len(Q) * 0.6180339887) %% 1 < t$male)   # low-discrepancy male share
      X <- t(mapply(function(a, s) row_x(t$cohort, t$g, a, s), pcq, sexq))
      list(r = t$cases, n = t$n, q = Q, x = as.vector(t(X)))
    } else if (table_mode == "at_mean") {               # one linear predictor: η at the mean covariates
      list(r = t$cases, n = t$n, q = 1L, x = row_x(t$cohort, t$g, t$pc_mean, t$male))
    } else {                                            # crude: genotype only, no covariates
      list(r = t$cases, n = t$n, q = 1L, x = row_x(t$cohort, t$g, 0, 0))
    }
  })
  u_sum <- lapply(seq_len(nrow(summ)), function(k) { x <- numeric(P); x[jb] <- 1
    list(kind = "arm_normal", y = summ$b[k], se = summ$se[k], n = 1, q = 1L, x = x) })
  units <- c(u_ipd, u_tab, u_sum)
  if (drop_beta) units <- lapply(units, function(u) {          # H0: β_g = 0
    X <- matrix(u$x, ncol = P, byrow = TRUE); X[, jb] <- 0; u$x <- as.vector(t(X)); u })
  units
}
prior_list <- function(beta_prior) lapply(pn, function(j)
  if (j == "beta_g") beta_prior else list(dist = "normal", loc = 0, scale = 10, df = 1))
run_engine <- function(units, beta_prior = list(dist = "normal", loc = 0, scale = 1, df = 1),
                       samples = 4000, pool = TRUE) {
  inp <- tempfile(fileext = ".json"); out <- tempfile(fileext = ".json")
  jsonlite::write_json(list(link = "logit", agd_likelihood = "binomial", par_names = pn,
                            priors = prior_list(beta_prior), units = units, samples = samples,
                            seed = 1, return_pool = pool), inp, auto_unbox = TRUE, digits = NA)
  t <- system.time(st <- system2(springs:::mlnmr_engine_bin(), c("--input", inp, "--output", out),
                                 stdout = FALSE, stderr = FALSE))[["elapsed"]]
  stopifnot(identical(st, 0L))
  r <- jsonlite::fromJSON(out, simplifyVector = TRUE); r$wall <- t
  if (!is.null(r$pool)) { r$pool$values <- as.matrix(r$pool$values); colnames(r$pool$values) <- pn }
  unlink(c(inp, out)); r
}
bsum <- function(r) { p <- r$priors$pars[[1]]; i <- match("beta_g", p$name)
  c(mean = p$mean[i], sd = p$sd[i], lo = p$q025[i], hi = p$q975[i]) }

# ------------------------------------------------------------------ (1) fits
units_avg <- build_units("averaged")
t_build <- system.time(units_avg <- build_units("averaged"))[["elapsed"]]
fit <- run_engine(units_avg)
gold <- glm(y ~ 0 + cohort + g + pc + sex, binomial, data = people)
gold_b <- c(mean = coef(gold)[["g"]], sd = sqrt(vcov(gold)["g", "g"]))

# Stan, same likelihood
stan_code <- "
data { int N; int P; array[N] int y; matrix[N, P] X; int K; int Q; array[K] int r; array[K] int n;
       matrix[K * Q, P] XA; int S; vector[S] b; vector[S] se; int jb; vector[P] psd; }
parameters { vector[P] th; }
model {
  th ~ normal(0, psd);
  y ~ bernoulli_logit(X * th);
  for (k in 1:K) r[k] ~ binomial(n[k], mean(inv_logit(XA[((k - 1) * Q + 1):(k * Q)] * th)));
  b ~ normal(th[jb], se);
}"
ipd <- people[people$type == "IPD", ]
XI <- t(mapply(row_x, ipd$cohort, ipd$g, ipd$pc, ipd$sex))
XA <- do.call(rbind, lapply(units_avg[nrow(ipd) + seq_len(nrow(tables))], function(u) matrix(u$x, ncol = P, byrow = TRUE)))
sdat <- list(N = nrow(ipd), P = P, y = ipd$y, X = XI, K = nrow(tables), Q = Q, r = tables$cases, n = tables$n,
             XA = XA, S = nrow(summ), b = summ$b, se = summ$se, jb = jb, psd = ifelse(pn == "beta_g", 1, 10))
suppressPackageStartupMessages(library(rstan)); options(mc.cores = 4)
t_compile <- system.time(sm <- stan_model(model_code = stan_code))[["elapsed"]]
t_stan <- system.time(sf <- sampling(sm, data = sdat, chains = 4, iter = 2000, seed = 1, refresh = 0))[["elapsed"]]
ss <- summary(sf, pars = "th")$summary
stan_b <- c(mean = ss[jb, "mean"], sd = ss[jb, "sd"], lo = ss[jb, "2.5%"], hi = ss[jb, "97.5%"], ess = ss[jb, "n_eff"])
sp_b <- bsum(fit)
z_all <- (fit$priors$pars[[1]]$mean - ss[, "mean"]) / ss[, "sd"]

# ------------------------------------------------------------------ (2) shortcuts
f_mean <- run_engine(build_units("at_mean"), pool = FALSE)
f_crude <- run_engine(build_units("crude"), pool = FALSE)
ivw_all <- do.call(rbind, lapply(coh$name, function(c) { f <- glm(y ~ g + pc + sex, binomial, data = people[people$cohort == c, ])
  data.frame(b = coef(f)[["g"]], v = vcov(f)["g", "g"]) }))
ivw <- c(mean = sum(ivw_all$b / ivw_all$v) / sum(1 / ivw_all$v), sd = sqrt(1 / sum(1 / ivw_all$v)))
shortcut <- rbind(
  data.frame(analysis = "gold standard: logistic regression on all individuals", what = "needs every genotype", estimate = gold_b[["mean"]], sd = gold_b[["sd"]]),
  data.frame(analysis = "**springs: IPD + averaged count tables + summary statistics**", what = "uses the data as available", estimate = sp_b[["mean"]], sd = sp_b[["sd"]]),
  data.frame(analysis = "Stan, same likelihood", what = sprintf("%.0f s (+%.0f s compile)", t_stan, t_compile), estimate = stan_b[["mean"]], sd = stan_b[["sd"]]),
  data.frame(analysis = "count tables at their mean covariates", what = "one η per observation (INLA-representable)", estimate = bsum(f_mean)[["mean"]], sd = bsum(f_mean)[["sd"]]),
  data.frame(analysis = "count tables crude (genotype only)", what = "ignores stratification", estimate = bsum(f_crude)[["mean"]], sd = bsum(f_crude)[["sd"]]),
  data.frame(analysis = "IVW of adjusted summaries from all 6 cohorts", what = "needs every cohort to rerun its regression", estimate = ivw[["mean"]], sd = ivw[["sd"]]))
shortcut$`bias vs gold (gold SDs)` <- (shortcut$estimate - gold_b[["mean"]]) / gold_b[["sd"]]
shortcut$`vs correct model (its SDs)` <- (shortcut$estimate - sp_b[["mean"]]) / sp_b[["sd"]]
write.csv(shortcut, file.path(OUT_DIR, "12_genetics_estimates.csv"), row.names = FALSE)

# ------------------------------------------------------------------ (3) prior sweep from one fit
f0 <- run_engine(build_units("averaged", drop_beta = TRUE), pool = FALSE)       # H0: β_g = 0
pool <- fit$pool; bg <- pool$values[, "beta_g"]; w <- pool$w
lZ1_ref <- fit$log_marginal_unnorm - 0.5 * log(2 * pi * 1^2)                    # normalise the N(0,1) β prior
lZ0 <- f0$log_marginal_unnorm - 0.5 * log(2 * pi)   # normalise the data-free N(0,1) β_g prior kept in H0
priors <- list("N(0, 0.05²)" = function(b) dnorm(b, 0, 0.05, log = TRUE),
               "N(0, 0.1²)"  = function(b) dnorm(b, 0, 0.1, log = TRUE),
               "N(0, 0.15²)" = function(b) dnorm(b, 0, 0.15, log = TRUE),
               "N(0, 0.2²)"  = function(b) dnorm(b, 0, 0.2, log = TRUE),
               "N(0, 0.4²)"  = function(b) dnorm(b, 0, 0.4, log = TRUE),
               "N(0, 1)"     = function(b) dnorm(b, 0, 1, log = TRUE),
               "Cauchy(0, 0.1)" = function(b) dcauchy(b, 0, 0.1, log = TRUE))
engine_prior <- list("N(0, 0.05²)" = list(dist = "normal", loc = 0, scale = 0.05, df = 1),
                     "N(0, 0.2²)" = list(dist = "normal", loc = 0, scale = 0.2, df = 1),
                     "Cauchy(0, 0.1)" = list(dist = "cauchy", loc = 0, scale = 0.1, df = 1))
norm_const <- list("N(0, 0.05²)" = -0.5 * log(2 * pi * 0.05^2), "N(0, 0.2²)" = -0.5 * log(2 * pi * 0.2^2),
                   "Cauchy(0, 0.1)" = -log(pi * 0.1))
t_sweep <- system.time(sweep <- do.call(rbind, lapply(names(priors), function(nm) {
  lr <- priors[[nm]](bg) - dnorm(bg, 0, 1, log = TRUE)
  ww <- w * exp(lr - max(lr)); s <- sum(ww); ww <- ww / s
  lZ1 <- lZ1_ref + log(s) + max(lr)                     # Z(new) = Z(ref) · E_ref[π_new/π_ref]
  m <- sum(ww * bg)
  data.frame(prior = nm, mean = m, sd = sqrt(sum(ww * (bg - m)^2)), ESS = 1 / sum(ww^2),
             log10_BF = (lZ1 - lZ0) / log(10))
})))[["elapsed"]]
# checks: direct refits under three priors
chk <- do.call(rbind, lapply(names(engine_prior), function(nm) {
  r <- run_engine(units_avg, beta_prior = engine_prior[[nm]], pool = FALSE)
  data.frame(prior = nm, refit_mean = bsum(r)[["mean"]], refit_sd = bsum(r)[["sd"]],
             refit_log10_BF = (r$log_marginal_unnorm + norm_const[[nm]] - lZ0) / log(10), refit_s = r$wall)
}))
sweep <- merge(sweep, chk, by = "prior", all.x = TRUE)
sweep <- sweep[match(names(priors), sweep$prior), ]
# Wakefield's approximate Bayes factor from the gold-standard estimate (normal priors only)
abf <- function(W) { V <- gold_b[["sd"]]^2; z <- gold_b[["mean"]] / gold_b[["sd"]]
  (0.5 * log(V / (V + W)) + 0.5 * z^2 * W / (V + W)) / log(10) }
sweep$Wakefield_log10_ABF <- sapply(sweep$prior, function(nm) {
  W <- c("N(0, 0.05²)" = 0.05, "N(0, 0.1²)" = 0.1, "N(0, 0.15²)" = 0.15, "N(0, 0.2²)" = 0.2,
         "N(0, 0.4²)" = 0.4, "N(0, 1)" = 1)[nm]
  if (is.na(W)) NA else abf(W^2) })
write.csv(sweep, file.path(OUT_DIR, "12_genetics_prior_sweep.csv"), row.names = FALSE)
LR <- 2 * (fit$log_lik_mode - f0$log_lik_mode)             # likelihood-ratio statistic, H1 vs H0
wald2 <- (gold_b[["mean"]] / gold_b[["sd"]])^2              # what Wakefield's ABF uses
rng <- range(sweep$log10_BF)

# ------------------------------------------------------------------ report
md <- c("## Example 12 — Genetic association: genotypes + count tables + summary statistics", "",
  info, "",
  "Model: logit P(case) = μ_cohort + β_g·g + β_pc·PC + β_sex·sex. Each individual is a Bernoulli spring; each genotype group of a count table is one spring whose force law is the logistic averaged over that group's covariate distribution (64 points); each summary statistic is a Gaussian spring on β_g. Priors N(0, 10²) on nuisance parameters, reference N(0, 1) on β_g.", "",
  "### (1) One model for all three data types", "",
  sprintf("* **springs %.2f s** (engine; importance ESS %.0f) vs **Stan %.0f s** sampling + %.0f s compile (min ESS %.0f). All %d parameters: max |z| springs vs Stan = %.3f.",
          fit$fit_wall_sec, fit$priors$ess[1], t_stan, t_compile, min(ss[, "n_eff"]), P, max(abs(z_all))), "",
  md_table(shortcut, 3), "",
  sprintf("* Combining the data as available recovers the gold standard to %.2f of its SD (some information is lost in tables and summaries: posterior SD %.3f vs %.3f).",
          abs(shortcut$`bias vs gold (gold SDs)`[2]), sp_b[["sd"]], gold_b[["sd"]]),
  sprintf("* Shortcuts: crude count tables are confounded by stratification (%.1f gold SDs off). Tables at their mean covariates — the model INLA can represent — remove most of the confounding but not the aggregation bias: they differ from the correct (averaged) model by %.2f of its SDs. Here the bias is modest because risks are moderate (≈ 20–40%%) and the logistic curve is nearly linear there; it grows with rarer outcomes and wider covariate spread.",
          shortcut$`bias vs gold (gold SDs)`[5], shortcut$`vs correct model (its SDs)`[4]), "",
  "### (2) The effect-size prior, applied afterwards", "",
  sprintf("Bayesian fine-mapping depends on the prior SD W of the allelic effect (e.g. Wakefield's 0.1–0.2), and it is contested. From the **one** fit above, every prior is a reweighting of its %d stored points (%.0f ms for all %d priors), giving the posterior of β_g and the Bayes factor for association (H1 vs H0: β_g = 0; H0 one extra fit, %.2f s). Direct refits under three priors and Wakefield's approximate Bayes factor (normal priors, from the gold-standard estimate) are shown for comparison.",
          nrow(pool$values), 1000 * t_sweep, length(priors), f0$fit_wall_sec), "",
  md_table(sweep, 3), "",
  sprintf("* The Bayes factor for association varies %.0f-fold (%.1f orders of magnitude) across these priors, while the posterior of β_g barely moves except under the tightest prior: the fine-mapping prior debate, from one fit, including a non-normal (Cauchy) prior.", 10^diff(rng), diff(rng)),
  sprintf("* Reweighted and directly refitted posteriors and Bayes factors agree (|Δ log10 BF| ≤ %.3f).", max(abs(sweep$log10_BF - sweep$refit_log10_BF), na.rm = TRUE)),
  sprintf("* Wakefield's ABF is a normal approximation built on the Wald statistic (z² = %.1f here); the spring Bayes factor uses the actual likelihood, whose likelihood-ratio statistic is %.1f. The gap (%.1f in log10) is the Wald-vs-likelihood difference, (z² − LR)/(2 ln 10) = %.1f: in a logistic model with re-estimated nuisance parameters the Wald approximation overstates the evidence. (On a summary-statistics-only model, where the likelihood is Gaussian, spring BFs equal the ABF: Example 13.)",
          wald2, LR, mean(sweep$Wakefield_log10_ABF - sweep$log10_BF, na.rm = TRUE), (wald2 - LR) / (2 * log(10))))
writeLines(md, file.path(OUT_DIR, "12_genetics.md"))
cat(md, sep = "\n")
