#!/usr/bin/env Rscript
# Example 13 — REAL genetic data: asthma case-control study across 8 European
# countries (SNPassoc::asthma, ECRHS; 1 551 complete cases), SNP rs184448 (the
# strongest adjusted association among the 51 genotyped SNPs).
#
# Designed data-sharing scenario on real data:
#   genotypes (IPD):   Sweden, UK, Spain
#   count tables:      Australia, Switzerland, France — cases / totals per genotype
#                      group + covariate summaries per group (age mean/SD, % male,
#                      % smokers)
#   summary statistics: Norway, Germany — adjusted log-OR per allele and its SE
# Model: logit P(asthma) = μ_country + β_g·(minor alleles) + β_male + β_age·(age−45)/10 + β_smoke.
#
# VALIDATION against established tools:
#   (V1) mixed data: springs vs Stan (rstan), same likelihood and priors;
#   (V2) all countries share genotypes: spring equilibrium vs glm() MLE;
#   (V3) all countries share only summary statistics: springs vs metafor
#        fixed-effect inverse-variance meta-analysis; Bayes factors vs Wakefield's
#        ABF (exact for a Gaussian likelihood);
#   (V4) effect-size prior sweep from one fit vs direct refits.
#
# Output: results/13_asthma_real.md, results/13_*.csv

source("examples/vs-multinma/common.R")
suppressPackageStartupMessages({ library(SNPassoc); library(metafor); library(rstan) })
options(mc.cores = 4)
data(asthma, package = "SNPassoc")
SNP <- "rs184448"
d <- subset(asthma, !country %in% c("Belgium", "Estonia"))          # cases only: no baseline
a <- as.integer(d[[SNP]]) - 1L; if (mean(a, na.rm = TRUE) > 1) a <- 2L - a
people <- data.frame(country = as.character(d$country), y = d$casecontrol, g = a,
                     male = as.numeric(d$gender == "Males"), age = (d$age - 45) / 10, smoke = d$smoke)
people <- people[complete.cases(people), ]
role <- c(Sweden = "IPD", UK = "IPD", Spain = "IPD", Australia = "counts", Switzerland = "counts",
          France = "counts", Norway = "summary", Germany = "summary")
people$role <- role[people$country]
info <- sprintf("SNPassoc::asthma (ECRHS), %d complete cases in 8 countries (%d asthma cases), SNP %s coded as minor-allele count; covariates sex, age, smoking.",
                nrow(people), sum(people$y), SNP)
cat(info, "\n")

# ---------------------------------------------------------------- spring units
ctry_mu <- names(role)[role != "summary"]
cov_names <- c("beta_male", "beta_age", "beta_smoke")
pn <- c(sprintf("mu[%s]", ctry_mu), "beta_g", cov_names)
P <- length(pn); jb <- match("beta_g", pn)
row_x <- function(c, g, male, age, smoke) { x <- numeric(P)
  if (!is.na(m <- match(sprintf("mu[%s]", c), pn))) x[m] <- 1
  x[jb] <- g; x[P - 2] <- male; x[P - 1] <- age; x[P] <- smoke; x }
Q <- 64; pts <- (seq_len(Q) - 0.5) / Q
ld <- function(k, mult) (k * mult) %% 1            # low-discrepancy sequences for binary covariates
tables <- do.call(rbind, lapply(split(people[people$role == "counts", ], ~ country + g, drop = TRUE), function(x)
  data.frame(country = x$country[1], g = x$g[1], cases = sum(x$y), n = nrow(x), age_mean = mean(x$age),
             age_sd = if (nrow(x) > 1) sd(x$age) else 0, male = mean(x$male), smoke = mean(x$smoke))))
adj_fit <- function(x) { f <- glm(y ~ g + male + age + smoke, binomial, data = x)
  c(b = coef(f)[["g"]], se = sqrt(vcov(f)["g", "g"])) }
summ <- do.call(rbind, lapply(names(role)[role == "summary"], function(c)
  data.frame(country = c, t(adj_fit(people[people$country == c, ])))))

unit_ipd <- function(x) lapply(seq_len(nrow(x)), function(i)
  list(r = x$y[i], n = 1, q = 1L, x = row_x(x$country[i], x$g[i], x$male[i], x$age[i], x$smoke[i])))
unit_table <- function(t, mode = "averaged") {
  if (mode == "averaged") {
    agq <- t$age_mean + t$age_sd * qnorm(pts)
    mq <- as.numeric(ld(seq_len(Q), 0.6180339887) < t$male); sq <- as.numeric(ld(seq_len(Q), 0.4142135624) < t$smoke)
    X <- t(sapply(seq_len(Q), function(k) row_x(t$country, t$g, mq[k], agq[k], sq[k])))
    list(r = t$cases, n = t$n, q = Q, x = as.vector(t(X)))
  } else if (mode == "at_mean") list(r = t$cases, n = t$n, q = 1L, x = row_x(t$country, t$g, t$male, t$age_mean, t$smoke))
  else list(r = t$cases, n = t$n, q = 1L, x = row_x(t$country, t$g, 0, 0, 0))
}
unit_summary <- function(b, se) { x <- numeric(P); x[jb] <- 1
  list(kind = "arm_normal", y = b, se = se, n = 1, q = 1L, x = x) }
mixed_units <- function(mode = "averaged", drop_beta = FALSE) {
  u <- c(unit_ipd(people[people$role == "IPD", ]),
         lapply(seq_len(nrow(tables)), function(k) unit_table(tables[k, ], mode)),
         lapply(seq_len(nrow(summ)), function(k) unit_summary(summ$b[k], summ$se[k])))
  if (drop_beta) u <- lapply(u, function(v) { X <- matrix(v$x, ncol = P, byrow = TRUE); X[, jb] <- 0
    v$x <- as.vector(t(X)); v })
  u
}
nuis <- list(dist = "normal", loc = 0, scale = 10, df = 1)
run_engine <- function(units, beta_prior = list(dist = "normal", loc = 0, scale = 1, df = 1),
                       nuisance = nuis, pool = TRUE, samples = 4000) {
  inp <- tempfile(fileext = ".json"); out <- tempfile(fileext = ".json")
  pri <- lapply(pn, function(j) if (j == "beta_g") beta_prior else nuisance)
  jsonlite::write_json(list(link = "logit", agd_likelihood = "binomial", par_names = pn, priors = pri,
                            units = units, samples = samples, seed = 1, return_pool = pool),
                       inp, auto_unbox = TRUE, digits = NA)
  t <- system.time(st <- system2(springs:::mlnmr_engine_bin(), c("--input", inp, "--output", out),
                                 stdout = FALSE, stderr = FALSE))[["elapsed"]]
  stopifnot(identical(st, 0L))
  r <- jsonlite::fromJSON(out, simplifyVector = TRUE); r$wall <- t
  if (!is.null(r$pool)) { r$pool$values <- as.matrix(r$pool$values); colnames(r$pool$values) <- pn }
  unlink(c(inp, out)); r
}
psum <- function(r, j = "beta_g") { p <- r$priors$pars[[1]]; i <- match(j, p$name)
  c(mean = p$mean[i], sd = p$sd[i], lo = p$q025[i], hi = p$q975[i]) }
flat <- list(dist = "flat")

# ---------------------------------------------------------------- (V1) mixed data vs Stan
U <- mixed_units()
fit <- run_engine(U)
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
ipd <- people[people$role == "IPD", ]
XI <- do.call(rbind, lapply(unit_ipd(ipd), `[[`, "x"))
XA <- do.call(rbind, lapply(seq_len(nrow(tables)), function(k) matrix(unit_table(tables[k, ])$x, ncol = P, byrow = TRUE)))
sdat <- list(N = nrow(ipd), P = P, y = ipd$y, X = XI, K = nrow(tables), Q = Q, r = tables$cases, n = tables$n,
             XA = XA, S = nrow(summ), b = summ$b, se = summ$se, jb = jb, psd = ifelse(pn == "beta_g", 1, 10))
t_compile <- system.time(sm <- stan_model(model_code = stan_code))[["elapsed"]]
t_stan <- system.time(sf <- sampling(sm, data = sdat, chains = 4, iter = 2000, seed = 1, refresh = 0))[["elapsed"]]
ss <- summary(sf, pars = "th")$summary
sp <- fit$priors$pars[[1]][match(pn, fit$priors$pars[[1]]$name), ]
v1 <- data.frame(parameter = pn, springs = sp$mean, stan = ss[, "mean"], springs_sd = sp$sd, stan_sd = ss[, "sd"],
                 z = (sp$mean - ss[, "mean"]) / ss[, "sd"], sd_ratio = sp$sd / ss[, "sd"], stan_ess = ss[, "n_eff"])
write.csv(v1, file.path(OUT_DIR, "13_v1_springs_vs_stan.csv"), row.names = FALSE)

# ---------------------------------------------------------------- (V2) all genotypes: springs vs glm
pn_all <- pn
all_units <- unit_ipd(people[people$role != "summary", ])     # countries with a μ
fit_all <- run_engine(all_units, beta_prior = flat, nuisance = flat, pool = FALSE)
gl <- glm(y ~ 0 + country + g + male + age + smoke, binomial, data = people[people$role != "summary", ])
gcoef <- coef(gl); names(gcoef) <- sub("^country(.*)$", "mu[\\1]", names(gcoef))
names(gcoef)[names(gcoef) == "g"] <- "beta_g"; names(gcoef)[names(gcoef) %in% c("male", "age", "smoke")] <-
  paste0("beta_", names(gcoef)[names(gcoef) %in% c("male", "age", "smoke")])
v2 <- max(abs(fit_all$mode[match(names(gcoef), fit_all$fixed_names)] - gcoef))
ll_diff <- abs(fit_all$log_lik_mode - as.numeric(logLik(gl)))

# ---------------------------------------------------------------- (V3) all summaries: springs vs metafor + ABF
summ_all <- do.call(rbind, lapply(names(role), function(c) data.frame(country = c, t(adj_fit(people[people$country == c, ])))))
su_units <- lapply(seq_len(nrow(summ_all)), function(k) unit_summary(summ_all$b[k], summ_all$se[k]))
# nuisance parameters carry no data here; N(0, 10²) keeps them proper and leaves β_g untouched
fit_su <- run_engine(su_units, beta_prior = flat, nuisance = nuis, pool = FALSE)
rm <- rma(yi = summ_all$b, sei = summ_all$se, method = "FE")
jbf <- match("beta_g", fit_su$fixed_names)        # Gaussian likelihood: equilibrium = posterior mean, curvature⁻¹ = variance
v3 <- c(springs = fit_su$mode[jbf], metafor = as.numeric(rm$beta),
        springs_sd = sqrt(matrix(unlist(fit_su$cov), length(fit_su$mode), byrow = TRUE)[jbf, jbf]), metafor_se = rm$se)
# Bayes factors on the summary-only model: springs (exact marginal likelihood) vs Wakefield ABF
fit_su1 <- run_engine(su_units, beta_prior = list(dist = "normal", loc = 0, scale = 1, df = 1), nuisance = nuis)
fit_su0 <- run_engine(lapply(su_units, function(v) { v$x[jb] <- 0; v }), nuisance = nuis, pool = FALSE)
Wset <- c(0.1, 0.2, 0.4, 1)
bf_from_pool <- function(f1, f0, ldens) {     # log10 BF10 for a new β prior, from the N(0,1) reference fit
  b <- f1$pool$values[, "beta_g"]; lr <- ldens(b) - dnorm(b, 0, 1, log = TRUE)
  ((f1$log_marginal_unnorm - 0.5 * log(2 * pi)) + log(sum(f1$pool$w * exp(lr - max(lr)))) + max(lr) -
     (f0$log_marginal_unnorm - 0.5 * log(2 * pi))) / log(10)      # normalise the data-free β_g prior in H0
}
abf <- function(bhat, V, W) (0.5 * log(V / (V + W^2)) + 0.5 * (bhat^2 / V) * W^2 / (V + W^2)) / log(10)
v3bf <- data.frame(W = Wset,
  springs_log10_BF = sapply(Wset, function(W) bf_from_pool(fit_su1, fit_su0, function(b) dnorm(b, 0, W, log = TRUE))),
  Wakefield_log10_ABF = sapply(Wset, function(W) abf(as.numeric(rm$beta), rm$se^2, W)))

# ---------------------------------------------------------------- (V4) prior sweep on the mixed model
f0 <- run_engine(mixed_units(drop_beta = TRUE), pool = FALSE)
# H0 keeps β_g as a data-free parameter with the unnormalised N(0,1) reference prior, which
# integrates to sqrt(2π): normalise it so that Z0 is the marginal likelihood under β_g = 0
lZ0 <- f0$log_marginal_unnorm - 0.5 * log(2 * pi)
priors <- list("N(0, 0.1²)" = function(b) dnorm(b, 0, 0.1, log = TRUE), "N(0, 0.2²)" = function(b) dnorm(b, 0, 0.2, log = TRUE),
               "N(0, 0.4²)" = function(b) dnorm(b, 0, 0.4, log = TRUE), "N(0, 1)" = function(b) dnorm(b, 0, 1, log = TRUE),
               "Cauchy(0, 0.2)" = function(b) dcauchy(b, 0, 0.2, log = TRUE))
eng_pr <- list("N(0, 0.1²)" = list(dist = "normal", loc = 0, scale = 0.1, df = 1),
               "N(0, 0.2²)" = list(dist = "normal", loc = 0, scale = 0.2, df = 1),
               "N(0, 0.4²)" = list(dist = "normal", loc = 0, scale = 0.4, df = 1),
               "N(0, 1)" = list(dist = "normal", loc = 0, scale = 1, df = 1),
               "Cauchy(0, 0.2)" = list(dist = "cauchy", loc = 0, scale = 0.2, df = 1))
nconst <- c("N(0, 0.1²)" = -0.5 * log(2 * pi * 0.01), "N(0, 0.2²)" = -0.5 * log(2 * pi * 0.04),
            "N(0, 0.4²)" = -0.5 * log(2 * pi * 0.16), "N(0, 1)" = -0.5 * log(2 * pi), "Cauchy(0, 0.2)" = -log(pi * 0.2))
b <- fit$pool$values[, "beta_g"]; w <- fit$pool$w
t_sweep <- system.time(sw <- do.call(rbind, lapply(names(priors), function(nm) {
  lr <- priors[[nm]](b) - dnorm(b, 0, 1, log = TRUE); ww <- w * exp(lr - max(lr)); s <- sum(ww); ww <- ww / s
  m <- sum(ww * b)
  data.frame(prior = nm, mean = m, sd = sqrt(sum(ww * (b - m)^2)), ESS = round(1 / sum(ww^2)),
             log10_BF = ((fit$log_marginal_unnorm - 0.5 * log(2 * pi)) + log(s) + max(lr) - lZ0) / log(10))
})))[["elapsed"]]
ref <- do.call(rbind, lapply(names(eng_pr), function(nm) { r <- run_engine(U, beta_prior = eng_pr[[nm]], pool = FALSE)
  data.frame(prior = nm, refit_mean = psum(r)[["mean"]], refit_sd = psum(r)[["sd"]],
             refit_log10_BF = (r$log_marginal_unnorm + nconst[[nm]] - lZ0) / log(10)) }))
sw <- merge(sw, ref, by = "prior"); sw <- sw[match(names(priors), sw$prior), ]
write.csv(sw, file.path(OUT_DIR, "13_v4_prior_sweep.csv"), row.names = FALSE)

# ---------------------------------------------------------------- shortcuts (context)
gold <- glm(y ~ 0 + country + g + male + age + smoke, binomial, data = people)
gold_b <- c(coef(gold)[["g"]], sqrt(vcov(gold)["g", "g"]))
f_mean <- run_engine(mixed_units("at_mean"), pool = FALSE); f_crude <- run_engine(mixed_units("crude"), pool = FALSE)
est <- data.frame(analysis = c("gold standard: glm on all 8 countries' genotypes", "**springs, data as available (mixed)**",
                               "Stan, same likelihood", "count tables at mean covariates (INLA-representable)",
                               "count tables crude", "metafor IVW, if all 8 countries published summaries"),
                  estimate = c(gold_b[1], psum(fit)[["mean"]], ss[jb, "mean"], psum(f_mean)[["mean"]], psum(f_crude)[["mean"]], as.numeric(rm$beta)),
                  sd = c(gold_b[2], psum(fit)[["sd"]], ss[jb, "sd"], psum(f_mean)[["sd"]], psum(f_crude)[["sd"]], rm$se))
est$OR <- exp(est$estimate)
write.csv(est, file.path(OUT_DIR, "13_estimates.csv"), row.names = FALSE)

# ---------------------------------------------------------------- report
md <- c("## Example 13 — Real genetic data: asthma, 8 countries (validation)", "", info, "",
  "Data-sharing scenario on the real data: genotypes from Sweden, UK, Spain; genotype-count tables with per-group covariate summaries from Australia, Switzerland, France; adjusted summary statistics only from Norway and Germany. Model: logit P(asthma) = μ_country + β_g·g + β_male + β_age·(age−45)/10 + β_smoke; priors N(0, 10²) on nuisance parameters, N(0, 1) reference on β_g.", "",
  "### Validation against established tools", "",
  "| check | reference | result |", "|---|---|---|",
  sprintf("| V1 mixed data (genotypes + tables + summaries), all %d parameters | Stan (rstan), same likelihood & priors: %.0f s sampling + %.0f s compile, min ESS %.0f | max \\|z\\| **%.3f**, SD ratio %.2f–%.2f; springs %.2f s |",
          P, t_stan, t_compile, min(ss[, "n_eff"]), max(abs(v1$z)), min(v1$sd_ratio), max(v1$sd_ratio), fit$fit_wall_sec),
  sprintf("| V2 all six countries share genotypes, flat priors | `glm()` logistic regression | equilibrium = MLE to **%.1e**; log-likelihoods equal to %.1e |", v2, ll_diff),
  sprintf("| V3 all eight countries share only summary statistics | `metafor::rma(method = \"FE\")` | equilibrium %.8f vs %.8f, SE %.8f vs %.8f |", v3[["springs"]], v3[["metafor"]], v3[["springs_sd"]], v3[["metafor_se"]]),
  sprintf("| V3 Bayes factors, summary-only model | Wakefield's ABF (exact for a Gaussian likelihood) | max \\|Δ log10 BF\\| **%.3f** over W = 0.1–1 |", max(abs(v3bf$springs_log10_BF - v3bf$Wakefield_log10_ABF))),
  sprintf("| V4 effect-size prior sweep (5 priors, one fit, %.0f ms) | direct refits | max \\|Δ mean\\| %.4f, max \\|Δ log10 BF\\| %.3f |",
          1000 * t_sweep, max(abs(sw$mean - sw$refit_mean)), max(abs(sw$log10_BF - sw$refit_log10_BF))), "",
  "V1 parameter by parameter:", "", md_table(v1, 3), "",
  "V3 Bayes factors (summary-only model):", "", md_table(v3bf, 3), "",
  "### Results on the real data", "",
  md_table(est, 3), "",
  "Effect-size prior sweep on the mixed model (from one fit; refits for comparison):", "", md_table(sw, 3), "",
  sprintf("* With a moderate signal (OR ≈ %.2f per allele), the evidence for association depends on the prior: log10 BF from %.2f to %.2f across these priors (BF %.0f to %.0f).",
          exp(psum(fit)[["mean"]]), min(sw$log10_BF), max(sw$log10_BF), 10^min(sw$log10_BF), 10^max(sw$log10_BF)))
writeLines(md, file.path(OUT_DIR, "13_asthma_real.md"))
cat(md, sep = "\n")
