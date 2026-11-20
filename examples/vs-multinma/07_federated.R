#!/usr/bin/env Rscript
# Example 7 — Federated (decentralised, privacy-preserving) ML-NMR with springs.
#
# The plaque psoriasis network of Examples 5–6, split into 4 sites that never
# pool data: UNCOVER-1, -2, -3 (IPD, ~1 300 patients each) and FIXTURE (AgD,
# a published trial). There is no coordinator: sites agree on the model and a
# random seed, and combine numbers only through a secure sum (ring with random
# masks), so no participant ever sees another site's contribution.
#
# Spring reading. Each site's patients are data springs acting on the shared
# parameters θ (baselines, treatment effects, covariate effects). A site can
# report the force and stiffness of its springs at any configuration θ, and
# its stored energy (log-likelihood) at any list of configurations. Forces add,
# stiffnesses add, energies add — so the joint network can be solved from
# sums alone:
#   Round type A (force):  sites send Σ-able (log-lik, force P, stiffness P×P) at
#                          the current θ; Newton relaxation to the joint
#                          equilibrium. Result = the pooled posterior mode and
#                          the "equivalent quadratic spring" (Laplace).
#   Round type B (energy): every site regenerates the SAME N random
#                          configurations from the shared seed (multivariate t
#                          around the equilibrium) and sends N numbers, its
#                          log-likelihood at each; the secure sum gives the
#                          exact joint energy, and importance reweighting gives
#                          the exact posterior (corrects the non-linear springs).
#   Priors:                sites share likelihood, not belief. The prior is
#                          applied after the sum, so each site obtains its OWN
#                          exact posterior under its own prior, locally.
#
# Checks: equilibrium vs the pooled springs fit; posterior vs multinma (pooled,
# Example 5) and vs pooled springs refits under each site's prior.
#
# Output: results/07_federated.md, results/07_*.csv

source("examples/vs-multinma/pso_data.R")

SITE_BIN <- sub("mlnmr$", "mlnmr_site", springs:::mlnmr_engine_bin())
stopifnot(file.exists(SITE_BIN))
work <- file.path(tempdir(), "federated"); dir.create(work, showWarnings = FALSE)
target <- data.frame(durnpso = 2.5, prevsys = 1, bsa = 0.6, weight = 12, psa = 1, row.names = "target")
N_POOL <- 16000L; NU <- 8; SHARED_SEED <- 20261115L

# ---------------------------------------------------------------- the sites
# Shared, public: the model (formula, link, likelihood, parameter list). Each
# site builds the design for its own data. (Covariate centring uses the network
# mean, itself obtained by a secure sum of Σ w·x and Σ w — checked below.)
b <- springs::mlnmr(pso_net, regression = reg, .build_only = TRUE)
P <- length(b$par_names)
site_ids <- unique(b$unit_study)
site_file <- setNames(file.path(work, paste0(make.names(site_ids), ".json")), site_ids)
for (s in site_ids) {
  jsonlite::write_json(list(link = b$link, agd_likelihood = b$likelihood, p = P,
                            units = b$units[b$unit_study == s]),
                       site_file[[s]], auto_unbox = TRUE, digits = NA)
}
site_info <- data.frame(site = site_ids,
  data = sapply(site_ids, function(s) if (s == "FIXTURE") "AgD (published arms)" else "IPD"),
  springs = sapply(site_ids, function(s) sum(b$unit_study == s)),
  patients = sapply(site_ids, function(s) sum(sapply(b$units[b$unit_study == s], `[[`, "n"))),
  row.names = NULL)

# site-side computation: runs on the site's machine, on the site's file only
site_query <- function(s, query) {
  qf <- tempfile(tmpdir = work, fileext = ".json"); of <- tempfile(tmpdir = work, fileext = ".json")
  jsonlite::write_json(query, qf, auto_unbox = TRUE, digits = NA)
  st <- system2(SITE_BIN, c("--data", site_file[[s]], "--query", qf, "--output", of))
  stopifnot(identical(st, 0L))
  r <- jsonlite::fromJSON(of, simplifyVector = TRUE); unlink(c(qf, of))
  if (!is.null(r$ll) && is.null(r$grad)) r$ll[!is.finite(r$ll)] <- -1e12   # impossible configuration: weight 0
  r
}

# ------------------------------------------------- secure sum (no coordinator)
# Ring protocol: site 1 adds a random mask M to its vector and passes it on;
# each site adds its own vector; site 1 removes M at the end. Every site sees
# only masked partial sums. (Production versions use fixed-point modular
# arithmetic and pairwise masks; floating point is enough to show the flow.)
comm <- setNames(numeric(length(site_ids)), site_ids)       # numbers sent per site
seen_by <- list()                                            # what site 2 observed
secure_sum <- function(contrib) {                            # contrib: named list of vectors
  v1 <- contrib[[1]]
  M <- stats::rnorm(length(v1), sd = 1e3 * (1 + stats::median(abs(unlist(contrib)))))
  run <- v1 + M
  for (k in seq_along(contrib)[-1]) {
    if (k == 2) seen_by[[length(seen_by) + 1]] <<- run[1:min(3, length(run))]
    run <- run + contrib[[k]]
  }
  for (s in names(contrib)) comm[[s]] <<- comm[[s]] + length(contrib[[s]])
  run - M
}

# the analysis prior (vignette: N(0, 10²) on everything) — applied AFTER sums
lp_vignette <- function(V) rowSums(stats::dnorm(V, 0, 10, log = TRUE))

# --------------------------------------------- 0. network mean via secure sum
cv <- b$spec$terms$vars
xs <- secure_sum(lapply(setNames(site_ids, site_ids), function(s) {
  r <- b$rows[b$rows$.study == s, ]
  c(colSums(r[, cv] * r$.w), sum(r$.w))
}))
xbar_fed <- xs[seq_along(cv)] / xs[length(xs)]
stopifnot(max(abs(xbar_fed - b$xbar)) < 1e-8)

# ------------------------------------- A. force rounds: relax to equilibrium
t0 <- Sys.time()
theta <- rep(0, P); rounds_A <- 0L; F_old <- -Inf; step_scale <- 1
prior_prec <- rep(1 / 100, P)
repeat {
  rounds_A <- rounds_A + 1L
  f <- secure_sum(lapply(setNames(site_ids, site_ids), function(s) {
    r <- site_query(s, list(mode = "force", theta = theta))
    c(r$ll, r$grad, as.vector(r$hess))
  }))
  ll <- f[1]; g <- f[1 + seq_len(P)]; H <- matrix(f[-(1:(P + 1))], P, P)
  Fv <- ll - 0.5 * sum(prior_prec * theta^2)
  if (Fv < F_old - 1e-9 * abs(F_old)) {          # overshoot: back off (costs a round)
    theta <- theta_old + 0.5 * (theta - theta_old); next
  }
  g <- g - prior_prec * theta; H <- H + diag(prior_prec)
  Hs <- (H + t(H)) / 2
  ch <- tryCatch(chol(Hs), error = function(e) chol(Hs + diag(1e-6 + abs(diag(Hs)) * 1e-3, P)))
  step <- backsolve(ch, forwardsolve(t(ch), g))
  theta_old <- theta; F_old <- Fv
  if (max(abs(step)) < 1e-9 || rounds_A > 60) break
  theta <- theta + step
}
t_A <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
names(theta) <- b$par_names
cov_lap <- chol2inv(ch); dimnames(cov_lap) <- list(b$par_names, b$par_names)

# pooled references
fe <- springs::mlnmr(pso_net, regression = reg)           # all data in one place (springs)
mn <- read.csv(file.path(OUT_DIR, "05_pars_fixed.csv"))   # multinma, pooled (Example 5)
eq_diff <- max(abs(theta - fe$mode[b$par_names]))

# ------------------------------------------ B. one energy round: exact posterior
t0 <- Sys.time()
set.seed(SHARED_SEED)                     # every site regenerates the same points
L <- t(chol(cov_lap))
Z <- matrix(stats::rnorm(N_POOL * P), N_POOL, P)
chi <- stats::rchisq(N_POOL, NU)
V <- sweep((Z %*% t(L)) * sqrt(NU / chi), 2, theta, "+"); colnames(V) <- b$par_names
zz <- rowSums(Z^2) * NU / chi
logq <- -0.5 * (NU + P) * log1p(zz / NU)           # proposal density up to a constant
ll_tot <- secure_sum(lapply(setNames(site_ids, site_ids), function(s)
  site_query(s, list(mode = "energy", thetas = unname(split(V, row(V)))))$ll))
t_B <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

posterior <- function(logprior) {
  lw <- ll_tot + logprior - logq
  w <- exp(lw - max(lw)); w <- w / sum(w)
  list(w = w, ess = 1 / sum(w^2), tab = springs:::.w_tab(V, w))
}
post <- posterior(lp_vignette(V))

# ----------------------------------------------------------- comparisons
key <- mn$parameter                                   # d and beta (multinma)
lap_tab <- data.frame(parameter = b$par_names, mean = theta, sd = sqrt(diag(cov_lap)))
z_vs <- function(tab, ref_mean, ref_sd, pars) {
  m <- match(pars, tab$parameter); (tab$mean[m] - ref_mean) / ref_sd
}
cmp <- data.frame(parameter = key,
  multinma = mn$multinma_mean, multinma_sd = mn$multinma_sd,
  federated_laplace = lap_tab$mean[match(key, lap_tab$parameter)],
  federated_exact = post$tab$mean[match(key, post$tab$parameter)],
  federated_exact_sd = post$tab$sd[match(key, post$tab$parameter)],
  pooled_springs = fe$summary$mean[match(key, fe$summary$parameter)])
cmp$z_laplace <- (cmp$federated_laplace - cmp$multinma) / cmp$multinma_sd
cmp$z_exact <- (cmp$federated_exact - cmp$multinma) / cmp$multinma_sd
cmp$z_exact_vs_pooled <- (cmp$federated_exact - cmp$pooled_springs) /
  fe$summary$sd[match(key, fe$summary$parameter)]
write.csv(cmp, file.path(OUT_DIR, "07_federated_vs_pooled.csv"), row.names = FALSE)

# --------------------------------------- C. every site its own prior, locally
site_prior <- list(
  "UNCOVER-1" = list(label = "vignette: all N(0, 10²)", lp = function(V) lp_vignette(V), args = list()),
  "UNCOVER-2" = list(label = "sceptical effects: d ~ N(0, 1)",
                     lp = function(V) { d <- startsWith(colnames(V), "d[")
                       rowSums(stats::dnorm(V[, !d], 0, 10, log = TRUE)) + rowSums(stats::dnorm(V[, d], 0, 1, log = TRUE)) },
                     args = list(prior_trt = 1)),
  "UNCOVER-3" = list(label = "sceptical modifiers: β ~ N(0, 0.5²)",
                     lp = function(V) { bb <- startsWith(colnames(V), "beta[")
                       rowSums(stats::dnorm(V[, !bb], 0, 10, log = TRUE)) + rowSums(stats::dnorm(V[, bb], 0, 0.5, log = TRUE)) },
                     args = list(prior_reg = 0.5)),
  "FIXTURE"   = list(label = "heavy-tailed modifiers: β ~ Cauchy(0, 0.5)",
                     lp = function(V) { bb <- startsWith(colnames(V), "beta[")
                       rowSums(stats::dnorm(V[, !bb], 0, 10, log = TRUE)) +
                         rowSums(stats::dt(V[, bb] / 0.5, 1, log = TRUE) - log(0.5)) },
                     args = list(prior_reg = multinma::cauchy(0, 0.5))))
show <- c("d[IXE_Q2W]", "d[SEC_300]", "d[ETN]")
own <- do.call(rbind, lapply(names(site_prior), function(s) {
  sp <- site_prior[[s]]
  t1 <- Sys.time(); po <- posterior(sp$lp(V)); t_loc <- as.numeric(difftime(Sys.time(), t1, units = "secs"))
  idx <- sample.int(N_POOL, 4000, replace = TRUE, prob = po$w)
  ff <- list(spec = b$spec, draws = V[idx, , drop = FALSE])
  re <- springs::mlnmr_relative_effects(ff, newdata = target)
  tab <- rbind(po$tab[po$tab$parameter %in% show, c("parameter", "mean", "sd")],
               re[, c("parameter", "mean", "sd")])
  # check: a pooled springs fit with all data and this prior
  ref <- do.call(springs::mlnmr, c(list(pso_net, regression = reg), sp$args))
  ref_re <- springs::mlnmr_relative_effects(ref, newdata = target)
  rt <- rbind(ref$summary[, c("parameter", "mean", "sd")], ref_re[, c("parameter", "mean", "sd")])
  m <- match(tab$parameter, rt$parameter)
  v <- setNames(sprintf("%s (%s)", fmt(tab$mean, 2), fmt(tab$sd, 2)), tab$parameter)
  data.frame(site = s, prior = sp$label, ESS = round(po$ess),
             `d IXE Q2W` = v[["d[IXE_Q2W]"]], `d SEC 300` = v[["d[SEC_300]"]], `d ETN` = v[["d[ETN]"]],
             `target: IXE Q2W` = v[["d[target: IXE_Q2W]"]], `target: ETN` = v[["d[target: ETN]"]],
             `max |z| vs pooled fit, same prior` = fmt(max(abs(tab$mean - rt$mean[m]) / rt$sd[m]), 3),
             `local s` = fmt(t_loc, 3), check.names = FALSE)
}))
write.csv(own, file.path(OUT_DIR, "07_own_priors.csv"), row.names = FALSE)

# ----------------------------------------------------------------- report
site_info$`numbers sent` <- comm[site_info$site]
site_info$`patient records sent` <- 0L
md <- c("## Example 7 — Federated ML-NMR: no pooled data, no coordinator", "",
  "The psoriasis ML-NMR (Examples 5–6) with each study as a separate site that never shares patient data. Sites agree on the model and a random seed and exchange numbers only through a ring **secure sum** (random masks), so nobody — not even another site — sees a single site's contribution. Every site evaluates only its own data springs (`gc-rust/src/bin/mlnmr_site.rs`).", "",
  md_table(site_info), "",
  "### Protocol", "",
  sprintf("0. **Network mean of the covariates** (for centring): one secure sum of Σw·x and Σw. Equal to the pooled value to %.0e.", max(abs(xbar_fed - b$xbar))),
  sprintf("1. **Force rounds — relax the joint network.** At the current configuration θ each site sends its log-likelihood, force (%d numbers) and stiffness (%d × %d); the sums are the joint network's force and stiffness, and a Newton step moves θ. **%d rounds** (%.1f s) reach the equilibrium; it equals the pooled springs fit to **%.1e**. The summed stiffness is each site's *equivalent spring*: exact for linear-Gaussian models, a quadratic (Laplace) approximation for non-linear ones.", P, P, P, rounds_A, t_A, eq_diff),
  sprintf("2. **One energy round — exact posterior.** Every site regenerates the same %d random configurations from the shared seed (multivariate t around the equilibrium) and returns one number per configuration: its log-likelihood there. The secure sum is the exact joint energy; reweighting gives the exact posterior (ESS %.0f; %.1f s).", N_POOL, post$ess, t_B),
  "3. **Priors last.** The prior multiplies the weights after the sum, so each site applies its own.", "",
  "### Federated = pooled", "",
  sprintf("* Against **multinma on the pooled data** (Example 5; d and β, %d quantities): equivalent quadratic springs only (after step 1): max |z| = **%.2f**; after the energy round: max |z| = **%.3f**, SD ratio %.2f–%.2f.",
          nrow(cmp), max(abs(cmp$z_laplace)), max(abs(cmp$z_exact)),
          min(cmp$federated_exact_sd / cmp$multinma_sd), max(cmp$federated_exact_sd / cmp$multinma_sd)),
  sprintf("* Against the **pooled springs fit** (all data in one place): max |z| = %.3f.", max(abs(cmp$z_exact_vs_pooled))),
  "* The step from 1 to 2 is the non-linearity of the binary springs (rare placebo responses, probit tail): a site's patients cannot be replaced *exactly* by a quadratic spring, but one round of energy evaluations restores exactness.", "",
  "### Every site its own posterior (no further communication)", "",
  "Each site reweights the same shared energies with its own prior. Checked against a pooled springs fit under the same prior. `target`: effect vs placebo in the transport population of Example 6 (25 y disease, 60% BSA, 120 kg, PsA, prior systemic).", "",
  md_table(own), "",
  "### What leaves a site", "",
  sprintf("Force rounds: (1 + P + P²) = %d numbers per round; energy round: %d numbers (log-likelihoods at shared random configurations). No patient record, covariate value or per-patient quantity. What site 2 saw of the first message passed to it (masked partial sums): %s.",
          1 + P + P^2, N_POOL, paste(format(signif(seen_by[[2]], 4)), collapse = ", ")),
  "", "Scope: fixed effects. Random effects add study-specific δ springs owned by each site and the τ² grid (one force/energy round per grid point, all in parallel); not run here.")
writeLines(md, file.path(OUT_DIR, "07_federated.md"))
cat(md, sep = "\n")
