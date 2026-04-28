#!/usr/bin/env Rscript
# Run bayesmeta on Crins 2014 under each of several heterogeneity priors,
# matching exactly what gc-rust ships.  Output a JSON with τ and τ²
# posterior summaries for each prior.

suppressPackageStartupMessages({library(bayesmeta); library(metafor); library(jsonlite)})
data("CrinsEtAl2014", package = "bayesmeta")
es <- escalc(measure = "OR", ai = exp.AR.events, n1i = exp.total,
             ci = cont.AR.events, n2i = cont.total, slab = publication,
             data = CrinsEtAl2014)

priors <- list(
  list(label = "Uniform on τ ∈ [0, ∞)",
       fun   = function(t) as.numeric(t >= 0)),                # improper uniform on τ ≥ 0
  list(label = "HalfNormal(τ; σ = 0.5)",
       fun   = function(t) dhalfnormal(t, scale = 0.5)),
  list(label = "HalfNormal(τ; σ = 1.0)",
       fun   = function(t) dhalfnormal(t, scale = 1.0)),
  list(label = "HalfCauchy(τ; σ = 0.5)",
       fun   = function(t) dhalfcauchy(t, scale = 0.5))
)

results <- list()
for (p in priors) {
  cat(sprintf("\n=== %s ===\n", p$label)); flush.console()
  fit <- bayesmeta(es, mu.prior.mean = 0, mu.prior.sd = 4,
                   tau.prior = p$fun, interval.type = "central")
  s <- fit$summary
  # posterior summaries on τ scale (default bayesmeta) and τ² scale
  tau_s <- list(
    mode   = unname(s["mode",   "tau"]),
    median = unname(s["median", "tau"]),
    mean   = unname(s["mean",   "tau"]),
    sd     = unname(s["sd",     "tau"]),
    lo95   = unname(s["95% lower", "tau"]),
    hi95   = unname(s["95% upper", "tau"])
  )
  # τ² summaries: monotone-equivariant median, derived mean = E[τ]² + Var(τ)
  t2_summary <- list(
    median  = tau_s$median^2,
    mean    = tau_s$mean^2 + tau_s$sd^2,
    lo95    = tau_s$lo95^2,
    hi95    = tau_s$hi95^2
  )
  mu_s <- list(
    mode   = unname(s["mode",   "mu"]),
    median = unname(s["median", "mu"]),
    mean   = unname(s["mean",   "mu"]),
    sd     = unname(s["sd",     "mu"]),
    lo95   = unname(s["95% lower", "mu"]),
    hi95   = unname(s["95% upper", "mu"])
  )
  results[[length(results)+1L]] <- list(
    label = p$label, tau = tau_s, tau2 = t2_summary, mu = mu_s
  )
  cat(sprintf("  τ median = %.4f, τ² median = %.4f, μ = %.3f ± %.3f\n",
              tau_s$median, t2_summary$median, mu_s$mean, mu_s$sd))
}

write_json(list(results = results),
           "test/rover2020/bayesmeta_results.json",
           auto_unbox = TRUE, pretty = TRUE)
cat("\nwrote test/rover2020/bayesmeta_results.json\n")
