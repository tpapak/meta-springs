#!/usr/bin/env Rscript
# Verify the 1-loop correction to ln Z in a 1-D toy before wiring it into the
# multivariate spring GC.
#
# Toy: single binomial arm with one Gaussian random effect.
#
#   model:   events ~ Binomial(n, sigmoid(mu0 + delta))
#            delta  ~ Normal(0, tau2)
#
#   marginal: Z(tau2) = integral over delta of
#                exp(-E(delta))
#            with E(delta) = n log(1 + exp(mu0 + delta))
#                          - e (mu0 + delta)
#                          + delta^2 / (2 tau2)
#
#   Gold: adaptive numerical quadrature of exp(-E)
#   Laplace:  -E(hat) + 1/2 log(2 pi) - 1/2 log f''(hat)
#   Corrected: Laplace + [5 (f''')^2 / (24 (f'')^3)] - [f'''' / (8 (f'')^2)]

# --- derivatives of E at a given delta ---
sig <- function(x) 1 / (1 + exp(-x))
E_val <- function(d, mu0, e, n, tau2)
  n * log1p(exp(mu0 + d)) - e * (mu0 + d) + d^2 / (2 * tau2)
E_g   <- function(d, mu0, e, n, tau2) {
  s <- sig(mu0 + d); -e + n*s + d/tau2
}
E_h   <- function(d, mu0, e, n, tau2) {
  s <- sig(mu0 + d); n*s*(1-s) + 1/tau2
}
E_3   <- function(d, mu0, e, n, tau2) {
  s <- sig(mu0 + d); n*s*(1-s)*(1 - 2*s)
}
E_4   <- function(d, mu0, e, n, tau2) {
  s <- sig(mu0 + d); n*s*(1-s)*(1 - 6*s*(1-s))
}

# Damped Newton with Armijo backtracking for reliable convergence.
mode1d <- function(mu0, e, n, tau2, tol = 1e-12, maxit = 200) {
  # Initial guess: unrestricted MLE of log-odds from data, projected back
  # to delta = mle - mu0. Safer than starting at 0.
  p_hat <- (e + 0.5) / (n + 1)
  d     <- log(p_hat / (1 - p_hat)) - mu0
  d     <- sign(d) * min(abs(d), 10)      # cap initial guess
  E_here <- function(dd) E_val(dd, mu0, e, n, tau2)
  for (it in 1:maxit) {
    g <- E_g(d, mu0, e, n, tau2)
    h <- E_h(d, mu0, e, n, tau2)
    dstep <- g / h
    # Armijo: shrink step until E decreases
    alpha <- 1
    E0 <- E_here(d)
    repeat {
      d_new <- d - alpha * dstep
      if (E_here(d_new) <= E0 - 1e-4 * alpha * g * dstep) break
      alpha <- alpha / 2
      if (alpha < 1e-12) break
    }
    d <- d_new
    if (abs(alpha * dstep) < tol) break
  }
  d
}

# Log-Z estimates
logZ_exact <- function(mu0, e, n, tau2) {
  # Log-domain quadrature: factor out E(mode) to avoid over/underflow.
  dhat  <- mode1d(mu0, e, n, tau2)
  Ehat  <- E_val(dhat, mu0, e, n, tau2)
  shat  <- 1 / sqrt(E_h(dhat, mu0, e, n, tau2))
  # integrand = exp(Ehat - E(d)) ; peak = 1 at d = dhat
  f <- function(d) {
    v <- Ehat - (n*log1p(exp(mu0+d)) - e*(mu0+d) + d^2/(2*tau2))
    ifelse(is.finite(v), exp(v), 0)
  }
  lo <- dhat - 40*shat
  hi <- dhat + 40*shat
  Z  <- integrate(f, lo, hi, rel.tol = 1e-10, subdivisions = 2000L)$value
  -Ehat + log(Z)
}

logZ_laplace <- function(mu0, e, n, tau2) {
  dhat <- mode1d(mu0, e, n, tau2)
  f    <- E_val(dhat, mu0, e, n, tau2)
  a    <- E_h (dhat, mu0, e, n, tau2)
  -f + 0.5*log(2*pi) - 0.5*log(a)
}

logZ_corrected <- function(mu0, e, n, tau2) {
  dhat <- mode1d(mu0, e, n, tau2)
  f    <- E_val(dhat, mu0, e, n, tau2)
  a    <- E_h (dhat, mu0, e, n, tau2)
  b    <- E_3 (dhat, mu0, e, n, tau2)
  c    <- E_4 (dhat, mu0, e, n, tau2)
  # 1-loop correction inside the log
  # Z_exact ≈ Z_Laplace * [1 + 5 b^2 / (24 a^3) - c / (8 a^2)]
  corr <- 1 + 5*b^2 / (24*a^3) - c / (8*a^2)
  -f + 0.5*log(2*pi) - 0.5*log(a) + log(corr)
}

# --- Scenarios that stress Laplace ---
# sparse, moderate, and dense events; small, moderate, and large tau2
scenarios <- expand.grid(
  mu0  = c(-3, -1, 0, 1),       # baseline log-odds (affects sigma value at mode)
  e_n  = c(0.02, 0.05, 0.2, 0.5),  # event rate (sparseness)
  n    = c(20, 100),
  tau2 = c(0.01, 0.1, 0.5, 1.5)
)
scenarios$e <- round(scenarios$e_n * scenarios$n)
scenarios   <- scenarios[scenarios$e >= 1 & scenarios$e <= scenarios$n - 1, ]

res <- do.call(rbind, lapply(seq_len(nrow(scenarios)), function(i) {
  s <- scenarios[i, ]
  lz_ex <- tryCatch(logZ_exact(s$mu0, s$e, s$n, s$tau2),
                    error = function(e) NA_real_)
  if (is.na(lz_ex)) return(NULL)
  lz_la <- logZ_laplace  (s$mu0, s$e, s$n, s$tau2)
  lz_co <- logZ_corrected(s$mu0, s$e, s$n, s$tau2)
  data.frame(
    mu0 = s$mu0, e = s$e, n = s$n, tau2 = s$tau2,
    exact      = lz_ex,
    laplace    = lz_la,    err_laplace   = lz_la - lz_ex,
    corrected  = lz_co,    err_corrected = lz_co - lz_ex
  )
}))

cat("\n=== 1D Laplace vs Laplace+correction vs exact numerical ===\n")
res$abs_imp_ratio <- abs(res$err_corrected) / pmax(abs(res$err_laplace), 1e-14)

# Summary
cat(sprintf("\nScenarios: %d\n", nrow(res)))
cat(sprintf("Mean |err| Laplace     : %.3e\n", mean(abs(res$err_laplace))))
cat(sprintf("Mean |err| corrected   : %.3e\n", mean(abs(res$err_corrected))))
cat(sprintf("Max  |err| Laplace     : %.3e\n", max (abs(res$err_laplace))))
cat(sprintf("Max  |err| corrected   : %.3e\n", max (abs(res$err_corrected))))
cat(sprintf("Fraction where corrected beats Laplace: %.2f\n",
            mean(abs(res$err_corrected) < abs(res$err_laplace))))

# Worst-Laplace cases, ranked, show correction effect
cat("\nTop 12 hardest scenarios for Laplace (sorted by |err_laplace|):\n")
ord <- order(-abs(res$err_laplace))
top <- res[ord[1:12],]
print(format(top[, c("mu0","e","n","tau2","err_laplace","err_corrected","abs_imp_ratio")],
             digits = 4, nsmall = 5), row.names = FALSE)

saveRDS(res, "test/gc_compare/verify_correction_1d.rds")
write.csv(res, "test/gc_compare/verify_correction_1d.csv", row.names = FALSE)
cat("\nSaved: test/gc_compare/verify_correction_1d.csv\n")
