#!/usr/bin/env Rscript
# Example 3b — How often does the heterogeneity prior change a conclusion?
#              Exact answer, without Monte Carlo noise.
#
# Example 3 counted "flips" (a basic comparison whose 95% CrI excludes 0 under
# some priors and not others) from 4 000 posterior draws per prior. Interval
# ends then carry Monte Carlo error, so a comparison whose interval sits right
# at 0 can "flip" by chance. In the spring/GC posterior the effects are an
# exact τ²-mixture of Gaussians, so the interval ends can be computed exactly
# (mixture CDF), with no draws. This script:
#   (1) re-runs the sweep (171 networks x 15 priors) and reports flips from
#       exact interval ends;
#   (2) shows the noise: the draw-based count under two different seeds;
#   (3) reports "clear" flips, where the interval end crosses 0 by more than
#       a margin on both sides (robust to any small approximation error);
#   (4) what MCMC would face: the expected flip count with the Monte Carlo
#       error of an MCMC run at the effective sample sizes seen in Example 1.
#
# Output: results/03b_sweep_exact.md, results/03b_sweep_exact.csv

source("examples/vs-multinma/common.R")
source("examples/vs-multinma/panel.R")

files <- list.files(file.path(REPO, "test", "nmadb"), pattern = "^nmadb_\\d+\\.json$", full.names = TRUE)
nets <- lapply(files, read_nmadb); names(nets) <- sub("\\.json$", "", basename(files))
nets <- nets[vapply(nets, function(d) "events" %in% names(d), logical(1))]

run <- function(seed) {
  t0 <- Sys.time()
  out <- do.call(rbind, lapply(names(nets), function(nm) {
    d <- nets[[nm]]
    ab <- data.frame(study = d$study, treatment = d$treatment, responders = d$events, sampleSize = d$n)
    s <- tryCatch(springs::mtc.sensitivity(springs::mtc.model(springs::mtc.network(ab)),
                                           priors = panel, n.iter = 4000, seed = seed),
                  error = function(e) NULL)
    if (is.null(s)) return(NULL)
    s$network <- nm; s
  }))
  attr(out, "sec") <- as.numeric(difftime(Sys.time(), t0, units = "secs")); out
}
s1 <- run(1); s2 <- run(2)
cat(length(unique(s1$network)), "networks\n")
write.csv(s1, file.path(OUT_DIR, "03b_sweep_exact.csv"), row.names = FALSE)

vague <- panel[!startsWith(panel, "ln:")]
# flips: per (network, comparison), significant under some priors but not all
flips <- function(sw, lo, hi, priors = panel, margin = 0) {
  sw <- sw[sw$prior %in% priors, ]
  sdv <- sw$sd_exact
  sig <- (sw[[lo]] > margin * sdv) | (sw[[hi]] < -margin * sdv)          # clearly excludes 0
  ns  <- (sw[[lo]] < -margin * sdv) & (sw[[hi]] > margin * sdv)          # clearly includes 0
  key <- paste(sw$network, sw$comparison)
  f <- tapply(sig, key, any) & tapply(ns, key, any)
  nets_f <- unique(sub(" .*", "", names(f)[f]))
  c(networks = length(nets_f), comparisons = sum(f), of_comparisons = length(f))
}
N <- length(unique(s1$network))
tab <- rbind(
  "draws, seed 1 (as in Example 3)" = flips(s1, "lo", "hi"),
  "draws, seed 2"                   = flips(s2, "lo", "hi"),
  "exact mixture"                   = flips(s1, "lo_exact", "hi_exact"),
  "exact, clear (margin 0.05 SD)"   = flips(s1, "lo_exact", "hi_exact", margin = 0.05),
  "exact, clear (margin 0.10 SD)"   = flips(s1, "lo_exact", "hi_exact", margin = 0.10),
  "exact, vague priors only"        = flips(s1, "lo_exact", "hi_exact", priors = vague),
  "exact, vague only, clear 0.10"   = flips(s1, "lo_exact", "hi_exact", priors = vague, margin = 0.10))
tab <- data.frame(count = rownames(tab), networks = tab[, "networks"],
                  `% of networks` = round(100 * tab[, "networks"] / N, 1),
                  comparisons = tab[, "comparisons"], `of` = tab[, "of_comparisons"],
                  check.names = FALSE, row.names = NULL)

# noise: networks flagged under one seed but not the other
f_of <- function(sw, lo, hi) {
  sig <- sw[[lo]] > 0 | sw[[hi]] < 0; key <- paste(sw$network, sw$comparison)
  f <- tapply(sig, key, function(x) any(x) & !all(x)); unique(sub(" .*", "", names(f)[f]))
}
a <- f_of(s1, "lo", "hi"); b <- f_of(s2, "lo", "hi"); e <- f_of(s1, "lo_exact", "hi_exact")
mc_err <- max(abs(c(s1$lo - s1$lo_exact, s1$hi - s1$hi_exact)) / s1$sd_exact)
med_err <- median(abs(c(s1$lo - s1$lo_exact, s1$hi - s1$hi_exact)) / s1$sd_exact)

# (4) what MCMC would face: perturb exact ends by the quantile MC error at ESS n
# (SE of a 2.5% quantile ≈ sqrt(p(1-p)/n) / φ(1.96) posterior SDs), average over 50 replicates
mc_sim <- function(n_eff, reps = 50) {
  se <- sqrt(0.025 * 0.975 / n_eff) / dnorm(qnorm(0.975))
  set.seed(7)
  v <- replicate(reps, {
    x <- s1
    x$lo_mc <- x$lo_exact + rnorm(nrow(x), 0, se * x$sd_exact)
    x$hi_mc <- x$hi_exact + rnorm(nrow(x), 0, se * x$sd_exact)
    flips(x, "lo_mc", "hi_mc")[["networks"]]
  })
  c(se = se, mean = mean(v), lo = min(v), hi = max(v))
}
sim <- t(sapply(c(4000, 1000, 300, 100), mc_sim))
simtab <- data.frame(`effective draws` = c(4000, 1000, 300, 100),
                     `interval-end error (SDs)` = round(sim[, "se"], 3),
                     `networks flagged, mean` = round(sim[, "mean"], 1),
                     `% of networks` = round(100 * sim[, "mean"] / N, 1),
                     `range over 50 runs` = sprintf("%d–%d", sim[, "lo"], sim[, "hi"]),
                     check.names = FALSE)

md <- c("## Example 3b — How often does the heterogeneity prior change a conclusion? (exact)", "",
  sprintf("%d binary nmadb networks × %d priors (the panel of Example 2). A *flip*: a basic comparison whose 95%% CrI excludes 0 under some priors and includes it under others. springs sweep: %.0f s per run.",
          N, length(panel), attr(s1, "sec")), "",
  "Example 3 computed interval ends from 4 000 draws per prior. Here they are also computed **exactly**, from the τ²-mixture-of-Gaussians posterior (bisection on the mixture CDF): no Monte Carlo error.", "",
  md_table(tab, 1), "",
  sprintf("* **Draw noise is real.** The two seeds flag %d and %d networks; %d networks are flagged by one seed but not the other. Interval ends from 4 000 draws differ from the exact ones by a median %.3f and up to %.2f posterior SDs.",
          length(a), length(b), length(union(setdiff(a, b), setdiff(b, a))), med_err, mc_err),
  sprintf("* **Exact count: %d of %d networks (%.0f%%).** Requiring the interval to cross 0 clearly (by ≥ 0.10 posterior SD on both sides) leaves %d (%.0f%%).",
          tab$networks[3], N, tab$`% of networks`[3], tab$networks[5], tab$`% of networks`[5]),
  "", "### What an MCMC sweep would report", "",
  "The exact interval ends perturbed by the Monte Carlo error of a 2.5%/97.5% quantile at a given effective sample size (Example 1: multinma's minimum bulk ESS was 70–1 600), 50 replicate sweeps each:", "",
  md_table(simtab), "",
  "Monte Carlo error adds spurious flips, and more of them at lower effective sample sizes. MCMC can only shrink this error by sampling longer; the spring posterior removes it.")
writeLines(md, file.path(OUT_DIR, "03b_sweep_exact.md"))
cat(md, sep = "\n")
