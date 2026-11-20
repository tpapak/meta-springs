#!/usr/bin/env Rscript
# Example 3 — DATABASE-WIDE PRIOR SENSITIVITY (what becomes routine).
#
# Every binary network in nmadb (log-OR scale, n = 171) x the 15-prior panel
# of Example 2. springs: one engine pass per network. multinma: timed on a
# random sample of networks with one prior, then extrapolated to the same
# workload (13 expressible priors x 171 networks); the sample doubles as an
# accuracy check on networks we did not choose.
#
# Question answered: how often does a 95%-CrI conclusion (d excludes 0)
# depend on which heterogeneity prior was used?
#
# Output: results/03_sweep_springs.csv, results/03_sweep_multinma_sample.csv,
#         results/03_sweep.md

source("examples/vs-multinma/common.R")
source("examples/vs-multinma/panel.R")     # `panel`, `label` (shared with Example 2)

set.seed(2026)
files <- list.files(file.path(REPO, "test", "nmadb"), pattern = "^nmadb_\\d+\\.json$",
                    full.names = TRUE)
nets <- lapply(files, read_nmadb)
names(nets) <- sub("\\.json$", "", basename(files))
nets <- nets[vapply(nets, function(d) "events" %in% names(d), logical(1))]
cat(length(nets), "binary networks\n")

# ---- springs: all networks x all priors ---------------------------------------
t0 <- Sys.time(); engine_total <- 0; fails <- character()
res <- list()
for (nm in names(nets)) {
  d <- nets[[nm]]
  ab <- data.frame(study = d$study, treatment = d$treatment,
                   responders = d$events, sampleSize = d$n)
  s <- tryCatch(springs::mtc.sensitivity(springs::mtc.model(springs::mtc.network(ab)),
                                         priors = panel, n.iter = 4000),
                error = function(e) NULL)
  if (is.null(s)) { fails <- c(fails, nm); next }
  engine_total <- engine_total + attr(s, "fit_wall_sec")
  s$network <- nm
  s$studies <- length(unique(d$study)); s$treatments <- length(unique(d$treatment))
  res[[nm]] <- s
}
wall_total <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
sw <- do.call(rbind, res)
write.csv(sw, file.path(OUT_DIR, "03_sweep_springs.csv"), row.names = FALSE)

sw$sig <- sw$lo > 0 | sw$hi < 0
by_cmp <- aggregate(sig ~ network + comparison, sw, function(x) c(any = any(x), all = all(x)))
by_cmp$flip <- by_cmp$sig[, "any"] & !by_cmp$sig[, "all"]
net_flip <- aggregate(flip ~ network, by_cmp, any)
vague <- panel[!startsWith(panel, "ln:")]
swv <- sw[sw$prior %in% vague, ]
by_v <- aggregate(sig ~ network + comparison, swv, function(x) any(x) & !all(x))
net_flip_v <- aggregate(sig ~ network, by_v, any)

# tau^2 posterior median spread across priors, per network
tau_spread <- aggregate(tau2_median ~ network, sw[!duplicated(sw[, c("network", "prior")]), ],
                        function(x) max(sqrt(x)) / max(min(sqrt(x)), 1e-3))
bf <- sw[!duplicated(sw[, c("network", "prior")]) & sw$prior == "hn:0.5", ]

# ---- multinma: timing + accuracy on a random sample ---------------------------
n_sample <- 12
smp <- sample(names(res), n_sample)
mrows <- list()
for (nm in smp) {
  d <- nets[[nm]]
  nt <- list(d = d, study = "study", trt = "treatment", r = "events", n = "n",
             ref = sort(unique(d$treatment))[1])
  sp <- fit_springs(sp_ab(nt), "hn:1")
  mn <- tryCatch(fit_multinma(mn_network(nt), "hn:1"), error = function(e) NULL)
  if (is.null(mn)) next
  cmp <- compare_effects(sp, mn, nt$ref)
  mrows[[nm]] <- data.frame(network = nm, studies = length(unique(d$study)),
    treatments = length(unique(d$treatment)), springs_s = sp$wall, multinma_s = mn$wall,
    min_ESS = min(cmp$multinma_ess), max_abs_z = max(abs(cmp$z_diff)),
    median_abs_z = median(abs(cmp$z_diff)),
    sd_ratio_min = min(cmp$sd_ratio), sd_ratio_max = max(cmp$sd_ratio))
  print(mrows[[nm]])
}
ms <- do.call(rbind, mrows)
write.csv(ms, file.path(OUT_DIR, "03_sweep_multinma_sample.csv"), row.names = FALSE)

n_expr <- sum(!vapply(panel, function(p) is.null(mn_prior(p)), logical(1)))
mn_extrap_h <- mean(ms$multinma_s) * n_expr * length(res) / 3600

md <- c("## Example 3 — prior sensitivity across nmadb (binary networks)", "",
  sprintf("* Networks: %d binary nmadb networks (%d–%d studies, %d–%d treatments); %d failed in springs%s.",
          length(res), min(sw$studies), max(sw$studies), min(sw$treatments), max(sw$treatments),
          length(fails), if (length(fails)) paste0(" (", paste(fails, collapse = ", "), ")") else ""),
  sprintf("* **springs: %d networks × %d priors = %d full posteriors in %.1f s wall from R (%.1f s in the engine).**",
          length(res), length(panel), length(res) * length(panel), wall_total, engine_total),
  sprintf("* multinma, measured on %d random networks (HN(τ;1)): mean %.1f s per fit (median %.1f). Extrapolated to the same workload (%d expressible priors × %d networks): **≈ %.1f hours**.",
          nrow(ms), mean(ms$multinma_s), median(ms$multinma_s), n_expr, length(res), mn_extrap_h),
  "", "### What the sweep shows", "",
  sprintf("* %d / %d networks (%.0f%%) have at least one basic comparison whose 95%% CrI excludes 0 under some priors but not others (all 15 priors).",
          sum(net_flip$flip), nrow(net_flip), 100 * mean(net_flip$flip)),
  sprintf("* Restricting to the 10 vague priors only: %d / %d (%.0f%%).",
          sum(net_flip_v$sig), nrow(net_flip_v), 100 * mean(net_flip_v$sig)),
  sprintf("* %d / %d comparisons in total change significance status across the panel.",
          sum(by_cmp$flip), nrow(by_cmp)),
  sprintf("* Ratio max/min posterior-median τ across priors: median %.2f, 90th percentile %.2f.",
          median(tau_spread$tau2_median), quantile(tau_spread$tau2_median, .9)),
  sprintf("* Bayes factor RE vs FE under HN(τ;0.5): log BF > 1 (favours RE) in %d, < −1 (favours FE) in %d, inconclusive in %d networks.",
          sum(bf$log_bf_re_fe > 1, na.rm = TRUE), sum(bf$log_bf_re_fe < -1, na.rm = TRUE),
          sum(abs(bf$log_bf_re_fe) <= 1, na.rm = TRUE)),
  "", "### multinma sample: accuracy and time on randomly chosen networks", "",
  md_table(ms, 2), "")
writeLines(md, file.path(OUT_DIR, "03_sweep.md"))
cat("wrote", file.path(OUT_DIR, "03_sweep.md"), "\n")
