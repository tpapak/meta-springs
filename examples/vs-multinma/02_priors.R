#!/usr/bin/env Rscript
# Example 2 — PRIOR EXAMINATION.
#
# A panel of 15 heterogeneity priors (vague half-normal / half-Cauchy /
# exponential / uniform, and the Turner et al. empirical log-normal priors on
# tau^2). springs evaluates the WHOLE panel from ONE tau^2 partition-function
# grid (each prior is a reweighting). multinma has to refit Stan once per prior
# and cannot express Uniform(0, U) on tau at all.
#
# Anchor to the literature: Turner et al. (2019, Stat Med 38:1321, Table 4,
# approach 1) analysed the smoking network under tau ~ U(0,2) and
# tau^2 ~ LN(-2.75, 1.61^2). We reproduce both columns.
#
# Output: results/02_priors_<net>.csv, results/02_priors.md

source("examples/vs-multinma/common.R")

source("examples/vs-multinma/panel.R")     # `panel`, `label`

nets <- list(
  smoking = list(name = "smoking", ref = "No intervention", d = multinma::smoking,
                 study = "studyn", trt = "trtc", r = "r", n = "n",
                 focus = "Group counselling"),
  thrombolytics = list(name = "thrombolytics", ref = "SK", d = multinma::thrombolytics,
                       study = "studyn", trt = "trtc", r = "r", n = "n",
                       focus = "Acc t-PA"))

md <- c("## Example 2 — prior examination", "")
for (nt in nets) {
  cat("==", nt$name, "\n")
  ab <- sp_ab(nt)

  # ---- springs: the whole panel in ONE engine pass --------------------------
  t_sp <- system.time(
    sens <- springs::mtc.sensitivity(springs::mtc.model(springs::mtc.network(ab)),
                                     priors = panel, n.iter = 4000))["elapsed"]
  eng <- attr(sens, "fit_wall_sec")
  # contrasts vs the network reference (engine reference is lexicographic)
  eng_ref <- sub(":.*", "", sens$comparison[1])
  eff <- function(p, t) {             # posterior mean of d[eng_ref, t]
    if (t == eng_ref) return(0)
    sens$effect[sens$prior == p & sens$comparison == paste0(eng_ref, ":", t)]
  }
  sp_focus <- function(p) eff(p, nt$focus) - eff(p, nt$ref)

  # ---- multinma: one Stan fit per prior --------------------------------------
  mnet <- mn_network(nt)
  rows <- list(); mn_total <- 0
  for (p in panel) {
    s <- sens[sens$prior == p, ][1, ]
    mn <- fit_multinma(mnet, p)
    r <- data.frame(prior = label[[p]],
                    springs_tau_med = sqrt(s$tau2_median),
                    springs_tau_lo = sqrt(s$tau2_lo), springs_tau_hi = sqrt(s$tau2_hi),
                    springs_focus = sp_focus(p),
                    log_BF_RE_vs_FE = s$log_bf_re_fe,
                    multinma_tau_med = NA, multinma_tau_lo = NA, multinma_tau_hi = NA,
                    multinma_focus = NA, multinma_s = NA)
    if (!is.null(mn)) {
      tk <- mn$summ[mn$summ$parameter == "tau", ]
      fk <- mn$summ[mn$summ$parameter == sprintf("d[%s]", nt$focus), ]
      r$multinma_tau_med <- tk$`50%`; r$multinma_tau_lo <- tk$`2.5%`
      r$multinma_tau_hi <- tk$`97.5%`; r$multinma_focus <- fk$mean
      r$multinma_s <- mn$wall; mn_total <- mn_total + mn$wall
    }
    rows[[p]] <- r
    cat(sprintf("  %-40s springs tau %.3f | multinma tau %s (%s s)\n", label[[p]],
                r$springs_tau_med, fmt(r$multinma_tau_med), fmt(r$multinma_s, 1)))
  }
  tab <- do.call(rbind, rows)
  write.csv(tab, file.path(OUT_DIR, sprintf("02_priors_%s.csv", nt$name)), row.names = FALSE)

  n_mn <- sum(!is.na(tab$multinma_s))
  md <- c(md, sprintf("### %s — %d priors", nt$name, length(panel)), "",
    sprintf("* **springs: all %d priors in one pass — %.3f s engine, %.2f s from R.**",
            length(panel), eng, t_sp),
    sprintf("* multinma: %d separate Stan fits — %.1f s total (%.1f s / prior); %d priors (uniform on τ) not expressible.",
            n_mn, mn_total, mn_total / n_mn, length(panel) - n_mn),
    sprintf("* Focus contrast: d[%s vs %s] (log OR, posterior mean).", nt$focus, nt$ref),
    "* `log_BF_RE_vs_FE`: log Bayes factor for random vs fixed effects under each prior, from the same grid (no extra fit; multinma offers no marginal likelihood).",
    "", md_table(tab, 3), "")
}

# ---- Turner et al. 2019 Table 4 (approach 1) reproduction --------------------
turner <- data.frame(
  prior = c("unif:2", "ln:-2.75:1.61"),
  dAB = c("0.48 (−0.29, 1.30)", "0.47 (−0.23, 1.20)"),
  dAC = c("0.84 (0.39, 1.34)", "0.81 (0.41, 1.26)"),
  dAD = c("1.09 (0.26, 2.00)", "1.05 (0.30, 1.87)"),
  tau = c("0.82 (0.55, 1.27)", "0.73 (0.50, 1.09)"))
ab <- sp_ab(nets$smoking)
q3 <- function(x) sprintf("%.2f (%.2f, %.2f)", median(x), quantile(x, .025), quantile(x, .975))
sp_rows <- lapply(turner$prior, function(p) {
  f <- fit_springs(ab, p, n_draws = 20000)
  D <- springs_draws_vs(f$res, "No intervention")
  data.frame(prior = p, dAB = q3(D[, "Self-help"]), dAC = q3(D[, "Individual counselling"]),
             dAD = q3(D[, "Group counselling"]), tau = q3(f$res$sd_d),
             engine_s = f$engine)
})
sp_tab <- do.call(rbind, sp_rows)
both <- rbind(cbind(source = "Turner 2019 (WinBUGS)", turner, engine_s = NA),
              cbind(source = "springs", sp_tab))
both <- both[order(both$prior), ]
write.csv(both, file.path(OUT_DIR, "02_turner2019_table4.csv"), row.names = FALSE)
md <- c(md, "### Reproducing Turner et al. 2019, Table 4 (smoking, approach 1)", "",
        "Posterior median (95% CrI). A = no intervention, B = self-help, C = individual, D = group counselling.",
        "multinma cannot fit the U(0,2) column (no uniform heterogeneity prior).", "",
        md_table(both, 4), "")

writeLines(md, file.path(OUT_DIR, "02_priors.md"))
cat("wrote", file.path(OUT_DIR, "02_priors.md"), "\n")
