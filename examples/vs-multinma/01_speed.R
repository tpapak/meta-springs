#!/usr/bin/env Rscript
# Example 1 — SPEED at equal accuracy.
#
# Same random-effects NMA, same data, matched priors (HalfNormal(tau;1)),
# fitted by springs (deterministic GC posterior, iid draws) and by multinma
# (Stan NUTS, 4 chains x 2000 iterations, chains in parallel on 4 cores).
#
# Networks: four multinma-bundled datasets + the three largest binary
# networks of the nmadb collection (95-111 studies).
#
# Output: results/01_speed.csv, results/01_agreement.csv, results/01_speed.md

source("examples/vs-multinma/common.R")

nets <- list(
  list(name = "smoking (multinma)", ref = "No intervention",
       d = multinma::smoking, study = "studyn", trt = "trtc", r = "r", n = "n"),
  list(name = "thrombolytics (multinma)", ref = "SK",
       d = multinma::thrombolytics, study = "studyn", trt = "trtc", r = "r", n = "n"),
  list(name = "atrial_fibrillation (multinma)", ref = "Placebo/Standard care",
       d = multinma::atrial_fibrillation, study = "studyc", trt = "trtc", r = "r", n = "n"),
  list(name = "parkinsons (multinma)", ref = "1",
       d = transform(multinma::parkinsons, trtn = as.character(trtn)),
       study = "studyn", trt = "trtn", y = "y", se = "se")
)
for (f in c("nmadb_479969", "nmadb_501194", "nmadb_501232")) {
  d <- read_nmadb(file.path(REPO, "test", "nmadb", paste0(f, ".json")))
  nets[[length(nets) + 1]] <- list(name = f, ref = sort(unique(d$treatment))[1],
                                   d = d, study = "study", trt = "treatment",
                                   r = "events", n = "n")
}

speed <- list(); agree <- list()
for (nt in nets) {
  cat("==", nt$name, "\n")
  binary <- !is.null(nt[["r"]])
  ab <- if (binary) to_springs_ab(nt[["d"]], nt[["study"]], nt[["trt"]], r = nt[["r"]], n = nt[["n"]])
        else        to_springs_ab(nt[["d"]], nt[["study"]], nt[["trt"]], y = nt[["y"]], se = nt[["se"]])

  # multinma network
  mnet <- mn_network(nt)

  sp <- fit_springs(ab, "hn:1")
  mn <- fit_multinma(mnet, "hn:1")
  cmp <- compare_effects(sp, mn, nt$ref)
  cmp$network <- nt$name
  agree[[nt$name]] <- cmp

  min_ess <- min(cmp$multinma_ess)
  speed[[nt$name]] <- data.frame(
    network = nt$name, outcome = if (binary) "binary" else "continuous",
    studies = length(unique(ab$study)), treatments = length(unique(ab$treatment)),
    springs_engine_s = sp$engine, springs_R_s = sp$wall, multinma_s = mn$wall,
    speedup = mn$wall / sp$wall,
    multinma_min_ESS = min_ess,
    springs_ESS_per_s = 4000 / sp$wall,      # iid draws: ESS = n draws
    multinma_ESS_per_s = min_ess / mn$wall,
    max_abs_z = max(abs(cmp$z_diff)),
    sd_ratio_range = sprintf("%.2f-%.2f", min(cmp$sd_ratio), max(cmp$sd_ratio)))
  print(speed[[nt$name]])
}
speed <- do.call(rbind, speed); agree <- do.call(rbind, agree)
write.csv(speed, file.path(OUT_DIR, "01_speed.csv"), row.names = FALSE)
write.csv(agree, file.path(OUT_DIR, "01_agreement.csv"), row.names = FALSE)

writeLines(c(
  "## Example 1 — speed at equal accuracy",
  "",
  sprintf("multinma: %d chains x %d iter (%d warm-up), %d cores in parallel. springs: 4000 iid draws, single thread. Prior HalfNormal(tau; 1), effects/baselines N(0,100^2) (multinma) vs flat (springs).",
          MN_CHAINS, MN_ITER, MN_ITER / 2, MN_CORES),
  "",
  md_table(speed, 2),
  "",
  "`max_abs_z` = largest |springs mean − multinma mean| / multinma posterior SD over all d[·] and tau; `sd_ratio` = springs SD / multinma SD.",
  "",
  "### Per-parameter agreement",
  "",
  md_table(agree[, c("network", "parameter", "springs_mean", "multinma_mean",
                     "springs_sd", "multinma_sd", "z_diff")], 3)
), file.path(OUT_DIR, "01_speed.md"))
cat("wrote", file.path(OUT_DIR, "01_speed.md"), "\n")
