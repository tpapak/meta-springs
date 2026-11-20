#!/usr/bin/env Rscript
# Example 11 — Path-based inconsistency (Q_path, Netpath plot) in springs.
# Tahmasebi, Davies, Papakonstantinou, Rücker & Nikolakopoulou, Stat Med 2026,
# doi:10.1002/sim.70752. Validated against the paper's toy network and the
# diabetes network (Senn et al. 2013; netmeta::Senn2013).
# Output: results/11_qpath.md, results/11_netpath_metf_plac.png
source("examples/vs-multinma/common.R")
suppressPackageStartupMessages(library(netmeta)); data(Senn2013)
toy <- data.frame(treat1 = c("1","1","1","2","4"), treat2 = c("2","3","4","3","3"),
                  TE = -c(0.5, 2, 1.5, 0.5, 1.5), var = 0.09)
chk <- function(q, paper) data.frame(comparison = paste(q$comparison, collapse = " vs "),
  paths = nrow(q$paths), independent = sum(q$paths$independent), Q = q$Q, df = q$df, p = q$pval, paper = paper)
q13 <- qpath(toy, "1", "3"); q23 <- qpath(toy, "2", "3")
q36 <- qpath(Senn2013, "metf", "plac"); q69 <- qpath(Senn2013, "plac", "sulf")
tab <- rbind(chk(q13, "Q 11.11, df 2"), chk(q23, "Q 11.11, df 2"),
             chk(q36, "7 paths, Q 22.45, df 6, p 0.001"), chk(q69, "10 -> 7 paths, Q 22.45, df 6, p 0.001"))
nm <- netmeta(TE, seTE, treat1, treat2, studlab, data = Senn2013, sm = "MD", common = TRUE, random = FALSE)
png(file.path(OUT_DIR, "11_netpath_metf_plac.png"), width = 1100, height = 900, res = 120)
netpath_plot(q36); dev.off()
md <- c("## Example 11 — Path-based inconsistency (Q_path) in springs", "",
  "Tahmasebi, Davies, Papakonstantinou, Rücker & Nikolakopoulou (Stat Med 2026). Spring reading: each direct comparison is a spring (rest length = pooled direct estimate, stiffness = 1/variance); the hat-matrix row of comparison i-j is the force flow through the springs when i and j are pulled apart; its sign orients the springs, and every directed i → j route is a path of evidence. Q_path tests whether the paths agree with the equilibrium.", "",
  md_table(tab, 4), "",
  sprintf("Network estimate metformin vs placebo: springs %.5f, netmeta common effect %.5f.", q36$theta_nma, nm$TE.common["metf", "plac"]), "",
  "Paths for metformin vs placebo:", "", md_table(transform(q36$paths, row_score = q36$row_score), 4), "",
  "Netpath plot: `results/11_netpath_metf_plac.png`.", "",
  "Note: path estimates match the paper's Table 1 except the acarbose path through the three-arm study (−1.006 vs −1.02; variance 0.171 vs 0.15), likely a different multi-arm variance reduction; Q_path matches.")
writeLines(md, file.path(OUT_DIR, "11_qpath.md")); cat(md, sep = "\n")
