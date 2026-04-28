#!/usr/bin/env Rscript
# Per-replica metafor REML on the same JSON files gc-rust just ran.
# Records τ̂² (untruncated) and per-replica wall time. Writes one CSV.

suppressPackageStartupMessages({ library(jsonlite); library(metafor) })

ROOT  <- "test/viechtbauer2005"
CTRL  <- list(tau2.min = -Inf)  # Viechtbauer's "untruncated" stance
conds <- c("nbar20_s2t0.125","nbar20_s2t0.25","nbar20_s2t0.5",
           "nbar40_s2t0.125","nbar40_s2t0.25","nbar40_s2t0.5")

build_pw <- function(arms_df) {
  studies <- sort(unique(arms_df$study))
  do.call(rbind, lapply(studies, function(s) {
    sub <- arms_df[arms_df$study == s, , drop = FALSE]
    c1 <- sub[sub$treatment == 1, , drop = FALSE]
    c2 <- sub[sub$treatment == 2, , drop = FALSE]
    if (nrow(c1) != 1 || nrow(c2) != 1) return(NULL)
    data.frame(yi = c2$mean - c1$mean,
               vi = c1$sd^2 / c1$n + c2$sd^2 / c2$n)
  }))
}

rows <- list()
t0_total <- Sys.time()
for (cond in conds) {
  files <- sort(Sys.glob(file.path(ROOT, "data", cond, "rep_*.json")))
  cat(sprintf("%-22s %4d files\n", cond, length(files))); flush.console()
  for (i in seq_along(files)) {
    arms <- fromJSON(files[i])
    df   <- build_pw(arms)
    if (is.null(df) || any(df$vi <= 0) || any(!is.finite(df$vi))) next
    # untruncated REML — time it
    t1 <- Sys.time()
    fit <- tryCatch(
      suppressWarnings(rma(yi = df$yi, vi = df$vi,
                           method = "REML", control = CTRL)),
      error = function(e) NULL)
    t2 <- Sys.time()
    if (is.null(fit)) next
    rows[[length(rows)+1L]] <- data.frame(
      condition = cond, rep = i,
      tau2_reml_untrunc = unname(fit$tau2),
      wall_sec = as.numeric(difftime(t2, t1, units = "secs"))
    )
  }
}
res <- do.call(rbind, rows)
total <- as.numeric(difftime(Sys.time(), t0_total, units = "secs"))
write.csv(res, file.path(ROOT, "results", "metafor_perrep.csv"), row.names = FALSE)
cat(sprintf("\nwrote %d rows; total wall = %.1fs (%.1f reps/s)\n",
            nrow(res), total, nrow(res)/total))
