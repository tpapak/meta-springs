#!/usr/bin/env Rscript
# Verify the effect sign convention on a single dataset
library(jsonlite)
sp <- fromJSON("test/synth_bias/spring_results.json", simplifyDataFrame=FALSE)
mf <- fromJSON("test/synth_bias/manifest.json", simplifyDataFrame=FALSE)

# Look at first T=4, tau2=0.3 dataset
for (i in seq_along(sp)) {
  m <- mf[[which(sapply(mf, function(x) x$name == sp[[i]]$name))]]
  if (m$treats == 4 && abs(m$tau2 - 0.3) < 0.01) {
    cat(sprintf("Dataset: %s\n", sp[[i]]$name))
    cat(sprintf("True d_true: %s\n", paste(round(m$d_true, 3), collapse=", ")))
    cat("Spring effects:\n")
    for (e in sp[[i]]$effects) {
      cat(sprintf("  tid=%d  est=%+.3f  sd=%.3f  true=%+.3f\n",
          e$tid, e$est, sqrt(e$var),
          m$d_true[e$tid] - m$d_true[1]))
    }
    break
  }
}
