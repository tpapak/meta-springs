# Re-fit netmeta on the same synthetic data CINeMA used and dump the
# per-pairwise-row IV weights as JSON.
#
# Why we need this: my Python `iv_weights.json` uses naive per-pair
# 1/(v + τ²) weights; for the 3-arm study netmeta corrects within-study
# covariance via the Lu-Ades / C-matrix machinery, so its effective
# weights diverge from the naive form. To reproduce netmeta's
# `netcontrib(study = TRUE)` exactly we need its actual weights.

suppressPackageStartupMessages({
  library(netmeta)
  library(meta)
  library(jsonlite)
})

raw <- jsonlite::read_json("test/contribution/synthetic/payload.json",
                           simplifyVector = TRUE)
indata <- raw$indata
indata$id <- as.integer(indata$id)
indata$r  <- as.integer(indata$r)
indata$n  <- as.integer(indata$n)

D <- indata
Dpairs <- pairwise(treat = t, event = r, n = n,
                   data = D, studlab = id, sm = "OR",
                   allstudies = TRUE)

nma <- netmeta(TE, seTE, treat1, treat2, studlab,
               data = Dpairs, sm = "OR",
               common = TRUE, random = TRUE)

tau <- nma$tau
cat(sprintf("netmeta tau = %.6f\n", tau))

# Random-effects IV weight per pairwise row.
# Dpairs holds within-study contrasts; nma$seTE.adj.random is the
# multi-arm-corrected per-row SE (variance includes the C-matrix
# adjustment for multi-arm studies). Falls back to nma$seTE.adj if
# the .random suffix isn't present in this netmeta version.
seTE_adj <- {
  if      (!is.null(nma$seTE.adj.random)) nma$seTE.adj.random
  else if (!is.null(nma$seTE.adj))        nma$seTE.adj
  else                                    Dpairs$seTE
}
w_random <- 1 / (seTE_adj ^ 2 + tau ^ 2)

# Build (study, comparison, weight) records — comparison normalised
# alphabetically so it matches my port's canonical key.
norm_pair <- function(t1, t2) paste(sort(c(as.character(t1), as.character(t2))),
                                    collapse = ":")
recs <- lapply(seq_len(nrow(Dpairs)), function(i) {
  list(
    study      = as.character(Dpairs$studlab[i]),
    comparison = norm_pair(Dpairs$treat1[i], Dpairs$treat2[i]),
    weight     = w_random[i]
  )
})

writeLines(toJSON(recs, auto_unbox = TRUE, pretty = TRUE),
           "test/contribution/synthetic/iv_weights_netmeta.json")
cat(sprintf("wrote %d weight rows\n", length(recs)))
