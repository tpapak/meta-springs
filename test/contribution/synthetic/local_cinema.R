# Re-fit netmeta on the synthetic data and dump the per-(study,
# comparison) IV weights with multi-arm Lu-Ades correction. Used by
# the per-study redistribution comparison.
#
# The full CINeMA-shape response (`local_cinema_response.json`) is
# produced by piping the payload through `../cinema/backend/R/run_cli.R`,
# which is exactly what the cinema.med.auth.gr Flask backend invokes.
# We don't replicate that conversion logic here.

suppressPackageStartupMessages({
  library(jsonlite)
  library(netmeta)
  library(meta)
})

raw <- jsonlite::read_json("test/contribution/synthetic/payload.json",
                           simplifyVector = TRUE)
indata <- raw$indata
indata$id <- as.integer(indata$id)
indata$r  <- as.integer(indata$r)
indata$n  <- as.integer(indata$n)

D <- indata
Dpairs <- pairwise(treat = t, event = r, n = n,
                   data = D, studlab = id, sm = raw$sm,
                   allstudies = TRUE)
nma <- netmeta(TE, seTE, treat1, treat2, studlab,
               data = Dpairs, sm = raw$sm,
               common = TRUE, random = TRUE)

seTE_adj <- {
  if      (!is.null(nma$seTE.adj.random)) nma$seTE.adj.random
  else if (!is.null(nma$seTE.adj))        nma$seTE.adj
  else                                    Dpairs$seTE
}
w_random <- 1 / (seTE_adj ^ 2 + nma$tau ^ 2)

norm_pair <- function(t1, t2) paste(sort(c(as.character(t1),
                                           as.character(t2))),
                                    collapse = ":")
recs <- lapply(seq_len(nrow(Dpairs)), function(i) {
  list(study      = as.character(Dpairs$studlab[i]),
       comparison = norm_pair(Dpairs$treat1[i], Dpairs$treat2[i]),
       weight     = w_random[i])
})
writeLines(toJSON(recs, auto_unbox = TRUE, pretty = TRUE),
           "test/contribution/synthetic/iv_weights_netmeta.json")
cat("wrote iv_weights_netmeta.json (", length(recs), "rows, tau =",
    nma$tau, ")\n")
