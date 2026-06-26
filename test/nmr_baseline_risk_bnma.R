# Baseline-risk network meta-regression reference = the MetaInsight engine.
# MetaInsight does baseline-risk via `bnma` (Achana 2013), NOT gemtc.
# Model (binomial, logit):  logit(p_ik) = eta_i + d_t + b_bl[t]*(eta_i - mx_bl)
#   eta_i  = latent study baseline (control log-odds), independent prior
#   mx_bl  = mean continuity-corrected observed control-arm log-odds (FIXED constant)
#   b_bl   = baseline-risk interaction slope ("common"=shared B), b_bl[ref]=0
# Reported d_t = relative effect at baseline = mx_bl.
# Dataset: bnma::certolizumab (12 placebo-controlled RA studies, ACR50) -- the canonical
# bnma baseline-risk example (vignette line 66).

suppressMessages({ library(bnma); library(jsonlite) })
set.seed(1)
data(certolizumab)
trts <- certolizumab$Treat.order            # Placebo first (reference)

# mx_bl exactly as bnma computes it: mean continuity-corrected control log-odds.
isctrl <- certolizumab$Treat == "Placebo"
rc <- certolizumab$Outcomes[isctrl]; nc <- certolizumab$N[isctrl]
rd <- rc; rd[rc == 0] <- 0.5; nd <- nc; nd[rc == 0] <- nc[rc == 0] + 1
mx_bl <- mean(log((rd/nd) / (1 - rd/nd)))
cat(sprintf("mx_bl (mean control log-odds, cc) = %.6f\n", mx_bl))

run_bnma <- function(type) {
  # vague prec.Eta so baselines are effectively free (matches the free-node spring model)
  net <- with(certolizumab, network.data(
    Outcomes = Outcomes, Study = Study, Treat = Treat, N = N,
    response = "binomial", Treat.order = trts,
    type = type, baseline = "common", baseline.risk = "independent",
    prec.Eta = 1e-4))
  res <- network.run(net, n.chains = 4, max.run = 2000000,
                     setsize = 20000, n.run = 200000, conv.limit = 1.05)
  list(net = net, summary = summary(res))
}

extract <- function(s, type) {
  st <- s$summary.samples$statistics; qt <- s$summary.samples$quantiles
  nm <- rownames(st)
  pick <- function(p) if (p %in% nm)
    list(mean = unname(st[p,"Mean"]), sd = unname(st[p,"SD"]),
         lo = unname(qt[p,"2.5%"]), hi = unname(qt[p,"97.5%"])) else NULL
  ds <- list()
  for (k in 2:length(trts)) { nmk <- paste0("d[",k,"]"); if (nmk %in% nm) ds[[trts[k]]] <- pick(nmk) }
  out <- list(type = type, mx_bl = mx_bl, d = ds)
  for (cand in c("B","b_bl[2]")) if (cand %in% nm) { out$B <- pick(cand); break }
  for (cand in c("sd","sd[1]")) if (cand %in% nm) { out$sd <- pick(cand); break }
  out
}

cat("\n=== bnma FIXED-effect, shared baseline-risk slope ===\n")
fe <- run_bnma("fixed"); print(round(fe$summary$summary.samples$statistics, 4))
feo <- extract(fe$summary, "fixed")

cat("\n=== bnma RANDOM-effects, shared baseline-risk slope ===\n")
re <- run_bnma("random"); print(round(re$summary$summary.samples$statistics, 4))
reo <- extract(re$summary, "random")

dir.create("test/nmr_baseline_risk_results", showWarnings = FALSE)
write_json(list(dataset = "bnma::certolizumab", mx_bl = mx_bl, treatments = trts,
                fixed = feo, random = reo),
           "test/nmr_baseline_risk_results/bnma.json", auto_unbox = TRUE, pretty = TRUE, digits = 8)
cat("\nWrote test/nmr_baseline_risk_results/bnma.json\n")
