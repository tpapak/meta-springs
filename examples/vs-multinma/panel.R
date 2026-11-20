# The 15-prior heterogeneity panel shared by Examples 2 and 3.
# Spec strings are understood by both the springs engine and mn_prior().

panel <- c(
  # vague / weakly informative on tau
  "hn:0.5", "hn:1", "hn:2", "hn:5", "hc:0.5", "hc:1", "exp:1", "exp:2",
  "unif:2", "unif:5",
  # Turner empirical predictive priors on tau^2 (log-OR scale)
  "ln:-2.75:1.61",   # 2019 Tab 2: subjective outcomes (used for smoking in Turner 2019)
  "ln:-2.26:1.45",   # 2015 Tab IV: subjective, non-pharma vs non-pharma
  "ln:-2.56:1.74",   # 2015: general healthcare setting (null model)
  "ln:-4.18:1.41",   # 2015 Tab IV: all-cause mortality, pharma vs pharma
  "ln:-3.95:1.34"    # 2015 Tab IV: all-cause mortality, pharma vs placebo
)
label <- c(
  "hn:0.5" = "HN(τ;0.5)", "hn:1" = "HN(τ;1)", "hn:2" = "HN(τ;2)", "hn:5" = "HN(τ;5)",
  "hc:0.5" = "HC(τ;0.5)", "hc:1" = "HC(τ;1)", "exp:1" = "Exp(τ;1)", "exp:2" = "Exp(τ;2)",
  "unif:2" = "U(τ;0,2)", "unif:5" = "U(τ;0,5)",
  "ln:-2.75:1.61" = "LN(τ²;−2.75,1.61²) Turner subjective",
  "ln:-2.26:1.45" = "LN(τ²;−2.26,1.45²) Turner subj. NP-vs-NP",
  "ln:-2.56:1.74" = "LN(τ²;−2.56,1.74²) Turner general",
  "ln:-4.18:1.41" = "LN(τ²;−4.18,1.41²) Turner mortality P-vs-P",
  "ln:-3.95:1.34" = "LN(τ²;−3.95,1.34²) Turner mortality P-vs-C")

