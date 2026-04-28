# Published IPD example — logistic GLMM, with spring-network view rendered to HTML.
#
# Dataset: multinma::plaque_psoriasis_ipd (Phillippo 2020 / Phillippo et al. 2023)
#   — a Riley-style individual-participant-data meta-analysis, 4 studies,
#     4118 patients, binary pasi75 outcome.
#
# Model:  logit P(pasi75 = 1)  =  β · trt  +  γ · weight  +  u_study
# Fit:    lme4::glmer (logistic GLMM, Laplace likelihood, random intercept per study)
# Output: test/ipd_glmm_springs.html

suppressPackageStartupMessages({
  library(multinma)
  library(lme4)
})

data(plaque_psoriasis_ipd)
d <- plaque_psoriasis_ipd

# Keep patients with complete cases for the covariate.
d <- d[complete.cases(d[, c("pasi75", "trtc", "studyc", "weight")]), ]
d$trtc  <- factor(d$trtc, levels = c("PBO", "ETN", "IXE_Q2W", "IXE_Q4W", "UST"))
d$studyc <- factor(d$studyc)
d$wt_c  <- as.numeric(scale(d$weight, center = TRUE, scale = 10))   # per-10kg

# ---- fit: logistic GLMM, random intercept per study ----
fit <- glmer(pasi75 ~ trtc + wt_c + (1 | studyc),
             data    = d,
             family  = binomial,
             control = glmerControl(optimizer = "bobyqa"))

co  <- summary(fit)$coefficients
vc  <- as.data.frame(VarCorr(fit))
tau2 <- vc$vcov[vc$grp == "studyc"]
re  <- ranef(fit)$studyc[[1]]
re_names <- rownames(ranef(fit)$studyc)

# ---- IRLS weights at the converged fit ----
# For logistic GLMM each patient's observation spring has hardness w_i = p_i·(1 − p_i)
# (Bernoulli Fisher weight).  Compute from the fitted mean.
mu <- fitted(fit)
w  <- mu * (1 - mu)
summary_w <- quantile(w, probs = c(0, .1, .25, .5, .75, .9, 1))

# ---- build HTML ----
out <- "test/ipd_glmm_springs.html"

fe_tbl <- paste(
  sprintf(
    "    <tr><td>%s</td><td>%s</td><td>%.4f</td><td>%.4f</td><td>%.2f</td></tr>",
    c("(Intercept)", "ETN vs PBO", "IXE Q2W vs PBO", "IXE Q4W vs PBO", "UST vs PBO", "weight (per 10 kg)"),
    c("β₀", "β_ETN", "β_IXE_Q2W", "β_IXE_Q4W", "β_UST", "β_wt"),
    co[, "Estimate"], co[, "Std. Error"], co[, "z value"]
  ),
  collapse = "\n"
)

re_tbl <- paste(
  sprintf("    <tr><td>%s</td><td>u_%s</td><td>%+.4f</td></tr>",
          re_names, re_names, re),
  collapse = "\n"
)

# --- SVG spring diagram ---
# Five sample patients (one per study + one placebo), stacked; show link to
# fixed-effect and random-effect nodes.
svg <- '
<svg viewBox="0 0 820 520" xmlns="http://www.w3.org/2000/svg"
     style="background:#fafafa;border:1px solid #ccc;border-radius:6px">
  <defs>
    <marker id="arrow" markerWidth="7" markerHeight="7" refX="6" refY="3.5"
            orient="auto"><polygon points="0 0,7 3.5,0 7" fill="#666"/></marker>
    <pattern id="spring" width="8" height="4" patternUnits="userSpaceOnUse">
      <path d="M0 2 L2 0 L4 4 L6 0 L8 2" stroke="#888" fill="none"/>
    </pattern>
  </defs>

  <style>
    .node-beta  { fill:#d47; stroke:#822; stroke-width:1.5 }
    .node-u     { fill:#4b7; stroke:#273; stroke-width:1.5 }
    .node-obs   { fill:#47d; stroke:#228; stroke-width:1 }
    .node-zero  { fill:#aaa; stroke:#555; stroke-dasharray:3,2 }
    .lbl        { font:12px sans-serif; fill:#222; text-anchor:middle }
    .lbl-s      { font:10px sans-serif; fill:#555 }
    .spring-obs { stroke:#48d; stroke-width:1.8; fill:none }
    .spring-shr { stroke:#5c4; stroke-width:2.4; fill:none; stroke-dasharray:4,3 }
    .pin        { stroke:#555; stroke-width:1; fill:#555 }
    h           { font:14px sans-serif; fill:#222 }
  </style>

  <!-- fixed-effect node column (left) -->
  <text x="90" y="30" class="lbl" style="font-weight:bold">fixed effects β</text>
  <circle cx="90" cy="70"  r="18" class="node-beta"/>
  <text x="90" y="74" class="lbl">β₀</text>
  <circle cx="90" cy="120" r="18" class="node-beta"/>
  <text x="90" y="124" class="lbl">β_ETN</text>
  <circle cx="90" cy="170" r="18" class="node-beta"/>
  <text x="90" y="174" class="lbl">β_IXE2</text>
  <circle cx="90" cy="220" r="18" class="node-beta"/>
  <text x="90" y="224" class="lbl">β_IXE4</text>
  <circle cx="90" cy="270" r="18" class="node-beta"/>
  <text x="90" y="274" class="lbl">β_UST</text>
  <circle cx="90" cy="320" r="18" class="node-beta"/>
  <text x="90" y="324" class="lbl">β_wt</text>

  <!-- random-effect column -->
  <text x="380" y="30" class="lbl" style="font-weight:bold">random ints u</text>
  <circle cx="380" cy="90"  r="18" class="node-u"/>
  <text x="380" y="94" class="lbl">u_IXORA</text>
  <circle cx="380" cy="170" r="18" class="node-u"/>
  <text x="380" y="174" class="lbl">u_UN1</text>
  <circle cx="380" cy="250" r="18" class="node-u"/>
  <text x="380" y="254" class="lbl">u_UN2</text>
  <circle cx="380" cy="330" r="18" class="node-u"/>
  <text x="380" y="334" class="lbl">u_UN3</text>

  <!-- pinned 0 for shrinkage springs -->
  <rect x="540" y="200" width="30" height="20" class="node-zero"/>
  <text x="555" y="215" class="lbl">0</text>
  <text x="555" y="244" class="lbl-s">(pinned)</text>

  <!-- shrinkage springs from u to 0 -->
  <path class="spring-shr" d="M398 90  L540 210"/>
  <path class="spring-shr" d="M398 170 L540 210"/>
  <path class="spring-shr" d="M398 250 L540 210"/>
  <path class="spring-shr" d="M398 330 L540 210"/>
  <text x="465" y="145" class="lbl-s" style="fill:#383">1/τ²</text>

  <!-- one representative obs spring -->
  <text x="700" y="30" class="lbl" style="font-weight:bold">example patient i</text>
  <circle cx="700" cy="150" r="16" class="node-obs"/>
  <text x="700" y="154" class="lbl">η_i</text>
  <rect x="750" y="140" width="30" height="20" class="node-zero"/>
  <text x="765" y="155" class="lbl">y_i</text>
  <text x="765" y="177" class="lbl-s">(pinned)</text>
  <path class="spring-obs" d="M716 150 L750 150"/>
  <text x="733" y="140" class="lbl-s" style="fill:#28a">w_i</text>

  <!-- wires: fixed-effect nodes to η_i (linear combination) -->
  <line x1="108" y1="70"  x2="684" y2="150" stroke="#ddd"/>
  <line x1="108" y1="120" x2="684" y2="150" stroke="#ddd"/>
  <line x1="108" y1="170" x2="684" y2="150" stroke="#ddd"/>
  <line x1="108" y1="220" x2="684" y2="150" stroke="#ddd"/>
  <line x1="108" y1="270" x2="684" y2="150" stroke="#ddd"/>
  <line x1="108" y1="320" x2="684" y2="150" stroke="#ddd"/>
  <line x1="398" y1="90"  x2="684" y2="150" stroke="#ddd"/>

  <text x="410" y="480" class="lbl-s">
    η_i = X_iβ + Z_iu; observation spring hardness w_i = p_i(1−p_i) (Fisher weight).
  </text>
  <text x="410" y="498" class="lbl-s">
    Shrinkage springs pull each u_j toward 0 with hardness 1/τ².
  </text>
</svg>
'

html <- sprintf('<!DOCTYPE html>
<html lang="en"><head><meta charset="utf-8">
<title>IPD GLMM — spring network view</title>
<style>
  body  { font-family: -apple-system, sans-serif; max-width: 900px; margin: 2em auto;
          color:#222; line-height:1.45; padding: 0 1em; }
  h1    { color:#234; border-bottom: 2px solid #e0e3e8; padding-bottom: .3em; }
  h2    { color:#345; margin-top: 1.5em; }
  table { border-collapse: collapse; margin: 0.5em 0; }
  th, td { padding: 4px 10px; border: 1px solid #ddd; font-variant-numeric: tabular-nums; }
  th    { background: #f2f4f7; text-align: left; }
  td.n  { text-align: right; }
  .box  { background: #f7f9fb; border-left: 4px solid #68a; padding: .7em 1em; margin: 1em 0; }
  code  { background:#f4f4f5; padding:1px 4px; border-radius:3px }
</style></head><body>

<h1>Published IPD example — logistic GLMM with spring-network view</h1>

<div class="box">
<b>Dataset</b>: <code>multinma::plaque_psoriasis_ipd</code>
(Phillippo, Dias, Ades et al., 2023 — plaque psoriasis IPD-MA following
Riley-style individual-participant-data methodology).<br>
<b>n</b> = %d patients,  <b>J</b> = %d studies,
<b>treatments</b>: PBO, ETN, IXE_Q2W, IXE_Q4W, UST.<br>
<b>Outcome</b>: <code>pasi75</code> — Bernoulli (1 if 75%% skin clearance at endpoint).<br>
<b>Model</b>: <code>pasi75 ~ trtc + wt_c + (1|studyc)</code>, fit by
<code>lme4::glmer</code> (Laplace logistic GLMM).
</div>

<h2>Fit: fixed effects (log-odds scale)</h2>
<table><tr><th>term</th><th>param</th><th>estimate</th><th>SE</th><th>z</th></tr>
%s
</table>

<h2>Random effects (study-level BLUPs)</h2>
<p>τ² = <b>%.4f</b> &nbsp;&nbsp; τ = <b>%.4f</b></p>
<table><tr><th>study</th><th>node</th><th>BLUP (log-odds shift)</th></tr>
%s
</table>

<h2>Spring network at the fit</h2>
<ul>
  <li><b>Observation springs</b>: <b>%d</b>, one per patient. Hardness
      <code>w_i = p_i(1 − p_i)</code> (IRLS Fisher weight at the converged η̂).
      Summary of w_i across patients:
      <table style="margin-top:4px">
      <tr><th>min</th><th>q10</th><th>q25</th><th>median</th><th>q75</th><th>q90</th><th>max</th></tr>
      <tr>%s</tr></table>
  </li>
  <li><b>Shrinkage springs</b>: <b>%d</b>, one per study. Hardness <code>1/τ² = %.4f</code>.</li>
  <li><b>Free nodes</b>: 6 fixed-effect (β₀, β_ETN, β_IXE_Q2W, β_IXE_Q4W, β_UST, β_wt) + %d
      random-effect (u_j per study).</li>
  <li><b>Pinned targets</b>: %d patient y values + 1 shared zero for the shrinkage springs.</li>
</ul>

%s

<h2>Spring picture in text</h2>
<pre style="font-size:12px;line-height:1.2;background:#f7f9fb;padding:0.8em;border-radius:6px">
fixed effects (6 nodes):         random intercepts (4 nodes):     per-patient (4118 obs):

  β₀    ●──┐                       u_IXORA  ○──┐                     y_i ●───[w_i]───○ η_i
  β_ETN ●──┤                       u_UN1    ○──┤                          ↑
  β_IXE2●──┼──── X_i·β ─────► η_i ◄──────── Z_i·u  ──┘                 pinned
  β_IXE4●──┤                       u_UN2    ○──┤ shrinkage springs
  β_UST ●──┤                       u_UN3    ○──┤ [1/τ²]  →  0
  β_wt  ●──┘                                    └────────────○
                                                            pinned

Each patient spring pulls η_i toward y_i with hardness w_i (different per patient!
this is what distinguishes GLMM from LMM — hardnesses are not uniform).
Each study spring pulls u_j toward 0 with the same hardness 1/τ².
</pre>

<p><small>Generated by <code>test/ipd_glmm_springs.R</code> on the
<code>multinma::plaque_psoriasis_ipd</code> dataset, model fit by
<code>lme4::glmer</code>.</small></p>

</body></html>
',
nrow(d), nlevels(d$studyc),
fe_tbl,
tau2, sqrt(tau2),
re_tbl,
nrow(d),
paste(sprintf("<td class='n'>%.3f</td>", summary_w), collapse = ""),
nlevels(d$studyc), 1/tau2,
nlevels(d$studyc),
nrow(d),
svg
)

writeLines(html, out)
cat("Wrote", out, "(", nchar(html), "bytes)\n")
cat("Open in browser:  file://", normalizePath(out), "\n", sep = "")
