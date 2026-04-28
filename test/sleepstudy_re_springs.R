# Visualize the tier-3 spring network for sleepstudy.
#
# Model:  Reaction ~ Days + (Days | Subject)  (random intercept + random slope, 2×2 Σ)
# Each subject gets 2 r.e. nodes (u_int, u_slope) coupled by a cross-spring
# whose hardness is determined by the off-diagonal of Σ⁻¹.
#
# Output:  test/sleepstudy_re_springs.html
#
# Run:   Rscript test/sleepstudy_re_springs.R

suppressPackageStartupMessages(library(lme4))

# --- fit on the canonical lme4 sleepstudy (matches our spring-EM on the
# same-data test to within linear-solve precision) ---
fit <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy, REML = TRUE,
            control = lmerControl(optimizer = "bobyqa"))

fe  <- fixef(fit)
vc  <- as.data.frame(VarCorr(fit))
Sigma <- matrix(NA, 2, 2)
Sigma[1,1] <- vc$vcov[vc$grp == "Subject" & vc$var1 == "(Intercept)" & is.na(vc$var2)]
Sigma[2,2] <- vc$vcov[vc$grp == "Subject" & vc$var1 == "Days"        & is.na(vc$var2)]
Sigma[1,2] <- Sigma[2,1] <- vc$vcov[vc$grp == "Subject" & !is.na(vc$var2)]
s2e   <- vc$vcov[vc$grp == "Residual"]
rho   <- Sigma[1,2] / sqrt(Sigma[1,1]*Sigma[2,2])
Sinv  <- solve(Sigma)
k_self_int   <- Sinv[1,1] + Sinv[1,2]   # = row sum of row 1 of Σ⁻¹
k_self_slope <- Sinv[2,2] + Sinv[1,2]   # = row sum of row 2 of Σ⁻¹
k_cross      <- -Sinv[1,2]              # cross-spring between u_int and u_slope
k_obs        <- 1 / s2e                 # uniform for Gaussian LMM

blups <- ranef(fit)$Subject
subj_ids <- rownames(blups)
u_int  <- blups[, "(Intercept)"]
u_days <- blups[, "Days"]
J <- nrow(blups)

# Per-spring energies
E_self_int   <- 0.5 * k_self_int   * u_int^2
E_self_slope <- 0.5 * k_self_slope * u_days^2
E_cross      <- 0.5 * k_cross      * (u_int - u_days)^2

# Pick three representative subjects: largest +, largest −, close to zero
ord <- order(u_int)
rep_ix <- c(ord[J], ord[1], ord[round(J/2)])     # biggest, smallest, median
rep_ix <- unique(rep_ix)

# ---- SVG: structural diagram (node types + spring types, one subject slab) ----
svg_struct <- '
<svg viewBox="0 0 900 440" xmlns="http://www.w3.org/2000/svg" style="background:#fafafa;border:1px solid #ccc;border-radius:6px">
  <style>
    .bt { fill:#d24; stroke:#822; stroke-width:1.5 }
    .ui { fill:#26a; stroke:#134; stroke-width:1.5 }
    .us { fill:#4a8; stroke:#163; stroke-width:1.5 }
    .zr { fill:#aaa; stroke:#555; stroke-dasharray:3,2 }
    .ya { fill:#cc7; stroke:#775 }
    .obs  { stroke:#555; stroke-width:1.8; fill:none }
    .self { stroke:#2a5; stroke-width:2.2; fill:none; stroke-dasharray:4,3 }
    .cross{ stroke:#c26; stroke-width:2.5; fill:none }
    .lbl   { font:13px sans-serif; fill:#111; text-anchor:middle }
    .lbl-s { font:11px sans-serif; fill:#555 }
  </style>

  <!-- Left column: fixed effects -->
  <text x="100" y="40" class="lbl" style="font-weight:bold">fixed β (shared)</text>
  <circle cx="100" cy="90" r="22" class="bt"/><text x="100" y="94" class="lbl">β_Int</text>
  <circle cx="100" cy="170" r="22" class="bt"/><text x="100" y="174" class="lbl">β_Days</text>

  <!-- Middle: one subject subgraph -->
  <text x="450" y="40" class="lbl" style="font-weight:bold">one subject (of 18): 2 r.e. + cross-spring</text>
  <circle cx="380" cy="140" r="22" class="ui"/><text x="380" y="144" class="lbl">u_int</text>
  <circle cx="520" cy="140" r="22" class="us"/><text x="520" y="144" class="lbl">u_sl</text>

  <!-- self-springs to zero -->
  <rect x="320" y="260" width="30" height="22" class="zr"/><text x="335" y="275" class="lbl">0</text>
  <rect x="560" y="260" width="30" height="22" class="zr"/><text x="575" y="275" class="lbl">0</text>
  <path class="self" d="M380 162 L335 260"/>
  <text x="340" y="220" class="lbl-s" style="fill:#2a5">k_self_int</text>
  <path class="self" d="M520 162 L575 260"/>
  <text x="580" y="220" class="lbl-s" style="fill:#2a5">k_self_slope</text>

  <!-- cross-spring between u_int and u_slope -->
  <path class="cross" d="M402 140 Q450 100 498 140"/>
  <text x="450" y="100" class="lbl-s" style="fill:#c26;font-weight:bold">k_cross  (NEW in tier 3)</text>

  <!-- Right: one example observation -->
  <text x="820" y="40" class="lbl" style="font-weight:bold">one obs (of 180)</text>
  <circle cx="780" cy="140" r="18" class="ya"/><text x="780" y="144" class="lbl-s">η_i</text>
  <rect x="820" y="131" width="30" height="20" class="zr"/><text x="835" y="145" class="lbl">y_i</text>

  <path class="obs" d="M796 140 L820 141"/>
  <text x="810" y="130" class="lbl-s">k_obs</text>

  <!-- schematic wires from β and u into η_i -->
  <line x1="122" y1="90"  x2="764" y2="140" stroke="#ddd"/>
  <line x1="122" y1="170" x2="764" y2="140" stroke="#ddd"/>
  <line x1="402" y1="140" x2="764" y2="140" stroke="#ddd"/>
  <line x1="520" y1="140" x2="764" y2="140" stroke="#ddd"/>

  <!-- legend -->
  <g transform="translate(50,340)">
    <text x="0" y="0" class="lbl-s" style="font-weight:bold">legend</text>
    <circle cx="10" cy="20" r="8" class="bt"/><text x="25" y="24" class="lbl-s">fixed-effect node β</text>
    <circle cx="10" cy="45" r="8" class="ui"/><text x="25" y="49" class="lbl-s">random-effect node u (intercept)</text>
    <circle cx="10" cy="70" r="8" class="us"/><text x="25" y="74" class="lbl-s">random-effect node u (slope)</text>
    <rect x="2" y="85" width="16" height="12" class="zr"/><text x="25" y="95" class="lbl-s">pinned anchor (y_i value, or 0)</text>
  </g>
  <g transform="translate(400,340)">
    <path class="obs" d="M0 20 L40 20"/><text x="50" y="24" class="lbl-s">observation spring — hardness 1/σ²_ε (uniform for Gaussian)</text>
    <path class="self" d="M0 45 L40 45"/><text x="50" y="49" class="lbl-s">self shrinkage spring — hardness (Σ⁻¹)_ii + off-diag</text>
    <path class="cross" d="M0 70 L40 70"/><text x="50" y="74" class="lbl-s">cross shrinkage spring — hardness −(Σ⁻¹)_{ij}</text>
  </g>
</svg>
'

# ---- SVG: 3 representative subjects, hardness-scaled spring widths, BLUP-colored self-springs ----
make_subj_svg <- function(ixs) {
  rows <- ""
  xs <- c(120, 430, 740)
  widths_scale <- function(k) 0.6 + 4 * k / max(k_self_int, k_self_slope, k_cross, 0.01)  # crude visual

  for (i in seq_along(ixs)) {
    ix <- ixs[i]
    u1 <- u_int[ix]; u2 <- u_days[ix]
    x0 <- xs[i]
    col_int  <- if (u1 > 0) "#d24" else if (u1 < 0) "#269" else "#666"
    col_slope<- if (u2 > 0) "#d24" else if (u2 < 0) "#269" else "#666"
    sw_si <- widths_scale(k_self_int)
    sw_ss <- widths_scale(k_self_slope)
    sw_cx <- widths_scale(k_cross)

    rows <- paste0(rows, sprintf('
      <g transform="translate(%d,40)">
        <text x="100" y="-5" class="lbl" style="font-weight:bold">Subject %s</text>
        <circle cx="60"  cy="60" r="22" class="ui"/><text x="60"  y="64" class="lbl">u_int</text>
        <circle cx="140" cy="60" r="22" class="us"/><text x="140" y="64" class="lbl">u_sl</text>

        <rect x="30"  y="170" width="30" height="22" class="zr"/><text x="45"  y="185" class="lbl">0</text>
        <rect x="155" y="170" width="30" height="22" class="zr"/><text x="170" y="185" class="lbl">0</text>

        <!-- self springs coloured by BLUP sign -->
        <path d="M60 82 L45 170"  stroke="%s" stroke-width="%.2f" stroke-dasharray="4,3" fill="none"/>
        <path d="M140 82 L170 170" stroke="%s" stroke-width="%.2f" stroke-dasharray="4,3" fill="none"/>

        <!-- cross spring -->
        <path d="M82 60 Q100 20 118 60" stroke="#c26" stroke-width="%.2f" fill="none"/>

        <text x="100" y="215" class="lbl-s">u_int = %.2f   u_slope = %.2f</text>
        <text x="100" y="230" class="lbl-s">E_int=%.3f  E_sl=%.3f  E_cross=%.3f</text>
      </g>',
      x0, subj_ids[ix],
      col_int,  sw_si,
      col_slope, sw_ss,
      sw_cx,
      u1, u2,
      E_self_int[ix], E_self_slope[ix], E_cross[ix]
    ))
  }

  sprintf('
<svg viewBox="0 0 900 300" xmlns="http://www.w3.org/2000/svg" style="background:#fafafa;border:1px solid #ccc;border-radius:6px">
  <style>
    .ui { fill:#26a; stroke:#134; stroke-width:1.5 }
    .us { fill:#4a8; stroke:#163; stroke-width:1.5 }
    .zr { fill:#aaa; stroke:#555; stroke-dasharray:3,2 }
    .lbl   { font:13px sans-serif; fill:#111; text-anchor:middle }
    .lbl-s { font:10.5px sans-serif; fill:#555; text-anchor:middle }
  </style>
  %s
</svg>', rows)
}
svg_reps <- make_subj_svg(rep_ix)

# ---- BLUP table ----
blup_rows <- paste(sprintf(
  "    <tr><td>%s</td><td class='n'>%+.3f</td><td class='n'>%+.3f</td><td class='n'>%.4f</td><td class='n'>%.4f</td><td class='n'>%.4f</td></tr>",
  subj_ids, u_int, u_days, E_self_int, E_self_slope, E_cross
), collapse = "\n")

# ---- total energies ----
tot_obs_energy <- 0.5 * sum(resid(fit)^2) / s2e
tot_shr_energy <- sum(E_self_int) + sum(E_self_slope) + sum(E_cross)

html <- sprintf('<!DOCTYPE html>
<html><head><meta charset="utf-8"><title>Tier-3 spring network — sleepstudy</title>
<style>
  body { font-family: -apple-system, sans-serif; max-width: 960px; margin: 2em auto;
         padding: 0 1em; color:#222; line-height:1.5 }
  h1 { border-bottom:2px solid #e0e3e8; padding-bottom:.3em; color:#234 }
  h2 { color:#345; margin-top:1.5em }
  table { border-collapse:collapse; margin:.6em 0 }
  th, td { border:1px solid #ddd; padding:4px 10px; font-variant-numeric:tabular-nums }
  th { background:#f2f4f7; text-align:left }
  td.n { text-align:right }
  .box { background:#f7f9fb; border-left:4px solid #68a; padding:.7em 1em; margin:1em 0 }
  code { background:#f4f4f5; padding:1px 4px; border-radius:3px }
</style></head><body>

<h1>Tier-3 spring network — sleepstudy <code>Reaction ~ Days + (Days|Subject)</code></h1>

<div class="box">
Each subject gets a <b>2-vector random effect</b> (intercept, Days slope) with a
<b>2×2 covariance Σ</b>.  The shrinkage quadratic <code>½ u_jᵀ Σ⁻¹ u_j</code> decomposes
into <b>3 springs per subject</b>: two self-springs (one per r.e. component) plus one
<b>cross-spring</b> coupling intercept and slope.  The cross-spring is the new structure
vs. tier 1.
</div>

<h2>Fit summary (lme4 REML)</h2>
<table><tr><th>quantity</th><th>value</th></tr>
<tr><td>β_Int</td><td class="n">%.4f</td></tr>
<tr><td>β_Days</td><td class="n">%.4f</td></tr>
<tr><td>Σ[Int, Int]</td><td class="n">%.3f</td></tr>
<tr><td>Σ[Days, Days]</td><td class="n">%.3f</td></tr>
<tr><td>Σ[Int, Days]</td><td class="n">%.3f</td></tr>
<tr><td>ρ</td><td class="n">%.4f</td></tr>
<tr><td>σ²_residual</td><td class="n">%.2f</td></tr>
</table>

<h2>Spring hardnesses (from Σ⁻¹)</h2>
<table><tr><th>spring type</th><th>count</th><th>hardness</th><th>where from</th></tr>
<tr><td>observation</td><td class="n">180</td><td class="n">%.5f</td><td>1/σ²_ε (uniform)</td></tr>
<tr><td>self on u_int</td><td class="n">18</td><td class="n">%.5f</td><td>(Σ⁻¹)_{11} + (Σ⁻¹)_{12}</td></tr>
<tr><td>self on u_slope</td><td class="n">18</td><td class="n">%.5f</td><td>(Σ⁻¹)_{22} + (Σ⁻¹)_{12}</td></tr>
<tr><td>cross int ↔ slope</td><td class="n">18</td><td class="n">%.5f</td><td>−(Σ⁻¹)_{12}  (positive because ρ > 0 ⇒ (Σ⁻¹)_{12} < 0)</td></tr>
</table>

<h2>Structural diagram</h2>
%s

<h2>Three representative subjects</h2>
<p>Spring-width ∝ hardness; self-spring colour: <span style="color:#d24">red</span> = positive BLUP, <span style="color:#269">blue</span> = negative.  Energy values (½ · k · ℓ²) shown for each spring in the subject.</p>
%s

<h2>All 18 subject BLUPs and per-spring energies</h2>
<table><tr><th>Subject</th><th>u_int</th><th>u_slope</th><th>E_self_int</th><th>E_self_slope</th><th>E_cross</th></tr>
%s
</table>

<h2>Aggregate</h2>
<ul>
  <li><b>Total free nodes</b>: 2 β + 36 u = <b>38</b></li>
  <li><b>Total springs</b>: 180 obs + 36 self + 18 cross = <b>234</b></li>
  <li><b>Total observation-spring energy</b>: %.2f</li>
  <li><b>Total shrinkage-spring energy</b>: %.2f  (split: self_int=%.2f, self_slope=%.2f, cross=%.2f)</li>
  <li><b>Total spring energy</b>: %.2f   &nbsp;≈  (n−p)/2 = %d  &nbsp;&nbsp;(REML fixpoint identity)</li>
</ul>

<p><small>Generated by <code>test/sleepstudy_re_springs.R</code>.  Our Haskell tier-3
spring-EM (<code>fitLMMRE</code>) agrees with this lme4 fit to 4 decimals on the
same data (see <code>test/sleepstudy_re_compare.R</code>).</small></p>
</body></html>
',
fe[["(Intercept)"]], fe[["Days"]],
Sigma[1,1], Sigma[2,2], Sigma[1,2], rho, s2e,
k_obs, k_self_int, k_self_slope, k_cross,
svg_struct, svg_reps,
blup_rows,
tot_obs_energy,
tot_shr_energy, sum(E_self_int), sum(E_self_slope), sum(E_cross),
tot_obs_energy + tot_shr_energy, (nrow(sleepstudy) - 2L) %/% 2L
)

out <- "test/sleepstudy_re_springs.html"
writeLines(html, out)
cat("Wrote", out, "(", nchar(html), "bytes)\n")
cat("Open in browser:  file://", normalizePath(out), "\n", sep = "")
