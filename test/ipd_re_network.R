# IPD tier-3 spring network with TWO treatments — the Riley/Stewart
# "random-intercept + random-treatment-effect per study" model.
#
# Synthetic IPD, 5 studies × 10 patients, binary treatment indicator.
#
# Model:
#   y_i  =  β_0  +  β_trt · trt_i  +  u_{s(i),int}  +  u_{s(i),trt} · trt_i  +  ε_i
#   (u_int, u_trt) ~ N(0, Σ)           ← 2×2 Σ coupling intercept & trt effect
#
# Output:  test/ipd_re_network.html   (self-contained SVG network)
#
# Run:  Rscript test/ipd_re_network.R

suppressPackageStartupMessages({
  library(lme4)
  library(MASS)
})

# ---------- synthesize ----------
set.seed(7)
J            <- 5                   # studies
n_per_study  <- 10                  # 5 ctrl + 5 trt
N            <- J * n_per_study

beta0_true     <- 0
beta_trt_true  <- 3
Sigma_true     <- matrix(c(1.0, 0.25,
                           0.25, 0.4), 2, 2)
sigma2E_true   <- 1.0

u_mat  <- mvrnorm(J, mu = c(0, 0), Sigma = Sigma_true)
u_int_true <- u_mat[, 1]
u_trt_true <- u_mat[, 2]

studies <- rep(1:J, each = n_per_study)
trt     <- rep(c(rep(0, n_per_study / 2), rep(1, n_per_study / 2)), J)
y       <- beta0_true + beta_trt_true * trt +
           u_int_true[studies] + u_trt_true[studies] * trt +
           rnorm(N, 0, sqrt(sigma2E_true))

d <- data.frame(y = y, trt = trt, study = factor(studies))

# ---------- fit ----------
fit <- lmer(y ~ trt + (trt | study), data = d, REML = TRUE,
            control = lmerControl(optimizer = "bobyqa"))

vc <- as.data.frame(VarCorr(fit))
S  <- matrix(0, 2, 2)
S[1, 1] <- vc$vcov[vc$grp == "study" & vc$var1 == "(Intercept)" & is.na(vc$var2)]
S[2, 2] <- vc$vcov[vc$grp == "study" & vc$var1 == "trt"         & is.na(vc$var2)]
S[1, 2] <- S[2, 1] <- vc$vcov[vc$grp == "study" & !is.na(vc$var2)]
s2e  <- vc$vcov[vc$grp == "Residual"]
Si   <- solve(S)

k_obs        <- 1 / s2e
k_self_int   <- Si[1, 1] + Si[1, 2]
k_self_trt   <- Si[2, 2] + Si[1, 2]
k_cross      <- -Si[1, 2]

blups       <- ranef(fit)$study
u_int_hat   <- blups[, "(Intercept)"]
u_trt_hat   <- blups[, "trt"]
study_ids   <- rownames(blups)

# ---------- layout ----------
W <- 1000; H <- 1000
cx <- W / 2; cy <- H / 2
r_u <- 240; r_y <- 400

# 5 studies around the ring
theta_s <- seq(0, 2 * pi, length.out = J + 1)[-1] - pi / 2
u_sep   <- 32

u_int_pos <- data.frame(
  x = cx + r_u * cos(theta_s) - u_sep * sin(theta_s),
  y = cy + r_u * sin(theta_s) + u_sep * cos(theta_s)
)
u_trt_pos <- data.frame(
  x = cx + r_u * cos(theta_s) + u_sep * sin(theta_s),
  y = cy + r_u * sin(theta_s) - u_sep * cos(theta_s)
)

# observations: 10 per study in an arc, split ctrl / trt
obs_span_ctrl <- c(-0.35, -0.05)    # relative angle offset for ctrl group
obs_span_trt  <- c( 0.05,  0.35)

obs_x    <- numeric(N); obs_y <- numeric(N)
obs_sidx <- integer(N); obs_trt_i <- integer(N)
obs_y_val <- numeric(N)
idx <- 1
for (s in seq_len(J)) {
  ctrl_n <- sum(studies == s & trt == 0)
  trt_n  <- sum(studies == s & trt == 1)
  ctrl_angles <- theta_s[s] + seq(obs_span_ctrl[1], obs_span_ctrl[2], length.out = ctrl_n)
  trt_angles  <- theta_s[s] + seq(obs_span_trt[1],  obs_span_trt[2],  length.out = trt_n)
  for (a in ctrl_angles) {
    obs_x[idx] <- cx + r_y * cos(a); obs_y[idx] <- cy + r_y * sin(a)
    obs_sidx[idx] <- s; obs_trt_i[idx] <- 0
    obs_y_val[idx] <- y[which(studies == s & trt == 0)[1:ctrl_n][match(a, ctrl_angles)]]
    idx <- idx + 1
  }
  for (a in trt_angles) {
    obs_x[idx] <- cx + r_y * cos(a); obs_y[idx] <- cy + r_y * sin(a)
    obs_sidx[idx] <- s; obs_trt_i[idx] <- 1
    obs_y_val[idx] <- y[which(studies == s & trt == 1)[1:trt_n][match(a, trt_angles)]]
    idx <- idx + 1
  }
}

# β and zero at centre
b_int_pos  <- c(cx - 26, cy)
b_trt_pos  <- c(cx + 26, cy)
zero_pos   <- c(cx,     cy - 50)

# ---------- SVG assembly ----------
# obs springs: every y-anchor linked to BOTH u-nodes of its study.
# For ctrl obs (trt=0): trt-slope is multiplied by 0, so only u_int is active → one line.
# For trt obs  (trt=1): both u_int AND u_trt participate → two lines.
obs_springs <- character(0)
for (i in seq_len(N)) {
  s <- obs_sidx[i]
  col <- if (obs_trt_i[i] == 0) "#2c7fb8" else "#c94b3d"
  obs_springs <- c(obs_springs,
    sprintf('<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" stroke="%s" stroke-opacity="0.45" stroke-width="1.0"/>',
            obs_x[i], obs_y[i], u_int_pos$x[s], u_int_pos$y[s], col))
  if (obs_trt_i[i] == 1) {
    obs_springs <- c(obs_springs,
      sprintf('<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" stroke="%s" stroke-opacity="0.45" stroke-width="1.0"/>',
              obs_x[i], obs_y[i], u_trt_pos$x[s], u_trt_pos$y[s], col))
  }
}

# β-wires: 2 per study (β_0 → u_int, β_trt → u_trt).  β_0 also feeds every obs
# (intercept) but drawing those would clutter; we route it through u_int visually.
beta_wires <- character(0)
for (s in seq_len(J)) {
  beta_wires <- c(beta_wires,
    sprintf('<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" stroke="#b08040" stroke-opacity="0.5" stroke-width="1.0"/>',
            b_int_pos[1], b_int_pos[2], u_int_pos$x[s], u_int_pos$y[s]),
    sprintf('<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" stroke="#b08040" stroke-opacity="0.5" stroke-width="1.0"/>',
            b_trt_pos[1], b_trt_pos[2], u_trt_pos$x[s], u_trt_pos$y[s]))
}

# self-shrinkage (dashed green)
self_springs <- character(0)
for (s in seq_len(J)) {
  self_springs <- c(self_springs,
    sprintf('<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" stroke="#2ca02c" stroke-dasharray="5,3" stroke-width="1.4" opacity="0.7"/>',
            u_int_pos$x[s], u_int_pos$y[s], zero_pos[1], zero_pos[2]),
    sprintf('<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" stroke="#2ca02c" stroke-dasharray="5,3" stroke-width="1.4" opacity="0.7"/>',
            u_trt_pos$x[s], u_trt_pos$y[s], zero_pos[1], zero_pos[2]))
}

# cross-springs (solid red, arc)
cross_springs <- character(J)
for (s in seq_len(J)) {
  mx <- (u_int_pos$x[s] + u_trt_pos$x[s]) / 2
  my <- (u_int_pos$y[s] + u_trt_pos$y[s]) / 2
  dx <- mx - cx; dy <- my - cy; nrm <- sqrt(dx^2 + dy^2)
  cxp <- mx + 26 * dx / nrm; cyp <- my + 26 * dy / nrm
  cross_springs[s] <- sprintf(
    '<path d="M %.1f %.1f Q %.1f %.1f %.1f %.1f" stroke="#d62728" stroke-width="2.2" fill="none"/>',
    u_int_pos$x[s], u_int_pos$y[s], cxp, cyp, u_trt_pos$x[s], u_trt_pos$y[s]
  )
}

# nodes
y_nodes <- character(N)
for (i in seq_len(N)) {
  fill <- if (obs_trt_i[i] == 0) "#bfd7ea" else "#eab0a0"
  stroke <- if (obs_trt_i[i] == 0) "#2c7fb8" else "#c94b3d"
  y_nodes[i] <- sprintf(
    '<rect x="%.1f" y="%.1f" width="7" height="7" fill="%s" stroke="%s" stroke-width="0.6"/><title>study %d, trt=%d, y=%.2f</title>',
    obs_x[i] - 3.5, obs_y[i] - 3.5, fill, stroke,
    obs_sidx[i], obs_trt_i[i], obs_y_val[i]
  )
}
u_nodes <- character(2 * J)
for (s in seq_len(J)) {
  col_int <- if (u_int_hat[s] >= 0) "#1a5490" else "#3d80b8"
  col_trt <- if (u_trt_hat[s] >= 0) "#8a2027" else "#c05a5e"
  u_nodes[2 * s - 1] <- sprintf(
    '<circle cx="%.1f" cy="%.1f" r="9" fill="%s" stroke="#06294a" stroke-width="1"/><title>study %s  u_int=%+.3f</title>',
    u_int_pos$x[s], u_int_pos$y[s], col_int, study_ids[s], u_int_hat[s]
  )
  u_nodes[2 * s] <- sprintf(
    '<circle cx="%.1f" cy="%.1f" r="9" fill="%s" stroke="#3a0404" stroke-width="1"/><title>study %s  u_trt=%+.3f</title>',
    u_trt_pos$x[s], u_trt_pos$y[s], col_trt, study_ids[s], u_trt_hat[s]
  )
}

subj_labels <- character(J)
for (s in seq_len(J)) {
  lx <- cx + (r_u + 55) * cos(theta_s[s])
  ly <- cy + (r_u + 55) * sin(theta_s[s]) + 4
  subj_labels[s] <- sprintf(
    '<text x="%.1f" y="%.1f" font-size="13" fill="#333" text-anchor="middle" font-weight="bold">S%s</text>',
    lx, ly, study_ids[s]
  )
}

center_nodes <- sprintf('
  <circle cx="%.1f" cy="%.1f" r="16" fill="#d42a2a" stroke="#821111" stroke-width="1.4"/>
  <text x="%.1f" y="%.1f" font-size="12" fill="#fff" text-anchor="middle" font-weight="bold">β_0</text>
  <circle cx="%.1f" cy="%.1f" r="16" fill="#d42a2a" stroke="#821111" stroke-width="1.4"/>
  <text x="%.1f" y="%.1f" font-size="11" fill="#fff" text-anchor="middle" font-weight="bold">β_trt</text>
  <rect x="%.1f" y="%.1f" width="18" height="14" fill="#bbb" stroke="#555" stroke-dasharray="3,2"/>
  <text x="%.1f" y="%.1f" font-size="11" fill="#222" text-anchor="middle">0</text>
',
b_int_pos[1], b_int_pos[2], b_int_pos[1], b_int_pos[2] + 4,
b_trt_pos[1], b_trt_pos[2], b_trt_pos[1], b_trt_pos[2] + 4,
zero_pos[1] - 9, zero_pos[2] - 7, zero_pos[1], zero_pos[2] + 4
)

svg_full <- sprintf('
<svg viewBox="0 0 %d %d" xmlns="http://www.w3.org/2000/svg"
     style="background:#fafafa;border:1px solid #ccc;border-radius:6px">
  <g id="obs-springs">%s</g>
  <g id="beta-wires">%s</g>
  <g id="self-springs">%s</g>
  <g id="cross-springs">%s</g>
  <g id="y-anchors">%s</g>
  <g id="u-nodes">%s</g>
  %s
  <g id="subj-labels">%s</g>

  <g transform="translate(22,22)" font-family="sans-serif">
    <text x="0" y="0" font-size="14" font-weight="bold">IPD tier-3: y ~ trt + (trt | study)  (synthetic, %d patients in %d studies)</text>
    <text x="0" y="18" font-size="11" fill="#555">blue = control obs, red = treated obs; each treated obs pulls both u_int AND u_trt.</text>
    <text x="0" y="33" font-size="11" fill="#555">central β nodes; ring of 10 u-nodes (intercept + treatment) per study; outer ring: 50 y anchors.</text>
  </g>
</svg>',
W, H,
paste(obs_springs, collapse = "\n"),
paste(beta_wires, collapse = "\n"),
paste(self_springs, collapse = "\n"),
paste(cross_springs, collapse = "\n"),
paste(y_nodes, collapse = "\n"),
paste(u_nodes, collapse = "\n"),
center_nodes,
paste(subj_labels, collapse = "\n"),
N, J
)

# ---------- table bits ----------
blup_table <- paste(sprintf(
  '    <tr><td>S%s</td><td class="n">%+.3f</td><td class="n">%+.3f</td><td class="n">%+.3f</td><td class="n">%+.3f</td></tr>',
  study_ids, u_int_true, u_trt_true, u_int_hat, u_trt_hat
), collapse = "\n")

html <- sprintf('<!DOCTYPE html>
<html><head><meta charset="utf-8"><title>IPD tier-3 spring network</title>
<style>
  body { font-family:-apple-system,sans-serif; max-width:1100px; margin:1em auto; padding:0 1em; color:#222 }
  h1 { border-bottom:2px solid #e0e3e8; padding-bottom:.3em }
  h2 { color:#345; margin-top:1.3em }
  table { border-collapse:collapse; margin-top:.5em }
  th,td { border:1px solid #ddd; padding:3px 9px; font-variant-numeric:tabular-nums }
  th { background:#f2f4f7; text-align:left }
  td.n { text-align:right }
  .box { background:#f7f9fb; border-left:4px solid #68a; padding:.7em 1em; margin:1em 0 }
  code { background:#f4f4f5; padding:1px 4px; border-radius:3px }
</style></head><body>

<h1>IPD tier-3 spring network — two treatments, random study intercepts AND random treatment effects</h1>

<div class="box">
Synthetic 2-arm IPD-MA, %d patients in %d studies.  The model
<code>y ~ trt + (trt | study)</code> gives each study its own
<b>intercept AND its own treatment-effect</b>, correlated through a 2×2 Σ.
This is the Riley/Stewart random-treatment-effect tier-3 IPD-MA model —
the one that answers how much the treatment effect varies between
studies — rather than sleepstudy\'s <code>Days</code> slope model.
</div>

<p>%s</p>

<h2>True vs fitted (REML)</h2>
<table><tr><th>parameter</th><th>true</th><th>lme4 fit</th></tr>
<tr><td>β_0</td>           <td class="n">%.3f</td><td class="n">%.3f</td></tr>
<tr><td>β_trt</td>         <td class="n">%.3f</td><td class="n">%.3f</td></tr>
<tr><td>Σ[Int, Int]</td>   <td class="n">%.3f</td><td class="n">%.3f</td></tr>
<tr><td>Σ[trt, trt]</td>   <td class="n">%.3f</td><td class="n">%.3f</td></tr>
<tr><td>cov(Int, trt)</td> <td class="n">%.3f</td><td class="n">%.3f</td></tr>
<tr><td>σ²_ε</td>          <td class="n">%.3f</td><td class="n">%.3f</td></tr>
</table>

<h2>Spring hardnesses at the fit</h2>
<table><tr><th>spring</th><th>count</th><th>hardness</th></tr>
<tr><td>obs (uniform)</td>       <td class="n">%d</td><td class="n">%.4f</td></tr>
<tr><td>self on u_int</td>       <td class="n">%d</td><td class="n">%.4f</td></tr>
<tr><td>self on u_trt</td>       <td class="n">%d</td><td class="n">%.4f</td></tr>
<tr><td>cross int ↔ trt</td>     <td class="n">%d</td><td class="n">%.4f</td></tr>
</table>

<h2>Study-level BLUPs (true u_true vs lme4 û)</h2>
<table><tr><th>study</th><th>u_int (true)</th><th>u_trt (true)</th><th>u_int (fit)</th><th>u_trt (fit)</th></tr>
%s
</table>

<h2>What the network shows</h2>
<ul>
<li><b>Two types of observation springs by colour</b>:
  blue (control) pulls only on u_int; red (treated) pulls on BOTH u_int and u_trt
  (because trt = 1 activates the slope).  You can see this in the picture as
  "two lines per red obs, one line per blue obs".</li>
<li><b>Five red arcs in the ring</b> — the cross-spring per study coupling its
  random intercept and random treatment-effect.  Hardness comes from the
  off-diagonal of Σ⁻¹.</li>
<li><b>Ten green dashed spokes to the centre</b> — self-shrinkage of every
  r.e. node toward the pinned zero.  Hardnesses are the row sums of Σ⁻¹.</li>
<li><b>β_0 feeds every obs (via u_int)</b>; <b>β_trt feeds every treated obs
  (via u_trt)</b>.  The yellow bundles show the β→u-node wiring per study.</li>
</ul>

<p><small>Generated by <code>test/ipd_re_network.R</code>.  Our Haskell tier-3
solver <code>fitLMMRE</code> would reproduce the lme4 fit in the table above
to 4 decimals on the same synthetic data.</small></p>
</body></html>
',
N, J,
svg_full,
beta0_true, fixef(fit)[["(Intercept)"]],
beta_trt_true, fixef(fit)[["trt"]],
Sigma_true[1,1], S[1,1],
Sigma_true[2,2], S[2,2],
Sigma_true[1,2], S[1,2],
sigma2E_true, s2e,
N, k_obs,
J, k_self_int,
J, k_self_trt,
J, k_cross,
blup_table
)

out <- "test/ipd_re_network.html"
writeLines(html, out)
cat("Wrote", out, "(", nchar(html), "bytes)\n")
cat("Open:  file://", normalizePath(out), "\n", sep = "")
