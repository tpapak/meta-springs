# Render the full tier-3 spring network for sleepstudy as an SVG.
# Every node and every spring is drawn — 180 obs + 38 free + 2 pinned centers = 220 nodes,
# 234 springs (180 obs + 36 self + 18 cross).
#
# Layout: polar.
#   centre (cx, cy)                    ← β_Int, β_Days, pinned 0
#   inner ring (radius r_u)            ← 36 u nodes, arranged by subject
#   outer ring (radius r_y)            ← 180 y data anchors, one per (subject,day)
#
# Run:  Rscript test/sleepstudy_re_network.R
# Out:  test/sleepstudy_re_network.html

suppressPackageStartupMessages(library(lme4))

fit  <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy, REML = TRUE,
             control = lmerControl(optimizer = "bobyqa"))
vc   <- as.data.frame(VarCorr(fit))
S    <- matrix(0, 2, 2)
S[1,1] <- vc$vcov[vc$grp == "Subject" & vc$var1 == "(Intercept)" & is.na(vc$var2)]
S[2,2] <- vc$vcov[vc$grp == "Subject" & vc$var1 == "Days"        & is.na(vc$var2)]
S[1,2] <- S[2,1] <- vc$vcov[vc$grp == "Subject" & !is.na(vc$var2)]
s2e  <- vc$vcov[vc$grp == "Residual"]
Si   <- solve(S)

k_obs        <- 1 / s2e
k_self_int   <- Si[1,1] + Si[1,2]
k_self_slope <- Si[2,2] + Si[1,2]
k_cross      <- -Si[1,2]

blups    <- ranef(fit)$Subject
subj_ids <- rownames(blups)
u_int    <- blups[, "(Intercept)"]
u_days   <- blups[, "Days"]
J <- nrow(blups)              # 18
N <- nrow(sleepstudy)         # 180

# --- layout ---
W  <- 1000; H <- 1000
cx <- W / 2; cy <- H / 2
r_u <- 220
r_y <- 400

# 18 subjects at equal angles
theta_s <- seq(0, 2 * pi, length.out = J + 1)[-1] - pi / 2   # start at top, go clockwise
# each subject has 2 u nodes side-by-side (tangential offset)
u_sep   <- 24
u_int_pos <- data.frame(
  x = cx + r_u * cos(theta_s) - u_sep * sin(theta_s),
  y = cy + r_u * sin(theta_s) + u_sep * cos(theta_s)
)
u_slope_pos <- data.frame(
  x = cx + r_u * cos(theta_s) + u_sep * sin(theta_s),
  y = cy + r_u * sin(theta_s) - u_sep * cos(theta_s)
)

# 10 observations per subject — small arc on the outer ring
day_span <- 0.32  # radians
day_pos <- list()
obs_subj_ix <- integer(N)
obs_day     <- integer(N)
y_vals      <- sleepstudy$Reaction
ii <- 1
for (s in seq_len(J)) {
  for (d in 0:9) {
    ang <- theta_s[s] + (d - 4.5) / 9 * day_span
    day_pos[[ii]] <- c(x = cx + r_y * cos(ang), y = cy + r_y * sin(ang))
    obs_subj_ix[ii] <- s
    obs_day[ii]     <- d
    ii <- ii + 1
  }
}
day_pos <- do.call(rbind, day_pos)

# β and zero at the centre (slight offset so they don't overlap)
b_int_pos  <- c(cx - 22, cy)
b_days_pos <- c(cx + 22, cy)
zero_pos   <- c(cx, cy - 42)

# --- SVG string building ---
# obs springs: one per observation, drawn y_i → subject's u_int (thin, faded)
obs_lines <- character(N)
for (i in seq_len(N)) {
  s <- obs_subj_ix[i]
  obs_lines[i] <- sprintf(
    '<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" stroke="#1f77b4" stroke-opacity="0.18" stroke-width="0.8"/>',
    day_pos[i, 1], day_pos[i, 2],
    u_int_pos$x[s], u_int_pos$y[s]
  )
}

# β-wires: for each subject, 2 thin yellow lines from β_int and β_days to its u_int
# (schematic — stands for the X_i·β part of η_i; one bundle per subject instead of 180)
beta_wires <- character(2 * J)
for (s in seq_len(J)) {
  beta_wires[2*s - 1] <- sprintf(
    '<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" stroke="#b08040" stroke-opacity="0.30" stroke-width="0.6"/>',
    b_int_pos[1],  b_int_pos[2],  u_int_pos$x[s], u_int_pos$y[s]
  )
  beta_wires[2*s] <- sprintf(
    '<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" stroke="#b08040" stroke-opacity="0.30" stroke-width="0.6"/>',
    b_days_pos[1], b_days_pos[2], u_slope_pos$x[s], u_slope_pos$y[s]
  )
}

# self-shrinkage springs: every u node to the pinned zero (dashed green)
self_springs <- character(2 * J)
for (s in seq_len(J)) {
  self_springs[2*s - 1] <- sprintf(
    '<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" stroke="#2ca02c" stroke-dasharray="4,3" stroke-width="1.1" opacity="0.6"/>',
    u_int_pos$x[s], u_int_pos$y[s], zero_pos[1], zero_pos[2]
  )
  self_springs[2*s] <- sprintf(
    '<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" stroke="#2ca02c" stroke-dasharray="4,3" stroke-width="1.1" opacity="0.6"/>',
    u_slope_pos$x[s], u_slope_pos$y[s], zero_pos[1], zero_pos[2]
  )
}

# cross-springs: arc between each subject's u_int and u_slope (solid magenta)
cross_springs <- character(J)
for (s in seq_len(J)) {
  # midpoint + small radial offset outward for the quadratic curve control point
  mx <- (u_int_pos$x[s] + u_slope_pos$x[s]) / 2
  my <- (u_int_pos$y[s] + u_slope_pos$y[s]) / 2
  # offset the control point outward (away from centre) so curve is visible
  dx <- mx - cx; dy <- my - cy
  nrm <- sqrt(dx^2 + dy^2)
  cxp <- mx + 22 * dx / nrm
  cyp <- my + 22 * dy / nrm
  cross_springs[s] <- sprintf(
    '<path d="M %.1f %.1f Q %.1f %.1f %.1f %.1f" stroke="#d62728" stroke-width="1.8" fill="none"/>',
    u_int_pos$x[s], u_int_pos$y[s], cxp, cyp,
    u_slope_pos$x[s], u_slope_pos$y[s]
  )
}

# nodes
# y anchors (pinned, yellow squares, small)
y_nodes <- character(N)
for (i in seq_len(N)) {
  y_nodes[i] <- sprintf(
    '<rect x="%.1f" y="%.1f" width="5" height="5" fill="#d4c25c" stroke="#8a7a30" stroke-width="0.4"/>',
    day_pos[i, 1] - 2.5, day_pos[i, 2] - 2.5
  )
}

# u nodes (blue = intercept, teal = slope)
u_nodes <- character(2 * J)
for (s in seq_len(J)) {
  # colour self-spring end by BLUP sign
  col_int  <- if (u_int[s]  >= 0) "#1a5490" else "#3d80b8"
  col_slp  <- if (u_days[s] >= 0) "#178a6d" else "#4ab295"
  u_nodes[2*s - 1] <- sprintf(
    '<circle cx="%.1f" cy="%.1f" r="7" fill="%s" stroke="#06294a" stroke-width="0.8"/><title>%s u_int=%.2f</title>',
    u_int_pos$x[s],  u_int_pos$y[s],  col_int, subj_ids[s], u_int[s]
  )
  u_nodes[2*s] <- sprintf(
    '<circle cx="%.1f" cy="%.1f" r="7" fill="%s" stroke="#083f2f" stroke-width="0.8"/><title>%s u_slope=%.2f</title>',
    u_slope_pos$x[s], u_slope_pos$y[s], col_slp, subj_ids[s], u_days[s]
  )
}

# subject labels on outside of ring
subj_labels <- character(J)
for (s in seq_len(J)) {
  lx <- cx + (r_u + 45) * cos(theta_s[s])
  ly <- cy + (r_u + 45) * sin(theta_s[s]) + 4
  subj_labels[s] <- sprintf(
    '<text x="%.1f" y="%.1f" font-size="10" fill="#444" text-anchor="middle">%s</text>',
    lx, ly, subj_ids[s]
  )
}

# β and zero central nodes
center_nodes <- sprintf('
  <circle cx="%.1f" cy="%.1f" r="14" fill="#d42a2a" stroke="#821111" stroke-width="1.2"/>
  <text x="%.1f" y="%.1f" font-size="11" fill="#fff" text-anchor="middle" font-weight="bold">β_I</text>
  <circle cx="%.1f" cy="%.1f" r="14" fill="#d42a2a" stroke="#821111" stroke-width="1.2"/>
  <text x="%.1f" y="%.1f" font-size="10" fill="#fff" text-anchor="middle" font-weight="bold">β_D</text>
  <rect   x="%.1f" y="%.1f" width="16" height="12" fill="#bbb" stroke="#555" stroke-dasharray="3,2"/>
  <text   x="%.1f" y="%.1f" font-size="10" fill="#222" text-anchor="middle">0</text>
',
b_int_pos[1],  b_int_pos[2],  b_int_pos[1],  b_int_pos[2] + 4,
b_days_pos[1], b_days_pos[2], b_days_pos[1], b_days_pos[2] + 4,
zero_pos[1] - 8, zero_pos[2] - 6, zero_pos[1], zero_pos[2] + 4
)

svg_full <- sprintf('
<svg viewBox="0 0 %d %d" xmlns="http://www.w3.org/2000/svg"
     style="background:#fafafa;border:1px solid #ccc;border-radius:6px">
  <!-- spring layers (back to front) -->
  <g id="obs-springs">%s</g>
  <g id="beta-wires">%s</g>
  <g id="self-springs">%s</g>
  <g id="cross-springs">%s</g>
  <!-- nodes -->
  <g id="y-anchors">%s</g>
  <g id="u-nodes">%s</g>
  %s
  <g id="subj-labels">%s</g>

  <g transform="translate(20,20)" font-family="sans-serif">
    <text x="0" y="0" font-size="14" font-weight="bold">sleepstudy tier-3 spring network</text>
    <text x="0" y="18" font-size="11" fill="#555">180 obs springs (blue, faded) · 36 self (green dashed) · 18 cross (red) · 2 β wires per subject (yellow)</text>
    <text x="0" y="33" font-size="11" fill="#555">centre: β_Int, β_Days, pinned 0 · ring of 36 u-nodes (blue = intercept, teal = slope) · outer: 180 y anchors</text>
  </g>
</svg>',
W, H,
paste(obs_lines, collapse = "\n"),
paste(beta_wires, collapse = "\n"),
paste(self_springs, collapse = "\n"),
paste(cross_springs, collapse = "\n"),
paste(y_nodes, collapse = "\n"),
paste(u_nodes, collapse = "\n"),
center_nodes,
paste(subj_labels, collapse = "\n")
)

# --- wrap in HTML ---
html <- sprintf('<!DOCTYPE html>
<html><head><meta charset="utf-8"><title>Tier-3 spring network</title>
<style>
  body { font-family:-apple-system,sans-serif; max-width:1080px; margin:1em auto; padding:0 1em; color:#222 }
  h1 { border-bottom:2px solid #e0e3e8; padding-bottom:.3em }
  .meta { color:#555 }
  table { border-collapse:collapse; margin-top:.6em }
  th,td { border:1px solid #ddd; padding:3px 9px; font-variant-numeric:tabular-nums }
  th { background:#f2f4f7; text-align:left }
  td.n { text-align:right }
</style></head><body>

<h1>Tier-3 spring network — sleepstudy <code>Reaction ~ Days + (Days|Subject)</code></h1>
<p class="meta">All 220 nodes and 234 springs drawn.  18 subjects arranged around the ring;
each subject has 2 r.e. nodes (intercept inside pair, slope outside) and 10 obs anchors
on the outer arc.  Hardnesses shown below come from the lme4 REML Σ⁻¹ at convergence.</p>

<p>
%s
</p>

<h2>Fit & hardnesses</h2>
<table>
<tr><th>quantity</th><th>value</th></tr>
<tr><td>β_Int / β_Days</td>         <td class="n">%.3f / %.3f</td></tr>
<tr><td>Σ[Int,Int] / Σ[Dy,Dy] / cov</td><td class="n">%.2f / %.2f / %.2f</td></tr>
<tr><td>σ²_resid</td>               <td class="n">%.2f</td></tr>
<tr><td>1/σ²_ε  (obs hardness)</td> <td class="n">%.5f</td></tr>
<tr><td>self on u_int       (×18)</td><td class="n">%.5f</td></tr>
<tr><td>self on u_slope     (×18)</td><td class="n">%.5f</td></tr>
<tr><td>cross int ↔ slope (×18)</td><td class="n">%.5f</td></tr>
</table>

<h2>Counts</h2>
<ul>
<li><b>Free nodes</b>: 2 β + %d u  =  <b>%d</b></li>
<li><b>Pinned anchors</b>: %d data y + 1 zero  =  <b>%d</b></li>
<li><b>Springs</b>: %d obs + %d self + %d cross  =  <b>%d</b></li>
</ul>

<p class="meta"><small>Generated by <code>test/sleepstudy_re_network.R</code>.
Hover a u-node in the SVG to see its BLUP.  β-wires are drawn as a bundle per
subject (18 pairs) rather than 180 individual wires per observation, to keep the
picture readable — the full linear predictor η_i = X_iβ + Z_iu still routes
through both β and u nodes, the SVG just shows one representative wire bundle.</small></p>
</body></html>
',
svg_full,
fixef(fit)[["(Intercept)"]], fixef(fit)[["Days"]],
S[1,1], S[2,2], S[1,2], s2e,
k_obs, k_self_int, k_self_slope, k_cross,
2 * J, 2 + 2 * J,
N, N + 1,
N, 2 * J, J, N + 2 * J + J
)

out <- "test/sleepstudy_re_network.html"
writeLines(html, out)
cat("Wrote", out, "(", nchar(html), "bytes)\n")
cat("Open:  file://", normalizePath(out), "\n", sep = "")
