#!/usr/bin/env python3
"""Reconstruct arm-level data from test/fallers.csv (contrast-format
NMA), correctly handling multi-arm trials by picking one common
reference arm per study and emitting K arm rows per K-arm trial.

For each unique study id:
  1. Collect all contrasts (t1, t2, eff, se).
  2. Build the implied arm graph; verify it's connected.
  3. Pick the treatment that appears in the most contrasts as the
     reference arm `r` for that study.
  4. Build a spanning tree of contrasts rooted at `r`; for each
     non-reference arm `t`, find the contrast that gives effect(r→t)
     directly or by adding intermediate steps.
  5. Emit K arm rows: arm `r` with mean = 0, arms `t` with mean =
     effect(r→t), and per-arm sd derived from the contrast SE.

Per-arm sd: for a balanced trial with shared within-arm variance σ²,
all reported contrast SEs σ_ab equal sqrt(2σ²/n), so σ²_arm = σ²/n =
σ_ab²/2. We use sd_per_arm = mean(reported SE)/√2 across this study's
contrasts. This matches how gc-rust's spring formulation expects
v_arm = σ²_arm/n_arm such that Var(contrast) = v_a + v_b = σ²_ab.

For pairwise (2-arm) studies the construction is identical to the
naive split-contrast approach we used before. The change only
matters for the 29 multi-arm trials.
"""

import csv, json, math
from collections import Counter, defaultdict
from pathlib import Path

ROOT = Path(__file__).resolve().parent
src = "/Users/tosku/Sync/Documents/slmm/test/fallers.csv"
rows = list(csv.DictReader(open(src)))
by_study = defaultdict(list)
for r in rows:
    eff = float(r["effect"]); se = float(r["se"])
    if not (math.isfinite(eff) and math.isfinite(se) and se > 0):
        continue
    by_study[r["id"]].append((r["t1"], r["t2"], eff, se))

arms_out = []
multi_count = 0
star_count  = 0
problem_studies = []

for study_idx, (sid, contrasts) in enumerate(sorted(by_study.items()), start=1):
    treats = set()
    for t1, t2, _, _ in contrasts:
        treats.add(t1); treats.add(t2)
    K = len(treats)

    if K == 2:
        # plain pairwise — split as before
        t1, t2, eff, se = contrasts[0]
        sd = se / math.sqrt(2.0)
        arms_out.append({"study": study_idx, "treatment": t1, "mean": 0.0, "sd": sd, "n": 1})
        arms_out.append({"study": study_idx, "treatment": t2, "mean": eff, "sd": sd, "n": 1})
        continue

    # multi-arm: pick reference = treatment appearing most often
    multi_count += 1
    counts = Counter()
    for t1, t2, _, _ in contrasts:
        counts[t1] += 1; counts[t2] += 1
    ref = counts.most_common(1)[0][0]

    # Build a spanning tree from ref: BFS over treatments
    # adjacency: treatment → list of (neighbour, signed_effect, se)
    adj = defaultdict(list)
    for t1, t2, eff, se in contrasts:
        adj[t1].append((t2,  eff, se))
        adj[t2].append((t1, -eff, se))

    # BFS to get effect(ref → t) for each treatment t
    eff_from_ref = {ref: 0.0}
    se_from_ref  = {ref: 0.0}   # accumulated SE (will use mean-SE/√2 instead)
    queue = [ref]; visited = {ref}
    while queue:
        cur = queue.pop(0)
        for nbr, dy, _ in adj[cur]:
            if nbr in visited: continue
            eff_from_ref[nbr] = eff_from_ref[cur] + dy
            visited.add(nbr); queue.append(nbr)

    if len(eff_from_ref) < K:
        problem_studies.append((sid, K, len(eff_from_ref)))
        # disconnected — fall back to per-row split
        for ci, (t1, t2, eff, se) in enumerate(contrasts):
            sd = se / math.sqrt(2.0)
            arms_out.append({"study": f"{study_idx}_x{ci}", "treatment": t1, "mean": 0.0, "sd": sd, "n": 1})
            arms_out.append({"study": f"{study_idx}_x{ci}", "treatment": t2, "mean": eff, "sd": sd, "n": 1})
        continue

    # Per-arm sd from average reported SE across contrasts of this trial
    avg_se = sum(c[3] for c in contrasts) / len(contrasts)
    sd_per_arm = avg_se / math.sqrt(2.0)
    star_count += 1

    # Emit K arms
    for t in sorted(treats):
        arms_out.append({"study": study_idx, "treatment": t,
                         "mean": eff_from_ref[t], "sd": sd_per_arm, "n": 1})

# Write
out = ROOT / "data" / "fallers_armlevel.json"
with open(out, "w") as f:
    json.dump(arms_out, f)

print(f"Studies: {len(by_study)}  (pairwise: {len(by_study)-multi_count}, multi-arm: {multi_count})")
print(f"Multi-arm reconstructed correctly via star: {star_count}")
print(f"Fallback split-row (disconnected adjacency): {len(problem_studies)}")
if problem_studies:
    print(f"  examples:", problem_studies[:5])
print(f"Total arm rows emitted: {len(arms_out)}")
print(f"Wrote {out}")
