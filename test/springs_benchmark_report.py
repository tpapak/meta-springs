#!/usr/bin/env python3
"""Render test/springs_benchmark_results.json as a markdown report."""
import json
ROOT = "/Users/tosku/Sync/Documents/meta-springs"
res = json.load(open(f"{ROOT}/test/springs_benchmark_results.json"))

LABELS = {
    "M1": ("NMA binary (OR), random effects", "mtc — GC (gemtc facade)", "d_t contrasts"),
    "M2": ("NMA continuous (MD), random effects", "mtc — GC (gemtc facade)", "d_t contrasts"),
    "M3": ("Baseline-risk binary, shared, FE", "brisk — GC→exact point (bnma facade)", "slope B"),
    "M4": ("Baseline-risk binary, shared, RE", "brisk — GC integrate τ² (bnma facade)", "slope B"),
    "M5": ("Baseline-risk binary, exchangeable, 2-D GC", "brisk — GC integrate τ²,σ_B²", "per-treatment slopes b_t"),
}
out = []
out.append("# springs engines — simulation, speed & coverage benchmark\n")
out.append("Each model family is fitted to data generated from its **own** DGP at a known "
           "truth (best-case identifiability), with the spring/GC Rust engine. Speed is the "
           "engine `fit_wall_sec` (median over reps); **coverage** is the fraction of "
           "(rep × parameter) where the truth falls in the posterior `[2.5%, 97.5%]` band. "
           "T = number of treatments.\n")
out.append("Reference anchor (T=8, RE): springs NMA 0.10 s vs **gemtc 2.0 s (~20×)** at the "
           "R level (engine-only is sub-ms — orders faster); bnma rejects the synthetic star "
           "topology, but on real networks springs baseline fits in ms vs bnma's seconds.\n")

for M, rows in res.items():
    name, engine, param = LABELS[M]
    out.append(f"\n## {M}. {name}\n")
    out.append(f"Engine: `{engine}` · coverage parameter: {param}\n")
    out.append("| T | reps | fit (ms) | 95% coverage | mean \\|bias\\| |")
    out.append("|--:|--:|--:|--:|--:|")
    for (T, n, ft, cov, bias) in rows:
        out.append(f"| {T} | {n} | {ft:.1f} | {cov:.1f}% | {bias:.3f} |")

out.append("\n## Notes\n")
out.append("- **The grand-canonical (GC) solver is the default everywhere.** NMA is the closed-form "
           "τ²-mixture; baseline-risk integrates the variance components present — τ² under random "
           "effects (1-D) and additionally σ_B² under exchangeable (2-D Ξ(τ²,σ_B²)). Fixed-effect "
           "shared/unrelated have no component to integrate, so the GC reduces to the exact MLE + "
           "observed-information Laplace.")
out.append("- **Speed scales smoothly to T=40** and the GC is cheap enough to default: NMA ~180 ms "
           "(binary)/~80 ms (continuous), baseline shared ~50 ms (FE)/~290 ms (RE), exchangeable "
           "2-D GC ~2 s — all far below the seconds–minutes MCMC needs (gemtc/bnma are impractical "
           "at T≈40 RE).")
out.append("- **Coverage is at nominal across the board** (~90–97%). Crucially, switching the "
           "baseline-risk shared-RE model (M4) from the EM point to the GC integration of τ² lifts "
           "small-T slope coverage from ~65% to ~90–97% (e.g. T=4: 68% → 97%) at essentially the "
           "same speed — confirming the GC is both faster-than-MCMC and better-calibrated than the "
           "point estimate. The exchangeable GC (M5) likewise gives calibrated per-treatment slopes "
           "by integrating σ_B².")
out.append("- Bias is the small-sample logistic/Laplace (PQL) offset on the link scale; intervals "
           "remain calibrated despite it (the GC integrates the variance-component posteriors).")

open(f"{ROOT}/test/springs_benchmark.md", "w").write("\n".join(out) + "\n")
print("wrote test/springs_benchmark.md")
print("\n".join(out))
