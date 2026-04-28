#!/usr/bin/env python3
"""Aggregate bias of τ² and effects over 20 replicates."""
import json
import statistics as st

truth = json.load(open("/tmp/reps/truth.json"))
gc = json.load(open("/tmp/reps_gc.json"))
mn = json.load(open("/tmp/reps_mn.json"))

T2 = float(truth["tau2_contrast_true"])
D  = list(map(float, truth["d_true"]))
def truth_pair(a, b): return D[int(b)-1] - D[int(a)-1]

print(f"Truth: τ²_contrast = {T2:.4f}    d_true = {[round(x,4) for x in D]}")
print(f"Reps: {len(gc['reps'])}\n")

# τ² bias distribution
def biases(reps, key):
    return [r[key] - T2 for r in reps]

print(f"{'method':<10} {'metric':<10} {'mean bias':>12} {'sd':>10} {'min':>10} {'max':>10}")
print("-" * 70)
for nm, reps in [("GC", gc["reps"]), ("multinma", mn["reps"])]:
    for key, label in [("tau2_mode","τ² mode"), ("tau2_median","τ² median"),
                       ("tau2_mean","τ² mean")]:
        bs = biases(reps, key)
        print(f"{nm:<10} {label:<10} {st.mean(bs):>+12.4f} {st.stdev(bs):>10.4f} "
              f"{min(bs):>+10.4f} {max(bs):>+10.4f}")
# REML across all 20 reps (frequentist baseline)
remls = [r["reml"] for r in gc["reps"]]
remlBs = [r - T2 for r in remls]
print(f"{'REML':<10} {'mode':<10} {st.mean(remlBs):>+12.4f} {st.stdev(remlBs):>10.4f} "
      f"{min(remlBs):>+10.4f} {max(remlBs):>+10.4f}")

# Effect bias
def eff_biases(reps):
    out = []
    for r in reps:
        for e in r["effects"]:
            a, b = e["label"].split("->")
            out.append(e["mean"] - truth_pair(a, b))
    return out

print()
print(f"{'method':<10} {'metric':<14} {'mean bias':>12} {'sd':>10} {'RMSE':>10}")
print("-" * 60)
for nm, reps in [("GC", gc["reps"]), ("multinma", mn["reps"])]:
    bs = eff_biases(reps)
    rmse = (sum(b*b for b in bs) / len(bs)) ** 0.5
    print(f"{nm:<10} {'effect mean':<14} {st.mean(bs):>+12.4f} {st.stdev(bs):>10.4f} "
          f"{rmse:>10.4f}")

# Per-rep paired GC vs multinma
print()
print(f"Per-rep paired |GC mean − multinma mean| on τ²:")
ts = [(g["tau2_mean"], m["tau2_mean"]) for g, m in zip(gc["reps"], mn["reps"])]
diffs = [abs(g - m) for g, m in ts]
print(f"  mean = {st.mean(diffs):.4f}   max = {max(diffs):.4f}")
