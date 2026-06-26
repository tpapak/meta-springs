#!/usr/bin/env python3
"""Simulation + benchmark for the springs engines (gemtc + bnma facades).

For each model family it generates data from that model's own DGP at a known
truth, fits with the verified Rust engine, and reports fit speed and 95%
credible-interval coverage as the number of treatments T scales up to 40.

Models:
  M1 NMA binary (OR), random effects            -> mtc      (gemtc facade)
  M2 NMA continuous (MD), random effects         -> mtc      (gemtc facade)
  M3 baseline-risk binary, shared slope, FE      -> brisk    (bnma facade)
  M4 baseline-risk binary, shared slope, RE      -> brisk    (bnma facade)
  M5 baseline-risk binary, exchangeable, 2-D GC  -> brisk gc (bnma facade)

Coverage = fraction of (rep x parameter) where the true value lies in the
[2.5%, 97.5%] quantile band of the posterior draws.
"""
import json, subprocess, tempfile, os, time, sys
import numpy as np

ROOT = "/Users/tosku/Sync/Documents/meta-springs"
MTC = f"{ROOT}/gc-rust/target/release/mtc"
BRISK = f"{ROOT}/gc-rust/target/release/brisk"

def sigmoid(x): return 1.0 / (1.0 + np.exp(-x))
def tname(t): return f"T{t:02d}"          # lexicographic == numeric; ref = T00

# ---------------------------------------------------------------- generators
def gen_nma(T, rng, binary, tau2=0.10, reps_per=2, narm=100):
    """Star + light extra topology; arm-level RE. truth d_t = linspace."""
    d = np.concatenate([[0.0], np.linspace(0.3, 1.6, T - 1)])   # d[0]=ref
    tau_arm = np.sqrt(tau2 / 2)
    recs, sid = [], 0
    for t in range(1, T):                       # ref vs t, reps_per studies
        for _ in range(reps_per):
            sid += 1
            mu = rng.normal(-0.4, 0.4)          # study baseline
            for tr in (0, t):
                lp = mu + d[tr] + rng.normal(0, tau_arm)
                if binary:
                    p = sigmoid(lp); e = rng.binomial(narm, p)
                    recs.append(dict(study=sid, treatment=tname(tr), events=int(e), n=narm))
                else:
                    y = lp + rng.normal(0, 1/np.sqrt(narm))
                    recs.append(dict(study=sid, treatment=tname(tr), mean=float(y),
                                     sd=1.0, n=narm))
    # a few active-active studies for connectivity
    for k in range(max(2, T // 4)):
        a, b = rng.integers(1, T), rng.integers(1, T)
        if a == b: continue
        sid += 1; mu = rng.normal(-0.4, 0.4)
        for tr in (a, b):
            lp = mu + d[tr] + rng.normal(0, tau_arm)
            if binary:
                p = sigmoid(lp); e = rng.binomial(narm, p)
                recs.append(dict(study=sid, treatment=tname(tr), events=int(e), n=narm))
            else:
                y = lp + rng.normal(0, 1/np.sqrt(narm))
                recs.append(dict(study=sid, treatment=tname(tr), mean=float(y), sd=1.0, n=narm))
    return recs, d

def gen_brisk(T, rng, regressor, B=-0.5, sigB2=0.06, tau2=0.0, reps_per=3, narm=200):
    """Baseline-risk DGP: logit p = eta_i + d_t + b_t (eta_i - mx). control=ref."""
    d = np.concatenate([[0.0], np.linspace(0.4, 1.4, T - 1)])
    if regressor == "exchangeable":
        b = np.concatenate([[0.0], rng.normal(B, np.sqrt(sigB2), T - 1)])
    else:
        b = np.concatenate([[0.0], np.full(T - 1, B)])
    # generate study baselines with spread (needed to identify the slope)
    eta_all = []
    recs, sid = [], 0
    tau_arm = np.sqrt(tau2 / 2) if tau2 > 0 else 0.0
    studies = []
    for t in range(1, T):
        for _ in range(reps_per):
            sid += 1; eta = rng.normal(0.0, 0.8); eta_all.append(eta)
            studies.append((sid, eta, t))
    mx = float(np.mean(eta_all))
    for (sid, eta, t) in studies:
        # control (ref)
        u0 = rng.normal(0, tau_arm) if tau_arm else 0.0
        p0 = sigmoid(eta + u0); e0 = rng.binomial(narm, p0)
        recs.append(dict(study=f"S{sid}", treatment=tname(0), events=int(e0), n=narm))
        # active t
        ut = rng.normal(0, tau_arm) if tau_arm else 0.0
        lp = eta + d[t] + b[t] * (eta - mx) + ut
        pt = sigmoid(lp); et = rng.binomial(narm, pt)
        recs.append(dict(study=f"S{sid}", treatment=tname(t), events=int(et), n=narm))
    return recs, dict(d=d, b=b, B=B)

# ------------------------------------------------------------------- runners
def write(recs):
    f = tempfile.NamedTemporaryFile("w", suffix=".json", delete=False)
    json.dump(recs, f); f.close(); return f.name

def run_mtc(recs, prior, samples=4000, seed=1):
    inp, out = write(recs), tempfile.mktemp(suffix=".json")
    t0 = time.time()
    r = subprocess.run([MTC, "--input", inp, "--output", out, "--prior", prior,
                        "--samples", str(samples), "--seed", str(seed)],
                       capture_output=True, text=True)
    wall = time.time() - t0
    os.unlink(inp)
    if r.returncode != 0 or not os.path.exists(out):
        raise RuntimeError("mtc failed: " + r.stderr[:200])
    rep = json.load(open(out)); os.unlink(out)
    return rep, rep.get("fit_wall_sec", wall)

def run_brisk(recs, regressor, effects, method, samples=4000, seed=1):
    inp, out = write(recs), tempfile.mktemp(suffix=".json")
    t0 = time.time()
    r = subprocess.run([BRISK, "--input", inp, "--output", out, "--ref", tname(0),
                        "--regressor", regressor, "--effects", effects, "--method", method,
                        "--samples", str(samples), "--seed", str(seed)],
                       capture_output=True, text=True)
    wall = time.time() - t0
    os.unlink(inp)
    if r.returncode != 0 or not os.path.exists(out):
        raise RuntimeError("brisk failed: " + r.stderr[:200])
    rep = json.load(open(out)); os.unlink(out)
    return rep, rep.get("fit_wall_sec", wall)

# ----------------------------------------------------------------- coverage
def cover_cols(draws, truth, treatments):
    """draws: list of rows aligned to `treatments`; truth aligned to treatments.
    Returns (#covered, #params, abs-bias-sum) over non-ref columns."""
    A = np.array(draws)
    lo = np.percentile(A, 2.5, axis=0); hi = np.percentile(A, 97.5, axis=0)
    med = np.percentile(A, 50, axis=0)
    cov = npar = 0; bias = 0.0
    for k, tr in enumerate(treatments):
        if tr == tname(0):  # ref column is identically 0
            continue
        npar += 1
        if lo[k] - 1e-9 <= truth[k] <= hi[k] + 1e-9: cov += 1
        bias += abs(med[k] - truth[k])
    return cov, npar, bias

def nma_cover(rep, truth_d):
    # rep param_names: d.T00.Tkk ; reorder to treatments order
    pn = rep["param_names"]; A = np.array(rep["draws"])
    cov = npar = 0; bias = 0.0
    for j, name in enumerate(pn):
        t = name.split(".")[-1]; idx = int(t[1:])
        col = A[:, j]
        lo, hi, md = np.percentile(col, [2.5, 97.5, 50])
        npar += 1
        if lo - 1e-9 <= truth_d[idx] <= hi + 1e-9: cov += 1
        bias += abs(md - truth_d[idx])
    return cov, npar, bias

def brisk_cover(rep, truth, which):
    trts = rep["treatments"]
    draws = rep["d_draws"] if which == "d" else rep["b_draws"]
    tvec = truth["d"] if which == "d" else truth["b"]
    # treatments are Tnn; align truth by numeric index
    tr_truth = np.array([tvec[int(t[1:])] for t in trts])
    return cover_cols(draws, tr_truth, trts)

# -------------------------------------------------------------------- sweep
def main():
    Ts = [4, 8, 16, 24, 32, 40]
    NREPS = {4: 100, 8: 100, 16: 80, 24: 60, 32: 50, 40: 40}
    models = sys.argv[1:] or ["M1", "M2", "M3", "M4", "M5"]
    results = {}
    for M in models:
        rows = []
        for T in Ts:
            n = NREPS[T]
            cov = npar = 0; bias = 0.0; ftimes = []
            for rep_i in range(n):
                rng = np.random.default_rng(1000 * Ts.index(T) + rep_i + hash(M) % 997)
                try:
                    if M == "M1":
                        recs, d = gen_nma(T, rng, binary=True)
                        rep, ft = run_mtc(recs, "flat", seed=rep_i + 1)
                        c, p, b = nma_cover(rep, d)
                    elif M == "M2":
                        recs, d = gen_nma(T, rng, binary=False)
                        rep, ft = run_mtc(recs, "flat", seed=rep_i + 1)
                        c, p, b = nma_cover(rep, d)
                    elif M == "M3":
                        recs, tr = gen_brisk(T, rng, "common", tau2=0.0)
                        rep, ft = run_brisk(recs, "common", "fixed", "gc", seed=rep_i + 1)
                        c, p, b = brisk_cover(rep, tr, "b")
                    elif M == "M4":
                        recs, tr = gen_brisk(T, rng, "common", tau2=0.08)
                        rep, ft = run_brisk(recs, "common", "random", "gc", seed=rep_i + 1)
                        c, p, b = brisk_cover(rep, tr, "b")
                    elif M == "M5":
                        recs, tr = gen_brisk(T, rng, "exchangeable", sigB2=0.06, tau2=0.0)
                        rep, ft = run_brisk(recs, "exchangeable", "fixed", "gc", seed=rep_i + 1)
                        c, p, b = brisk_cover(rep, tr, "b")
                    cov += c; npar += p; bias += b; ftimes.append(ft)
                except Exception as ex:
                    print(f"  {M} T={T} rep={rep_i} ERR {ex}", file=sys.stderr)
            covpct = 100.0 * cov / max(1, npar)
            mft = 1000.0 * np.median(ftimes) if ftimes else float("nan")
            mbias = bias / max(1, npar)
            rows.append((T, n, mft, covpct, mbias))
            print(f"{M} T={T:2d} n={n:3d}  fit={mft:8.2f}ms  cover95={covpct:5.1f}%  |bias|={mbias:.3f}")
            sys.stdout.flush()
        results[M] = rows
    json.dump(results, open(f"{ROOT}/test/springs_benchmark_results.json", "w"), indent=1)
    print("\nwrote test/springs_benchmark_results.json")

if __name__ == "__main__":
    main()
