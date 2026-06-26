#!/usr/bin/env python3
"""Merge the M1/M2 (results.json) + saved M3/M4/M5 (/tmp/bm_345.json) benchmark
results, rewrite the deck's COV & SPEED blocks, and regenerate the report."""
import json, re, subprocess
ROOT = "/Users/tosku/Sync/Documents/meta-springs"
res = json.load(open(f"{ROOT}/test/springs_benchmark_results.json"))   # M1/M2
res.update(json.load(open("/tmp/bm_345.json")))                        # M3/M4/M5
json.dump(res, open(f"{ROOT}/test/springs_benchmark_results.json", "w"), indent=1)

order = ["M1", "M2", "M3", "M4", "M5"]
covlbl = {"M1":"NMA binary (OR), RE","M2":"NMA continuous (MD), RE",
          "M3":"Baseline-risk shared, FE","M4":"Baseline-risk shared, RE",
          "M5":"Baseline-risk exch (GC)"}
cov_lines = []
for M in order:
    arr = [round(r[3], 1) for r in res[M]]
    pad = " " * max(1, 27 - len(covlbl[M]) - 3)
    cov_lines.append(f'  "{covlbl[M]}":{pad}[{",".join(str(x) for x in arr)}],')
cov_block = "const COV = {\n" + "\n".join(cov_lines) + "\n};"

def fmt(ms): return f"~{ms/1000:.1f} s" if ms >= 1000 else (f"{ms:.0f} ms" if ms >= 10 else f"{ms:.1f} ms")
splbl = {"M1":("NMA binary (OR), RE","gemtc ~sec–min"),
         "M2":("NMA continuous (MD), RE","gemtc ~sec–min"),
         "M3":("Baseline-risk shared, FE","bnma ~sec–min"),
         "M4":("Baseline-risk shared, RE","bnma ~sec–min"),
         "M5":("Baseline-risk exch, 2-D GC","bnma ~min")}
sp_lines = []
for M in order:
    r = res[M]; lbl, mcmc = splbl[M]
    sp_lines.append(f'  "{lbl}": {{t4:"{fmt(r[0][2])}", t16:"{fmt(r[2][2])}", t40:"{fmt(r[5][2])}", mcmc:"{mcmc}"}},')
speed_block = "const SPEED = {\n  // model: {label, t4, t16, t40, mcmc}\n" + "\n".join(sp_lines) + "\n};"

html = open(f"{ROOT}/test/springs_presentation.html").read()
html = re.sub(r"const COV = \{.*?\};", cov_block, html, flags=re.S)
html = re.sub(r"const SPEED = \{.*?\};", speed_block, html, flags=re.S)
open(f"{ROOT}/test/springs_presentation.html", "w").write(html)
subprocess.run(["python3", f"{ROOT}/test/springs_benchmark_report.py"], check=True)
print("deck + report refreshed\n"); print(cov_block)
