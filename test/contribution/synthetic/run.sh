#!/usr/bin/env bash
# Round-trip CINeMA / Rust / Haskell contribution-matrix comparison.
#
# Default backend: LOCAL R via cinema's `run_cli.R` — runs the exact
# same `runNMA` the cinema.med.auth.gr server does, so we get H +
# contribMatrix + studyContributions + everything else needed for a
# .cnm file without any network round-trip.
#
# Set CINEMA_REMOTE=1 to additionally hit https://cinema.med.auth.gr
# and assert local R matches the live API byte-for-byte.
#
# Pipeline:
#   1. generate.py builds a synthetic 5-treatment binary NMA payload.
#   2. Pipe it through ../cinema/backend/R/run_cli.R → CINeMA-shape JSON.
#   3. Optionally diff against the live API response.
#   4. Extract netmeta IV weights via local_cinema.R (multi-arm
#      Lu-Ades correction included).
#   5. Run our Rust + Haskell ports on H to reproduce contribMatrix.
#   6. Run both ports' study_contributions to reproduce studyContributions.
#   7. Diff Rust / Haskell vs CINeMA on both tables.

set -euo pipefail

cd "$(dirname "$0")/../../.."   # to slmm root
SYN=test/contribution/synthetic
CINEMA_R=../cinema/backend/R/run_cli.R

if [[ ! -f $CINEMA_R ]]; then
  echo "  $CINEMA_R not found — checkout the cinema repo alongside slmm/."
  exit 1
fi

echo "==> 1. generating synthetic payload"
python3 "$SYN/generate.py" > "$SYN/payload.json"
python3 -c "
import json
p = json.load(open('$SYN/payload.json'))
p['action'] = 'runNMA'
json.dump(p, open('$SYN/runnma_request.json','w'))"

echo "==> 2. running CINeMA's run_cli.R locally"
Rscript "$CINEMA_R" < "$SYN/runnma_request.json" \
  > "$SYN/local_cinema_response.json" 2>/dev/null
echo "  wrote local_cinema_response.json ($(wc -c < $SYN/local_cinema_response.json) bytes)"

if [[ ${CINEMA_REMOTE:-0} == 1 ]]; then
  echo "==> 2b. cross-checking against cinema.med.auth.gr"
  curl -sS -m 60 -X POST https://cinema.med.auth.gr/api/runNMA \
    -H 'Content-Type: application/json' \
    --data @"$SYN/payload.json" \
    -o "$SYN/cinema_response.json" \
    -w "  HTTP %{http_code}, %{size_download}B, %{time_total}s\n"
  python3 - << 'PY'
import json
api = json.load(open('test/contribution/synthetic/cinema_response.json'))
loc = json.load(open('test/contribution/synthetic/local_cinema_response.json'))
def flat(H): return {(r,c):float(v) for r,row in zip(H["rowNames"],H["data"]) for c,v in row.items()}
mH = max(abs(flat(api["H"]).get(k,0)-flat(loc["H"]).get(k,0))
         for k in set(flat(api["H"]))|set(flat(loc["H"])))
mC = max(abs(flat(api["contribMatrix"]).get(k,0)-flat(loc["contribMatrix"]).get(k,0))
         for k in set(flat(api["contribMatrix"]))|set(flat(loc["contribMatrix"])))
print(f"  local-R vs live-API: max |H| = {mH:.2e}, max |contribMatrix| = {mC:.2e}")
PY
fi

echo "==> 3. extracting hat matrix + cinema contribution into wide form"
python3 - << 'PY'
import json
d = json.load(open('test/contribution/synthetic/local_cinema_response.json'))
H = d["H"]
json.dump([{"row":r,"comparison":c,"value":float(v)}
           for r,row in zip(H["rowNames"],H["data"]) for c,v in row.items()],
          open('test/contribution/synthetic/hatmatrix.json','w'))
cm = d["contribMatrix"]
json.dump([{"row":r,"comparison":c,"value":float(v)}
           for r,row in zip(cm["rowNames"],cm["data"]) for c,v in row.items()],
          open('test/contribution/synthetic/cinema_contrib.json','w'))
PY

echo "==> 4. extracting netmeta IV weights via R"
Rscript "$SYN/local_cinema.R" 2>&1 | tail -1

echo "==> 5. running Rust port (contrast contributions)"
( cd gc-rust && cargo build --release --quiet --bin contribution )
./gc-rust/target/release/contribution \
  --input  "$SYN/hatmatrix.json" \
  --output "$SYN/rust_contrib.json"

echo "==> 6. running Haskell port (contrast contributions)"
stack build --fast --silent
stack runghc --silent --package aeson --package bytestring --package containers \
  "$SYN/run_haskell.hs"

echo "==> 7. running Rust port (study contributions)"
./gc-rust/target/release/contribution \
  --input  "$SYN/hatmatrix.json" \
  --output /tmp/_cm.json \
  --study-weights "$SYN/iv_weights_netmeta.json" \
  --studies "$SYN/rust_study.json" >/dev/null

echo "==> 8. running Haskell port (study contributions)"
stack runghc --silent --package aeson --package bytestring --package containers --package text \
  "$SYN/run_haskell_studies.hs"

echo "==> 9. comparing"
python3 - << 'PY'
import json, sys
def load_w(p): return {(e["row"],e["comparison"]):float(e["value"]) for e in json.load(open(p))}
def load_s(p): return {(e["comparison"],str(e["study"])):float(e["contribution"]) for e in json.load(open(p))}

c  = load_w('test/contribution/synthetic/cinema_contrib.json')
r  = load_w('test/contribution/synthetic/rust_contrib.json')
h  = load_w('test/contribution/synthetic/haskell_contrib.json')
ck = sorted(set(c)|set(r)|set(h))
mxRC = max(abs(r.get(k,0)-c.get(k,0)) for k in ck)
mxHC = max(abs(h.get(k,0)-c.get(k,0)) for k in ck)
mxRH = max(abs(r.get(k,0)-h.get(k,0)) for k in ck)
print(f"  contrast contributions ({len(ck)} cells)")
print(f"    max |Rust    - CINeMA|  = {mxRC:.2e}")
print(f"    max |Haskell - CINeMA|  = {mxHC:.2e}")
print(f"    max |Rust    - Haskell| = {mxRH:.2e}")

cs = json.load(open('test/contribution/synthetic/local_cinema_response.json'))["studyContributions"]
cinema_s = {(x["comparison"],str(x["study"])):float(x["contribution"]) for x in cs}
rust_s = load_s('test/contribution/synthetic/rust_study.json')
hask_s = load_s('test/contribution/synthetic/haskell_study.json')
sk = sorted(set(cinema_s)|set(rust_s)|set(hask_s))
mxRC2 = max(abs(rust_s.get(k,0)-cinema_s.get(k,0)) for k in sk)
mxHC2 = max(abs(hask_s.get(k,0)-cinema_s.get(k,0)) for k in sk)
mxRH2 = max(abs(rust_s.get(k,0)-hask_s.get(k,0))   for k in sk)
print(f"  study   contributions ({len(sk)} cells)")
print(f"    max |Rust    - CINeMA|  = {mxRC2:.2e}")
print(f"    max |Haskell - CINeMA|  = {mxHC2:.2e}")
print(f"    max |Rust    - Haskell| = {mxRH2:.2e}")

ok = (mxRC < 1e-3 and mxHC < 1e-3 and mxRH < 1e-9
      and mxRC2 < 1e-3 and mxHC2 < 1e-3 and mxRH2 < 1e-9)
print("  PASS" if ok else "  FAIL"); sys.exit(0 if ok else 1)
PY
