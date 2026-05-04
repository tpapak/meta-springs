#!/usr/bin/env python3
"""Fractional multi-commodity max-throughput LP oracle.

Reads JSON: { n, edges: [[u, v, cap], ...], commodities: [[s, t], ...] }
Solves: max Σ_i F_i  s.t.  per-commodity flow conservation and Σ_i |f_i,e| ≤ c_e.

Linearised by introducing t_i,e ≥ |f_i,e|.

Usage: mc_lp_oracle.py <input.json>
Output JSON: { F_per: [...], total: ... }
"""
import json
import sys

import numpy as np
from scipy.optimize import linprog


def main():
    data = json.load(open(sys.argv[1]))
    n = int(data["n"])
    edges = [tuple(e) for e in data["edges"]]  # (u, v, cap), u < v canonical
    commodities = [tuple(c) for c in data["commodities"]]  # (s, t)

    m = len(edges)
    K = len(commodities)

    # Variable layout:
    # f_{i, e}        (K * m)   index = i * m + e        (signed flow)
    # t_{i, e} >= 0   (K * m)   index = K*m + i*m + e    (|f| envelope)
    # F_i >= 0        (K)       index = 2*K*m + i
    n_var = 2 * K * m + K

    def f_idx(i, e):
        return i * m + e

    def t_idx(i, e):
        return K * m + i * m + e

    def F_idx(i):
        return 2 * K * m + i

    # Objective: maximise Σ F_i  ->  minimise -Σ F_i
    c = np.zeros(n_var)
    for i in range(K):
        c[F_idx(i)] = -1.0

    # Equality: flow conservation per commodity per vertex.
    # For each (i, v): Σ_{e=(v,*)} f_{i,e} - Σ_{e=(*,v)} f_{i,e} - F_i·(δ(v=s) − δ(v=t)) = 0
    # Note: edges are stored as (u, v, cap) with u < v canonical. f_{i,e} is the
    # signed flow on the edge with the convention "positive means u → v".
    A_eq = np.zeros((K * n, n_var))
    b_eq = np.zeros(K * n)
    for i, (s_i, t_i) in enumerate(commodities):
        for v in range(n):
            row = K * 0 + i * n + v if False else None  # unused
            r = i * n + v
            for e, (u, vv, _) in enumerate(edges):
                if u == v:
                    A_eq[r, f_idx(i, e)] += 1.0  # leaving v
                elif vv == v:
                    A_eq[r, f_idx(i, e)] -= 1.0  # entering v
            if v == s_i:
                A_eq[r, F_idx(i)] = -1.0  # source must produce F_i
            elif v == t_i:
                A_eq[r, F_idx(i)] = 1.0  # sink must absorb F_i

    # Inequality: t ≥ f and t ≥ −f, plus per-edge capacity Σ_i t_{i,e} ≤ c_e.
    A_ub_rows = []
    b_ub = []
    for i in range(K):
        for e in range(m):
            row = np.zeros(n_var)
            row[f_idx(i, e)] = 1.0
            row[t_idx(i, e)] = -1.0
            A_ub_rows.append(row)
            b_ub.append(0.0)
            row = np.zeros(n_var)
            row[f_idx(i, e)] = -1.0
            row[t_idx(i, e)] = -1.0
            A_ub_rows.append(row)
            b_ub.append(0.0)
    for e, (_, _, cap) in enumerate(edges):
        row = np.zeros(n_var)
        for i in range(K):
            row[t_idx(i, e)] = 1.0
        A_ub_rows.append(row)
        b_ub.append(float(cap))
    A_ub = np.vstack(A_ub_rows)
    b_ub = np.asarray(b_ub)

    # Bounds: f free, t >= 0, F >= 0
    bounds = [(None, None)] * (K * m) + [(0, None)] * (K * m) + [(0, None)] * K

    result = linprog(
        c,
        A_ub=A_ub,
        b_ub=b_ub,
        A_eq=A_eq,
        b_eq=b_eq,
        bounds=bounds,
        method="highs",
    )
    if not result.success:
        print(json.dumps({"error": result.message}))
        sys.exit(1)
    f_per = [float(result.x[F_idx(i)]) for i in range(K)]
    print(
        json.dumps({"F_per": f_per, "total": float(sum(f_per)), "status": result.status})
    )


if __name__ == "__main__":
    main()
