#!/usr/bin/env python3
"""Multi-commodity max-throughput LP via Gurobi.

Same problem as mc_lp_oracle.py (HiGHS), but with Gurobi as backend.
Useful for benchmarking PN against the industrial gold-standard LP solver.

Input JSON: { n, edges: [[u, v, cap], ...], commodities: [[s, t], ...] }
Output JSON: { F_per, total, time_ms, status }

Usage: mc_lp_oracle_gurobi.py <input.json>
"""
import json
import sys
import time

import gurobipy as gp
from gurobipy import GRB


def main():
    data = json.load(open(sys.argv[1]))
    n = int(data["n"])
    edges = [tuple(e) for e in data["edges"]]
    commodities = [tuple(c) for c in data["commodities"]]
    K = len(commodities)
    m = len(edges)

    t0 = time.perf_counter()

    model = gp.Model("mc_maxflow")
    model.setParam("OutputFlag", 0)  # quiet
    model.setParam("Threads", 0)     # use all cores
    t_construct_start = time.perf_counter()

    # Variables
    f = model.addVars(K, m, lb=-GRB.INFINITY, ub=GRB.INFINITY, name="f")
    t = model.addVars(K, m, lb=0.0, name="t")
    F = model.addVars(K, lb=0.0, name="F")

    # Conservation per commodity per vertex
    for i in range(K):
        s_i, t_i = commodities[i]
        for v in range(n):
            expr = gp.LinExpr()
            for e, (u, vv, _) in enumerate(edges):
                if u == v:
                    expr += f[i, e]
                elif vv == v:
                    expr -= f[i, e]
            if v == s_i:
                expr += F[i]  # source produces
            elif v == t_i:
                expr -= F[i]  # sink absorbs
            model.addConstr(expr == 0, name=f"cons_{i}_{v}")

    # Envelope: t >= |f|
    for i in range(K):
        for e in range(m):
            model.addConstr(t[i, e] >= f[i, e])
            model.addConstr(t[i, e] >= -f[i, e])

    # Capacity: Σ_i t[i,e] <= c_e
    for e, (_, _, cap) in enumerate(edges):
        model.addConstr(gp.quicksum(t[i, e] for i in range(K)) <= float(cap))

    # Maximise Σ_i F[i]
    model.setObjective(gp.quicksum(F[i] for i in range(K)), GRB.MAXIMIZE)

    t_construct_ms = (time.perf_counter() - t_construct_start) * 1000.0
    t_solve_start = time.perf_counter()
    model.optimize()
    t_solve_ms = (time.perf_counter() - t_solve_start) * 1000.0
    runtime_ms = float(model.Runtime) * 1000.0  # gurobi-internal solve time

    elapsed_ms = (time.perf_counter() - t0) * 1000.0

    if model.status not in (GRB.OPTIMAL, GRB.SUBOPTIMAL):
        print(json.dumps({
            "error": f"gurobi status {model.status}",
            "time_ms": elapsed_ms,
        }))
        sys.exit(1)

    f_per = [float(F[i].X) for i in range(K)]
    total = float(sum(f_per))

    print(json.dumps({
        "F_per": f_per,
        "total": total,
        "status": int(model.status),
        "time_ms": elapsed_ms,
        "construct_ms": t_construct_ms,
        "solve_wall_ms": t_solve_ms,
        "solve_runtime_ms": runtime_ms,
        "solver": "gurobi",
    }))


if __name__ == "__main__":
    main()
