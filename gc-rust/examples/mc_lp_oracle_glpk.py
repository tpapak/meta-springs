#!/usr/bin/env python3
"""MC max-flow LP via GLPK (GNU LP Kit) using PuLP."""
import json
import sys
import time

import pulp


def main():
    data = json.load(open(sys.argv[1]))
    n = int(data["n"])
    edges = [tuple(e) for e in data["edges"]]
    commodities = [tuple(c) for c in data["commodities"]]
    K = len(commodities)
    m = len(edges)

    t0 = time.perf_counter()

    prob = pulp.LpProblem("mc_maxflow", pulp.LpMaximize)
    f = {(i, e): pulp.LpVariable(f"f_{i}_{e}", lowBound=None, upBound=None)
         for i in range(K) for e in range(m)}
    t_aux = {(i, e): pulp.LpVariable(f"t_{i}_{e}", lowBound=0)
             for i in range(K) for e in range(m)}
    F = [pulp.LpVariable(f"F_{i}", lowBound=0) for i in range(K)]

    prob += pulp.lpSum(F[i] for i in range(K))

    for i in range(K):
        s_i, t_i = commodities[i]
        for v in range(n):
            terms = []
            for e, (u, vv, _) in enumerate(edges):
                if u == v: terms.append(f[i, e])
                elif vv == v: terms.append(-f[i, e])
            rhs = 0
            if v == s_i: rhs = F[i]
            elif v == t_i: rhs = -F[i]
            prob += pulp.lpSum(terms) == rhs

    for i in range(K):
        for e in range(m):
            prob += t_aux[i, e] >= f[i, e]
            prob += t_aux[i, e] >= -f[i, e]

    for e, (_, _, cap) in enumerate(edges):
        prob += pulp.lpSum(t_aux[i, e] for i in range(K)) <= float(cap)

    solver = pulp.GLPK_CMD(msg=0)
    prob.solve(solver)

    elapsed_ms = (time.perf_counter() - t0) * 1000.0

    if prob.status != 1:
        print(json.dumps({"error": f"GLPK status {prob.status}", "time_ms": elapsed_ms}))
        sys.exit(1)

    f_per = [float(F[i].value()) for i in range(K)]
    total = float(sum(f_per))
    print(json.dumps({"F_per": f_per, "total": total, "time_ms": elapsed_ms, "solver": "glpk"}))


if __name__ == "__main__":
    main()
