#!/usr/bin/env python3
"""Path-preselection LP for multi-commodity max-flow.

For each commodity i, precomputes the K_paths shortest s_i-t_i paths
(Yen's algorithm). LP variables: f_{i, p} ≥ 0 (flow per commodity per
preselected path).

Maximise:   Σ_i Σ_p f_{i,p}
Subject to: Σ_{(i,p): e ∈ p} f_{i,p} ≤ c_e  for each edge e
            f_{i,p} ≥ 0

This is what production WAN-TE controllers (Google B4, Microsoft SWAN,
etc.) actually solve, NOT the full direct MC max-flow LP. Variables are
K · K_paths instead of K · m, drastically smaller.

Input JSON: { n, edges: [[u, v, cap], ...], commodities: [[s, t], ...] }
CLI: mc_path_lp.py input.json [--paths=K] [--seed=N]
Output JSON: { F_per, total, lp_vars, lp_constraints, time_ms }
"""
import heapq
import json
import sys
import time
from collections import defaultdict

import numpy as np
from scipy.optimize import linprog


def build_adj(n, edges):
    adj = defaultdict(list)
    for ei, (u, v, _c) in enumerate(edges):
        adj[u].append((v, ei))
        adj[v].append((u, ei))
    return adj


def dijkstra_path(adj, weights, src, dst, n):
    """Shortest path under positive edge weights, return list of edge indices."""
    INF = float("inf")
    dist = [INF] * n
    pred_edge = [-1] * n
    pred_node = [-1] * n
    dist[src] = 0.0
    heap = [(0.0, src)]
    while heap:
        d, u = heapq.heappop(heap)
        if u == dst:
            break
        if d > dist[u] + 1e-15:
            continue
        for v, ei in adj[u]:
            nd = d + weights[ei]
            if nd < dist[v]:
                dist[v] = nd
                pred_edge[v] = ei
                pred_node[v] = u
                heapq.heappush(heap, (nd, v))
    if dist[dst] == INF:
        return None
    # Reconstruct
    path = []
    u = dst
    while u != src:
        e = pred_edge[u]
        if e < 0:
            return None
        path.append(e)
        u = pred_node[u]
    path.reverse()
    return path


def yens_k_shortest(adj, edges, src, dst, n, k_paths):
    """Yen's algorithm: top-k shortest simple paths under edge cost 1/cap."""
    weights = [1.0 / max(c, 1) for (_, _, c) in edges]
    A = []  # accepted paths
    B = []  # candidate paths (heap by length)
    p0 = dijkstra_path(adj, weights, src, dst, n)
    if p0 is None:
        return []
    A.append(p0)
    for _ in range(k_paths - 1):
        last = A[-1]
        for i in range(len(last)):
            spur_node_edge = last[i]
            # The spur node is the end-point of last[:i] starting from src
            # Walk path to find spur node
            cur = src
            for ei in last[:i]:
                u_e, v_e, _c = edges[ei]
                cur = v_e if u_e == cur else u_e
            spur_node = cur
            root_path = last[:i]
            # Edges to remove: any edge that starts the same root_path in some previous accepted path
            removed_edges = set()
            for path in A:
                if path[:i] == root_path and i < len(path):
                    removed_edges.add(path[i])
            # Removed-edge weights set to INF
            mod_weights = list(weights)
            for re in removed_edges:
                mod_weights[re] = 1e30
            spur = dijkstra_path(adj, mod_weights, spur_node, dst, n)
            if spur is None:
                continue
            full = root_path + spur
            # Length under original weights
            length = sum(weights[e] for e in full)
            if not any(p == full for p in A):
                heapq.heappush(B, (length, len(B), full))
        if not B:
            break
        _, _, p_next = heapq.heappop(B)
        A.append(p_next)
    return A


def main():
    args = {"--paths": "5", "--seed": "0"}
    pos_args = []
    for a in sys.argv[1:]:
        if a.startswith("--"):
            k, _, v = a.partition("=")
            args[k] = v if v else "1"
        else:
            pos_args.append(a)
    if not pos_args:
        print(json.dumps({"error": "usage: mc_path_lp.py input.json [--paths=K]"}))
        sys.exit(1)
    k_paths = int(args["--paths"])

    data = json.load(open(pos_args[0]))
    n = int(data["n"])
    edges = [tuple(e) for e in data["edges"]]
    commodities = [tuple(c) for c in data["commodities"]]
    K = len(commodities)
    m = len(edges)

    t0 = time.perf_counter()
    adj = build_adj(n, edges)

    # Precompute K_paths per commodity
    paths_per = []
    for s, t in commodities:
        p = yens_k_shortest(adj, edges, s, t, n, k_paths)
        paths_per.append(p)

    # LP: variables = list of (commodity, path_idx) pairs
    var_idx = {}
    n_var = 0
    for i, ps in enumerate(paths_per):
        for j in range(len(ps)):
            var_idx[(i, j)] = n_var
            n_var += 1

    if n_var == 0:
        print(json.dumps({"error": "no paths found"}))
        sys.exit(1)

    # Objective: minimise -Σ f_{i,p}  (we want to maximise total flow)
    c_obj = -np.ones(n_var)

    # Constraints: per-edge capacity
    # For each edge e: Σ_{(i,j): e ∈ paths_per[i][j]} f_{i,j} ≤ c_e
    A_ub = np.zeros((m, n_var))
    b_ub = np.zeros(m)
    for e_idx, (_, _, cap) in enumerate(edges):
        b_ub[e_idx] = float(cap)
    for i, ps in enumerate(paths_per):
        for j, path in enumerate(ps):
            v = var_idx[(i, j)]
            for e in path:
                A_ub[e, v] += 1.0

    bounds = [(0, None)] * n_var
    result = linprog(c_obj, A_ub=A_ub, b_ub=b_ub, bounds=bounds, method="highs")
    if not result.success:
        print(json.dumps({"error": result.message}))
        sys.exit(1)

    f_per = [0.0] * K
    for (i, j), v in var_idx.items():
        f_per[i] += float(result.x[v])
    total = float(sum(f_per))
    elapsed_ms = (time.perf_counter() - t0) * 1000.0

    print(json.dumps({
        "F_per": f_per,
        "total": total,
        "lp_vars": n_var,
        "lp_constraints": m,
        "k_paths": k_paths,
        "time_ms": elapsed_ms,
    }))


if __name__ == "__main__":
    main()
