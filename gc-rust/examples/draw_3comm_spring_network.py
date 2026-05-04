#!/usr/bin/env python3
"""Render a small 3-commodity spring network.

Each edge is a single shared spring (capacity c_e). Each commodity has
its own current flow f_{k,e}; the spring's nonlinear potential
−log(1 − (Σ_k|f_{k,e}|/c_e)²) couples the three commodity displacements
together through one shared-capacity barrier.
"""

import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
from matplotlib.patches import FancyArrowPatch
import numpy as np

# Layout: 6 nodes in a stretched pentagon-with-bridge.
# Three commodities each with their own (s, t):
#   C1 (red)   : 0 → 5
#   C2 (blue)  : 1 → 4
#   C3 (green) : 2 → 3
nodes = {
    0: (0.0, 1.5),    # s1
    1: (0.0, 0.0),    # s2
    2: (1.0, -1.2),   # s3
    3: (3.5, -1.2),   # t3
    4: (4.5, 0.0),    # t2
    5: (4.5, 1.5),    # t1
}
edges = [
    # (u, v, cap)
    (0, 1, 8),
    (0, 2, 6),
    (1, 2, 5),
    (1, 3, 7),
    (2, 3, 9),
    (3, 4, 6),
    (3, 5, 4),
    (4, 5, 5),
    (1, 4, 4),
    (2, 5, 3),
]
commodities = [(0, 5, 'C1', '#d62728'),
               (1, 4, 'C2', '#1f77b4'),
               (2, 3, 'C3', '#2ca02c')]

# Made-up illustrative flows per (commodity, edge) — toy values that
# satisfy conservation and capacity (Σ_k|f_k,e| ≤ c_e).
# (k, edge_idx): flow magnitude (signed: + if u→v)
flows = {
    # commodity 0 (s=0,t=5)  uses path 0→2→5  amount 3, and 0→1→...→5 amount 2
    (0, 0): 2,   # 0->1
    (0, 1): 3,   # 0->2
    (0, 9): 3,   # 2->5
    (0, 4): 2,   # 2->3 ? no — re-route: 0→1→3→5
    (0, 3): 2,   # 1->3
    (0, 6): 2,   # 3->5
    # commodity 1 (s=1,t=4) uses 1→4 directly (cap 4) amount 4
    (1, 8): 4,   # 1->4
    # commodity 2 (s=2,t=3) uses 2→3 amount 4
    (2, 4): 4,   # 2->3
}

def spring_path(p0, p1, n_coils=8, amp=0.06):
    """Return XY arrays for a spring (zigzag) between p0 and p1."""
    p0 = np.asarray(p0, dtype=float)
    p1 = np.asarray(p1, dtype=float)
    v = p1 - p0
    L = np.linalg.norm(v)
    u = v / L
    perp = np.array([-u[1], u[0]])
    # endpoints get straight segments (10% of length each side)
    pad = 0.12
    n_pts = 4 * n_coils + 2
    ts = np.linspace(pad, 1.0 - pad, n_pts)
    xs = []; ys = []
    for j, t in enumerate(ts):
        base = p0 + t * v
        sign = ((-1) ** j)
        offs = sign * amp * L * perp
        xs.append(base[0] + offs[0])
        ys.append(base[1] + offs[1])
    # straight pre/post legs
    pre0 = p0 + pad * v
    post0 = p0 + (1.0 - pad) * v
    return ([p0[0], pre0[0]] + xs + [post0[0], p1[0]],
            [p0[1], pre0[1]] + ys + [post0[1], p1[1]])

fig, ax = plt.subplots(figsize=(11, 6))
ax.set_facecolor('white')

# Draw springs.
for ei, (u, v, c) in enumerate(edges):
    p0 = nodes[u]; p1 = nodes[v]
    xs, ys = spring_path(p0, p1, n_coils=10, amp=0.04)
    # Spring stiffness modulated by load: thicker = more loaded
    used = sum(abs(flows.get((k, ei), 0.0)) for k in range(3))
    load = used / c
    base_lw = 1.5 + 3.0 * load
    ax.plot(xs, ys, color='#222222', lw=base_lw, alpha=0.85, zorder=2)

    # Edge mid-label: "c=…  used=…/…"
    mid = (np.array(p0) + np.array(p1)) / 2
    perp = np.array([-(p1[1]-p0[1]), p1[0]-p0[0]])
    perp = perp / (np.linalg.norm(perp) + 1e-9)
    label_pos = mid + 0.16 * perp
    txt = f"c={c}\n{used:.0f}/{c}"
    ax.text(label_pos[0], label_pos[1], txt,
            ha='center', va='center', fontsize=8.5,
            bbox=dict(facecolor='white', edgecolor='gray',
                      alpha=0.92, pad=2, boxstyle='round,pad=0.25'),
            zorder=4)

    # K=3 commodity flow rails — three short colored bars beside the spring,
    # only drawn if commodity uses this edge.
    rail_offsets = [-0.075, 0.0, 0.075]
    for k, (s, t, name, col) in enumerate(commodities):
        f_ke = flows.get((k, ei), 0.0)
        if abs(f_ke) < 1e-9: continue
        # Place a small colored bar near the edge midpoint, perpendicular offset
        rail_mid = mid + rail_offsets[k] * perp + 0.0 * perp
        # Bar length proportional to flow magnitude
        bar_len = 0.18 * abs(f_ke) / max(c, 1)
        edge_dir = np.array([p1[0]-p0[0], p1[1]-p0[1]])
        edge_dir = edge_dir / (np.linalg.norm(edge_dir) + 1e-9)
        bar_p0 = rail_mid - 0.5 * bar_len * edge_dir
        bar_p1 = rail_mid + 0.5 * bar_len * edge_dir
        ax.plot([bar_p0[0], bar_p1[0]], [bar_p0[1], bar_p1[1]],
                color=col, lw=4, solid_capstyle='round', zorder=3, alpha=0.9)

# Draw nodes.
for nid, (x, y) in nodes.items():
    is_source = nid in [c[0] for c in commodities]
    is_sink = nid in [c[1] for c in commodities]
    if is_source:
        kk = [c[0] for c in commodities].index(nid)
        col = commodities[kk][3]
        ax.add_patch(plt.Circle((x, y), 0.12, color=col, zorder=5))
        ax.text(x, y, f"s{kk+1}", ha='center', va='center', color='white',
                fontsize=10, fontweight='bold', zorder=6)
    elif is_sink:
        kk = [c[1] for c in commodities].index(nid)
        col = commodities[kk][3]
        ax.add_patch(plt.Circle((x, y), 0.12, color=col, zorder=5))
        ax.text(x, y, f"t{kk+1}", ha='center', va='center', color='white',
                fontsize=10, fontweight='bold', zorder=6)
    else:
        ax.add_patch(plt.Circle((x, y), 0.10, color='#bbbbbb',
                                ec='black', lw=1.0, zorder=5))
        ax.text(x, y, str(nid), ha='center', va='center', color='black',
                fontsize=9, zorder=6)

# Legend.
patches = [mpatches.Patch(color=c[3], label=f"{c[2]}: s{ki+1}→t{ki+1}")
           for ki, c in enumerate(commodities)]
patches.append(mpatches.Patch(color='#222222',
                              label="shared spring (line width ∝ load)"))
ax.legend(handles=patches, loc='lower left', fontsize=9, framealpha=0.95)

ax.set_xlim(-0.7, 5.3)
ax.set_ylim(-1.8, 2.3)
ax.set_aspect('equal')
ax.axis('off')
ax.set_title("3-commodity spring network — one shared nonlinear spring per edge\n"
             "edge potential V(f) = −log(1 − (Σₖ|fₖ,ₑ|/cₑ)²) couples the 3 commodity displacements",
             fontsize=11)

out = '/Users/tosku/Sync/Documents/slmm/gc-rust/examples/three_commodity_spring_network.png'
plt.tight_layout()
plt.savefig(out, dpi=160, bbox_inches='tight')
print(f"wrote {out}")
