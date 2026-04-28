//! Contribution matrix via the Papakonstantinou (2018) shortest-path
//! streams decomposition.
//!
//! Direct port of the shortest-path arm of
//! `../nma-contribution/src/Data/NMA/Contribution.hs`. We only support the
//! BFS (shortest-edge) stream finder; `findAStream` and `longestStream`
//! are intentionally absent.
//!
//! For each row of the hat matrix we:
//!
//!   1. Build a directed residual graph. Edge `a → b` carries capacity
//!      `|H[ij,ab]|`; orientation respects the sign of the hat entry.
//!   2. Repeatedly pick the BFS-shortest residual path source→sink.
//!   3. Stream flow φ = min capacity along the path. Each path edge
//!      receives `φ / L` contribution to its lex-canonical column.
//!   4. Subtract φ from each path edge; drop edges that fall below `EPS`.
//!   5. Repeat until no path remains.
//!
//! The contribution matrix's per-comparison values are then redistributed
//! across studies via [`study_contributions`], given each study's IV
//! weight on each direct comparison. The streams collected during the
//! decomposition are exposed as `path` contributions via
//! [`contribution_row`] and [`contribution_matrix`].

use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap, HashSet, VecDeque};

/// Capacity below which an edge is treated as removed. Matches the
/// `1e-8` cutoff in `updateFlow` from the Haskell reference.
pub const EPS: f64 = 1e-8;

// ─────────────────────────────────────────────────────────────────────
// Comparison ids
// ─────────────────────────────────────────────────────────────────────

/// Canonical, alphabetically ordered direct comparison "a:b" (with `a ≤ b`).
pub type CompId = String;

/// Build the canonical "a:b" comparison id with `a ≤ b`.
pub fn comp_id(a: &str, b: &str) -> CompId {
    if a <= b { format!("{a}:{b}") } else { format!("{b}:{a}") }
}

/// Split a comparison id "a:b" into its two treatment names. Returns
/// `None` if the id is malformed.
pub fn split_comp(c: &str) -> Option<(&str, &str)> {
    let mut it = c.splitn(2, ':');
    let a = it.next()?;
    let b = it.next()?;
    if a.is_empty() || b.is_empty() { return None; }
    Some((a, b))
}

// ─────────────────────────────────────────────────────────────────────
// Hat matrix shape
// ─────────────────────────────────────────────────────────────────────

/// One element of the wide hat-matrix JSON: `{row, comparison, value}`.
/// Mirrors `HElement` in `nma-contribution/src/Data/NMA.hs`.
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct HElement {
    pub row: String,
    pub comparison: String,
    pub value: f64,
}

/// A hat-matrix row: direct-comparison id → signed hat value.
pub type HatRow = BTreeMap<CompId, f64>;

/// Hat matrix indexed by network-comparison row.
pub type HatMatrix = BTreeMap<CompId, HatRow>;

/// Build a [`HatMatrix`] from the wide JSON list. If the same
/// `(row, comparison)` appears more than once the last entry wins
/// (matches `Map.union` on singletons).
pub fn hat_matrix_from_list(els: &[HElement]) -> HatMatrix {
    let mut hm: HatMatrix = BTreeMap::new();
    for e in els {
        hm.entry(e.row.clone())
            .or_default()
            .insert(e.comparison.clone(), e.value);
    }
    hm
}

// ─────────────────────────────────────────────────────────────────────
// Per-row residual graph
// ─────────────────────────────────────────────────────────────────────

/// Residual graph for one hat row. Vertices = treatment names. Edges
/// carry remaining capacity. Inner map is a `BTreeMap` so neighbour
/// iteration is deterministic.
#[derive(Debug, Clone)]
pub struct HatRowGraph {
    pub source: String,
    pub sink: String,
    pub caps: HashMap<String, BTreeMap<String, f64>>,
    pub contribution: ContributionRow,
}

impl HatRowGraph {
    /// Build the directed residual graph for one hat row. Entries with
    /// `|h| ≤ EPS` are dropped (they would never be selected anyway).
    pub fn from_hat_row(row_id: &CompId, row: &HatRow) -> Option<Self> {
        let (src, dst) = split_comp(row_id)?;
        let mut caps: HashMap<String, BTreeMap<String, f64>> = HashMap::new();
        let mut contribution: ContributionRow = BTreeMap::new();
        for (cid, h) in row {
            contribution.insert(cid.clone(), 0.0);
            let (a, b) = match split_comp(cid) {
                Some(p) => p,
                None => continue,
            };
            let mag = h.abs();
            if mag <= EPS {
                continue;
            }
            let (from, to) = if *h > 0.0 { (a, b) } else { (b, a) };
            caps.entry(from.to_string())
                .or_default()
                .insert(to.to_string(), mag);
        }
        Some(HatRowGraph {
            source: src.to_string(),
            sink: dst.to_string(),
            caps,
            contribution,
        })
    }

    /// Minimum residual capacity along a path. Empty path → 0.
    pub fn min_capacity(&self, path: &[(String, String)]) -> f64 {
        if path.is_empty() {
            return 0.0;
        }
        let mut m = f64::INFINITY;
        for (u, v) in path {
            if let Some(neis) = self.caps.get(u) {
                if let Some(c) = neis.get(v) {
                    if *c < m { m = *c; }
                } else {
                    return 0.0;
                }
            } else {
                return 0.0;
            }
        }
        m
    }

    /// Flatten remaining capacities for diagnostics / residue checks.
    pub fn residual_flows(&self) -> Vec<f64> {
        let mut out = Vec::new();
        for neis in self.caps.values() {
            for c in neis.values() {
                out.push(*c);
            }
        }
        out
    }

    /// Subtract the stream's flow from each path edge, dropping edges
    /// that fall below `EPS`, and accumulate `φ / L` contribution to each
    /// edge's canonical comparison id.
    pub fn apply_stream(&mut self, stream: &Stream) {
        let l = stream.path.len() as f64;
        if l == 0.0 || stream.flow <= EPS {
            return;
        }
        for (u, v) in &stream.path {
            if let Some(neis) = self.caps.get_mut(u) {
                if let Some(c) = neis.get_mut(v) {
                    *c -= stream.flow;
                    if *c < EPS {
                        neis.remove(v);
                    }
                }
                if neis.is_empty() {
                    self.caps.remove(u);
                }
            }
            let key = comp_id(u, v);
            if let Some(c) = self.contribution.get_mut(&key) {
                *c += stream.flow / l;
            }
        }
    }
}

// ─────────────────────────────────────────────────────────────────────
// Streams
// ─────────────────────────────────────────────────────────────────────

/// A directed source→sink path through the residual graph plus the
/// minimum capacity along it (the flow contributed by this stream).
#[derive(Debug, Clone, Serialize)]
pub struct Stream {
    pub path: Vec<(String, String)>,
    pub flow: f64,
}

/// BFS shortest stream — port of `shortestStream`. Picks the path with
/// the fewest edges. Returns `None` if no path remains source→sink.
pub fn shortest_stream(g: &mut HatRowGraph) -> Option<Stream> {
    if g.source == g.sink {
        return None;
    }
    let mut parent: HashMap<String, String> = HashMap::new();
    let mut q: VecDeque<String> = VecDeque::new();
    q.push_back(g.source.clone());
    let mut seen: HashSet<String> = HashSet::new();
    seen.insert(g.source.clone());
    let mut reached = false;
    'outer: while let Some(u) = q.pop_front() {
        if let Some(neis) = g.caps.get(&u) {
            for (v, cap) in neis {
                if *cap <= EPS { continue; }
                if seen.contains(v) { continue; }
                seen.insert(v.clone());
                parent.insert(v.clone(), u.clone());
                if v == &g.sink {
                    reached = true;
                    break 'outer;
                }
                q.push_back(v.clone());
            }
        }
    }
    if !reached {
        return None;
    }
    // Reconstruct path source→sink.
    let mut path: Vec<(String, String)> = Vec::new();
    let mut cur = g.sink.clone();
    while let Some(p) = parent.get(&cur).cloned() {
        path.push((p.clone(), cur.clone()));
        cur = p;
    }
    path.reverse();
    let phi = g.min_capacity(&path);
    if phi <= EPS {
        return None;
    }
    Some(Stream { path, flow: phi })
}

// ─────────────────────────────────────────────────────────────────────
// Contribution rows / matrix
// ─────────────────────────────────────────────────────────────────────

/// Per-row contributions, keyed by direct-comparison id (using whichever
/// orientation appeared in the original hat row).
pub type ContributionRow = BTreeMap<CompId, f64>;

/// Contribution matrix: one row per network comparison.
pub type ContributionMatrixMap = BTreeMap<CompId, ContributionRow>;

/// Per-row stream list — the path contributions emitted by a single
/// decomposition.
pub type StreamMatrixMap = BTreeMap<CompId, Vec<Stream>>;

/// Per-row decomposition. Iterates `shortest_stream` until no augmenting
/// path remains. Returns the contributions, the streams collected, and
/// the residual graph.
pub fn contribution_row(
    row_id: &CompId,
    row: &HatRow,
) -> Option<(ContributionRow, Vec<Stream>, HatRowGraph)> {
    let mut g = HatRowGraph::from_hat_row(row_id, row)?;
    let mut streams: Vec<Stream> = Vec::new();
    // 100k cap matches the Haskell reference. Real rows terminate in
    // O(|edges|) iterations.
    for _ in 0..100_000 {
        let s = match shortest_stream(&mut g) {
            Some(s) => s,
            None => break,
        };
        if s.path.is_empty() || s.flow <= EPS {
            break;
        }
        g.apply_stream(&s);
        streams.push(s);
    }
    Some((g.contribution.clone(), streams, g))
}

/// Apply [`contribution_row`] to every row, returning the contribution
/// matrix, the per-row stream list (path contributions), and residual
/// graphs (for residue diagnostics).
pub fn contribution_matrix(
    hm: &HatMatrix,
) -> (ContributionMatrixMap, StreamMatrixMap, BTreeMap<CompId, HatRowGraph>) {
    let mut cm: ContributionMatrixMap = BTreeMap::new();
    let mut sm: StreamMatrixMap = BTreeMap::new();
    let mut graphs: BTreeMap<CompId, HatRowGraph> = BTreeMap::new();
    for (rid, row) in hm {
        if let Some((cr, streams, g)) = contribution_row(rid, row) {
            cm.insert(rid.clone(), cr);
            sm.insert(rid.clone(), streams);
            graphs.insert(rid.clone(), g);
        }
    }
    (cm, sm, graphs)
}

/// Convenience: just the contributions for one row.
pub fn contribution_row_shortest_path(
    row_id: &CompId,
    row: &HatRow,
) -> Option<ContributionRow> {
    contribution_row(row_id, row).map(|(cr, _, _)| cr)
}

/// Convenience: just the contribution matrix.
pub fn contribution_matrix_shortest_path(hm: &HatMatrix) -> ContributionMatrixMap {
    contribution_matrix(hm).0
}

/// Sum of contributions in a row.
pub fn sum_contribution_row(cr: &ContributionRow) -> f64 {
    cr.values().sum()
}

// ─────────────────────────────────────────────────────────────────────
// JSON output shapes
// ─────────────────────────────────────────────────────────────────────

/// One row of the wide-form contribution-matrix JSON.
#[derive(Debug, Serialize)]
pub struct ContribJsonRow<'a> {
    pub row: &'a str,
    pub comparison: &'a str,
    pub value: f64,
}

/// Convert a contribution matrix to the wide-list JSON used by
/// CINeMA / `nma-contribution`.
pub fn contribution_matrix_to_list(cm: &ContributionMatrixMap) -> Vec<ContribJsonRow<'_>> {
    let mut out: Vec<ContribJsonRow<'_>> = Vec::new();
    for (row_id, cr) in cm {
        for (cid, v) in cr {
            out.push(ContribJsonRow {
                row: row_id.as_str(),
                comparison: cid.as_str(),
                value: *v,
            });
        }
    }
    out
}

/// One stream entry in the JSON shape used by Haskell's `StreamMatrix`.
#[derive(Debug, Serialize)]
pub struct StreamJsonRow<'a> {
    pub row: &'a str,
    pub length: usize,
    pub path: Vec<(String, String)>,
    pub contribution: f64,
}

/// Convert per-row streams to the JSON list shape used by
/// `nma-contribution`'s `StreamMatrix` ToJSON instance.
pub fn stream_matrix_to_list(sm: &StreamMatrixMap) -> Vec<StreamJsonRow<'_>> {
    let mut out: Vec<StreamJsonRow<'_>> = Vec::new();
    for (rid, streams) in sm {
        for s in streams {
            out.push(StreamJsonRow {
                row: rid.as_str(),
                length: s.path.len(),
                path: s.path.clone(),
                contribution: s.flow,
            });
        }
    }
    out
}

// ─────────────────────────────────────────────────────────────────────
// Study contributions
// ─────────────────────────────────────────────────────────────────────

/// One study's IV weight on one direct comparison.
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct PairwiseWeight {
    pub study: String,
    pub comparison: CompId,
    pub weight: f64,
}

/// One row of the per-study contribution table:
/// `(network_comparison, study, contribution)`. Mirrors the dataframe
/// shape returned by netmeta's `netcontrib(study = TRUE)` and the
/// `studyContributions` array the CINeMA backend emits.
#[derive(Debug, Clone, Serialize)]
pub struct StudyContrib {
    pub comparison: CompId,
    pub study: String,
    pub contribution: f64,
}

/// Redistribute the per-comparison contributions across studies in
/// proportion to each study's IV weight on that comparison. The
/// returned table aggregates `(network_comparison, study)` so each pair
/// appears at most once with the summed contribution across the
/// comparisons that study informs.
pub fn study_contributions(
    contrib: &ContributionMatrixMap,
    weights: &[PairwiseWeight],
) -> Vec<StudyContrib> {
    let mut total: BTreeMap<CompId, f64> = BTreeMap::new();
    let mut per: BTreeMap<(CompId, String), f64> = BTreeMap::new();
    for w in weights {
        *total.entry(w.comparison.clone()).or_default() += w.weight;
        *per.entry((w.comparison.clone(), w.study.clone())).or_default() += w.weight;
    }
    let mut acc: BTreeMap<(CompId, String), f64> = BTreeMap::new();
    for (rid, cr) in contrib {
        for (cid, c_val) in cr {
            let tot = match total.get(cid) {
                Some(t) if *t > 0.0 => *t,
                _ => continue,
            };
            for ((cmp, st), w) in &per {
                if cmp != cid { continue; }
                let share = c_val * w / tot;
                *acc.entry((rid.clone(), st.clone())).or_default() += share;
            }
        }
    }
    let mut out: Vec<StudyContrib> = acc
        .into_iter()
        .map(|((cmp, study), v)| StudyContrib {
            comparison: cmp,
            study,
            contribution: v,
        })
        .collect();
    out.sort_by(|a, b| {
        a.comparison.cmp(&b.comparison).then(a.study.cmp(&b.study))
    });
    out
}

// ─────────────────────────────────────────────────────────────────────
// Tests
// ─────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    fn triangle_row() -> (CompId, HatRow) {
        let mut row: HatRow = BTreeMap::new();
        row.insert("A:B".into(), 1.0 / 3.0);
        row.insert("B:C".into(), 1.0 / 3.0);
        row.insert("A:C".into(), 2.0 / 3.0);
        ("A:C".into(), row)
    }

    fn ref_dir() -> PathBuf {
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../test/contribution")
    }

    fn load_hm(name: &str) -> HatMatrix {
        let p = ref_dir().join(name);
        let s = std::fs::read_to_string(&p)
            .unwrap_or_else(|e| panic!("read {}: {e}", p.display()));
        let els: Vec<HElement> = serde_json::from_str(&s)
            .unwrap_or_else(|e| panic!("parse {}: {e}", p.display()));
        hat_matrix_from_list(&els)
    }

    #[test]
    fn split_and_canonical() {
        assert_eq!(split_comp("foo:bar"), Some(("foo", "bar")));
        assert_eq!(comp_id("b", "a"), "a:b");
        assert_eq!(comp_id("a", "b"), "a:b");
    }

    #[test]
    fn triangle_row_attributes_correctly() {
        let (rid, row) = triangle_row();
        let cr = contribution_row_shortest_path(&rid, &row).expect("graph");
        assert!((sum_contribution_row(&cr) - 1.0).abs() < 1e-6);
        assert!((cr["A:C"] - 2.0 / 3.0).abs() < 1e-6);
        assert!((cr["A:B"] - 1.0 / 6.0).abs() < 1e-6);
        assert!((cr["B:C"] - 1.0 / 6.0).abs() < 1e-6);
    }

    #[test]
    fn negative_hat_value_flips_edge_direction() {
        let mut row: HatRow = BTreeMap::new();
        row.insert("A:B".into(), 1.0 / 3.0);
        row.insert("B:C".into(), -1.0 / 3.0);
        row.insert("A:C".into(), 2.0 / 3.0);
        let cr =
            contribution_row_shortest_path(&"A:C".into(), &row).expect("graph");
        assert!((cr["A:C"] - 2.0 / 3.0).abs() < 1e-6);
        assert!(cr["A:B"].abs() < 1e-6);
    }

    // ── nma-contribution shortest-path test parity ───────────────────

    /// nma-contribution test1: read all hat-matrix fixtures.
    #[test]
    fn nma_test1_read_all_fixtures() {
        for name in [
            "diabetes_indrhat.json",
            "big_widehat.json",
            "cipriani_2011hat.json",
            "Donghat.json",
            "Leuchthat.json",
        ] {
            let hm = load_hm(name);
            assert!(!hm.is_empty(), "{name}: empty hat matrix");
        }
    }

    /// nma-contribution test2: build per-row residual graph.
    #[test]
    fn nma_test2_build_graph() {
        let hm = load_hm("big_widehat.json");
        let comp: CompId = "agom:amit".into();
        let row = hm.get(&comp).expect("agom:amit row missing");
        let g = HatRowGraph::from_hat_row(&comp, row).expect("graph");
        assert_eq!(g.source, "agom");
        assert_eq!(g.sink, "amit");
    }

    /// nma-contribution testdiabetes4: contributionRow with shortestStream
    /// produces a finite sum on diabetes 2:3.
    #[test]
    fn nma_test4_diabetes_row_sums_finite() {
        let hm = load_hm("diabetes_indrhat.json");
        let comp: CompId = "2:3".into();
        let row = hm.get(&comp).expect("2:3 row missing");
        let cr = contribution_row_shortest_path(&comp, row).expect("graph");
        let s = sum_contribution_row(&cr);
        assert!(s.is_finite() && s > 0.0, "sum was {s}");
    }

    /// big_widehat rows sum to 1 ± 0.001 with shortest-path streams.
    /// (Stricter than the upstream test5 threshold because `shortestStream`
    /// happens to extract every path on this fixture.)
    #[test]
    fn big_widehat_sums_to_one_shortest_path() {
        let hm = load_hm("big_widehat.json");
        let cm = contribution_matrix_shortest_path(&hm);
        let mut max_dev = 0.0_f64;
        for cr in cm.values() {
            let dev = (sum_contribution_row(cr) - 1.0).abs();
            if dev > max_dev { max_dev = dev; }
        }
        assert!(max_dev < 0.001, "max_dev = {max_dev}");
    }

    /// nma-contribution test8: residue flows on Leucht below 0.0006
    /// after `shortestStream` (BFS).
    #[test]
    fn nma_test8_leucht_residue_shortest_path() {
        let hm = load_hm("Leuchthat.json");
        let (_, _, graphs) = contribution_matrix(&hm);
        let mut max_flow = 0.0_f64;
        for g in graphs.values() {
            for f in g.residual_flows() {
                if f > max_flow { max_flow = f; }
            }
        }
        assert!(max_flow < 0.0006, "max residual = {max_flow}");
    }

    /// nma-contribution teststreams: streams collected from the
    /// decomposition are non-empty.
    #[test]
    fn nma_teststreams_diabetes_streams_non_empty() {
        let hm = load_hm("diabetes_indrhat.json");
        let (_, sm, _) = contribution_matrix(&hm);
        assert!(!sm.is_empty());
        assert!(sm.values().any(|v| !v.is_empty()));
    }

    /// Per-study redistribution on the synthetic triangle.
    #[test]
    fn study_contributions_split_by_weight() {
        let (rid, row) = triangle_row();
        let mut hm: HatMatrix = BTreeMap::new();
        hm.insert(rid.clone(), row);
        let cm = contribution_matrix_shortest_path(&hm);
        let weights = vec![
            PairwiseWeight { study: "s1".into(), comparison: "A:C".into(), weight: 1.0 },
            PairwiseWeight { study: "s1".into(), comparison: "A:B".into(), weight: 1.0 },
            PairwiseWeight { study: "s1".into(), comparison: "B:C".into(), weight: 1.0 },
            PairwiseWeight { study: "s2".into(), comparison: "A:B".into(), weight: 3.0 },
            PairwiseWeight { study: "s2".into(), comparison: "B:C".into(), weight: 3.0 },
        ];
        let table = study_contributions(&cm, &weights);
        let s1 = table.iter().find(|r| r.study == "s1").expect("s1 row");
        let s2 = table.iter().find(|r| r.study == "s2").expect("s2 row");
        let expected_s1 = 2.0 / 3.0 + 0.25 * (1.0 / 6.0) + 0.25 * (1.0 / 6.0);
        let expected_s2 = 0.75 * (1.0 / 6.0) + 0.75 * (1.0 / 6.0);
        assert!((s1.contribution - expected_s1).abs() < 1e-9, "s1: {}", s1.contribution);
        assert!((s2.contribution - expected_s2).abs() < 1e-9, "s2: {}", s2.contribution);
        let total = s1.contribution + s2.contribution;
        assert!((total - 1.0).abs() < 1e-9, "total: {total}");
    }
}
