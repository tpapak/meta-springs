//! Symmetric CSR adjacency for a weighted graph Laplacian.

pub type Edge = (usize, usize, i64);

/// `(L + εI) x` action on a sparse weighted graph, with vertex-major
/// storage for cache-friendly SpMV.
///
/// `nbrs[row_ptr[u]..row_ptr[u+1]]` is vertex `u`'s adjacency list, each
/// entry a `(v, w)` pair. Each undirected edge appears twice — once
/// from each endpoint. Weights are stored as `f32` to halve the cache
/// footprint; arithmetic promotes to `f64`. (Note: tried mixed-precision
/// f32 reads on the input `x` too — that introduces ~1e-7 noise into
/// `Ap` and breaks CG convergence at the 1e-8 tolerance we want; reverted.)
#[derive(Clone)]
pub struct CsrLap {
    pub n: usize,
    pub row_ptr: Vec<usize>,
    pub nbr_v: Vec<u32>,
    pub nbr_w: Vec<f32>,
}

impl CsrLap {
    /// Build from a directed edge list. Multi-edges (e.g. one `s → v`
    /// and one `v → t`) accumulate weight on the canonical undirected
    /// pair. The result is the *undirected* graph Laplacian.
    pub fn from_directed_edges(edges: &[Edge], n: usize) -> Self {
        // Canonicalise + sort + merge multi-edges.
        let mut canonical: Vec<(u32, u32, f32)> = edges
            .iter()
            .filter_map(|&(u, v, c)| {
                if u == v {
                    return None;
                }
                let (a, b) = if u < v {
                    (u as u32, v as u32)
                } else {
                    (v as u32, u as u32)
                };
                Some((a, b, c as f32))
            })
            .collect();
        canonical.sort_unstable_by_key(|&(a, b, _)| (a, b));
        let mut merged: Vec<(u32, u32, f32)> = Vec::with_capacity(canonical.len());
        for entry in canonical {
            if let Some(last) = merged.last_mut() {
                if last.0 == entry.0 && last.1 == entry.1 {
                    last.2 += entry.2;
                    continue;
                }
            }
            merged.push(entry);
        }

        // Degree pass + CSR fill.
        let mut deg = vec![0_usize; n];
        for &(a, b, _) in &merged {
            deg[a as usize] += 1;
            deg[b as usize] += 1;
        }
        let mut row_ptr = vec![0_usize; n + 1];
        for i in 0..n {
            row_ptr[i + 1] = row_ptr[i] + deg[i];
        }
        let total = row_ptr[n];
        let mut nbr_v = vec![0_u32; total];
        let mut nbr_w = vec![0.0_f32; total];
        let mut cursor = row_ptr.clone();
        for &(a, b, w) in &merged {
            let pa = cursor[a as usize];
            nbr_v[pa] = b;
            nbr_w[pa] = w;
            cursor[a as usize] = pa + 1;
            let pb = cursor[b as usize];
            nbr_v[pb] = a;
            nbr_w[pb] = w;
            cursor[b as usize] = pb + 1;
        }
        Self { n, row_ptr, nbr_v, nbr_w }
    }

    /// Convenience constructor for benches that already produce one edge
    /// per undirected pair (random-graph generator). Equivalent to
    /// `from_directed_edges` once duplicates are dropped.
    pub fn from_undirected_edges(edges: &[Edge], n: usize) -> Self {
        Self::from_directed_edges(edges, n)
    }

    /// Build directly from an already-canonical undirected edge list with
    /// `f64` weights. Each `(u, v, w)` appears once; `u != v`. Used by
    /// the IRLS max-flow loop, which rebuilds the Laplacian per outer
    /// iteration with current conductances `1/r_e`.
    pub fn from_canonical_weights(edges: &[(u32, u32, f64)], n: usize) -> Self {
        let mut deg = vec![0_usize; n];
        for &(a, b, _) in edges {
            deg[a as usize] += 1;
            deg[b as usize] += 1;
        }
        let mut row_ptr = vec![0_usize; n + 1];
        for i in 0..n {
            row_ptr[i + 1] = row_ptr[i] + deg[i];
        }
        let total = row_ptr[n];
        let mut nbr_v = vec![0_u32; total];
        let mut nbr_w = vec![0.0_f32; total];
        let mut cursor = row_ptr.clone();
        for &(a, b, w) in edges {
            let pa = cursor[a as usize];
            nbr_v[pa] = b;
            nbr_w[pa] = w as f32;
            cursor[a as usize] = pa + 1;
            let pb = cursor[b as usize];
            nbr_v[pb] = a;
            nbr_w[pb] = w as f32;
            cursor[b as usize] = pb + 1;
        }
        Self { n, row_ptr, nbr_v, nbr_w }
    }

    pub fn n(&self) -> usize {
        self.n
    }

    pub fn nnz(&self) -> usize {
        self.nbr_v.len() / 2
    }

    /// Diagonal of `L + εI` — weighted degrees plus `ε`.
    pub fn diag(&self, eps: f64) -> Vec<f64> {
        let mut d = vec![eps; self.n];
        for u in 0..self.n {
            let mut s = 0.0_f64;
            for k in self.row_ptr[u]..self.row_ptr[u + 1] {
                s += self.nbr_w[k] as f64;
            }
            d[u] += s;
        }
        d
    }

    /// `y ← (L + εI) x`, cache-friendly vertex-major SpMV. Parallelised
    /// over vertices via rayon for large graphs.
    pub fn apply(&self, eps: f64, x: &[f64], y: &mut [f64]) {
        const PAR_THRESHOLD: usize = 200_000;
        if self.n < PAR_THRESHOLD {
            self.apply_serial(eps, x, y);
        } else {
            self.apply_parallel(eps, x, y);
        }
    }

    fn apply_serial(&self, eps: f64, x: &[f64], y: &mut [f64]) {
        for u in 0..self.n {
            let xu = x[u];
            let mut acc = eps * xu;
            for k in self.row_ptr[u]..self.row_ptr[u + 1] {
                let v = self.nbr_v[k] as usize;
                let w = self.nbr_w[k] as f64;
                acc += w * (xu - x[v]);
            }
            y[u] = acc;
        }
    }

    fn apply_parallel(&self, eps: f64, x: &[f64], y: &mut [f64]) {
        use rayon::prelude::*;
        let row_ptr = &self.row_ptr;
        let nbr_v = &self.nbr_v;
        let nbr_w = &self.nbr_w;
        y.par_iter_mut().enumerate().for_each(|(u, yu)| {
            let xu = x[u];
            let mut acc = eps * xu;
            for k in row_ptr[u]..row_ptr[u + 1] {
                let v = nbr_v[k] as usize;
                let w = nbr_w[k] as f64;
                acc += w * (xu - x[v]);
            }
            *yu = acc;
        });
    }
}
