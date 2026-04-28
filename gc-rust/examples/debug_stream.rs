use gc_rust::contribution::*;

fn main() {
    let raw = std::fs::read_to_string(
        "/Users/tosku/Sync/Documents/slmm/test/contribution/synthetic/hatmatrix.json"
    ).unwrap();
    let els: Vec<HElement> = serde_json::from_str(&raw).unwrap();
    let hm = hat_matrix_from_list(&els);

    let row_id = "A:D".to_string();
    let row = hm.get(&row_id).unwrap();
    let mut g = HatRowGraph::from_hat_row(&row_id, row).unwrap();

    eprintln!("source={} sink={}", g.source, g.sink);

    for i in 0..10 {
        match shortest_stream(&mut g) {
            None => { eprintln!("[{}] no path", i); break; }
            Some(s) => {
                eprintln!(
                    "[{}] phi={:.6} L={} path={:?}",
                    i, s.flow, s.path.len(), s.path
                );
                g.apply_stream(&s);
                let mut ks: Vec<_> = g.contribution.keys().cloned().collect();
                ks.sort();
                let snap: Vec<String> = ks.iter()
                    .map(|k| format!("{}={:.5}", k, g.contribution[k]))
                    .collect();
                eprintln!("    contrib: {}", snap.join(" "));
            }
        }
    }

    eprintln!("\nfinal:");
    let mut keys: Vec<_> = g.contribution.keys().cloned().collect();
    keys.sort();
    for k in &keys {
        eprintln!("  {}: {:.6}", k, g.contribution[k]);
    }
    let s: f64 = g.contribution.values().sum();
    eprintln!("  sum: {:.6}", s);
}
