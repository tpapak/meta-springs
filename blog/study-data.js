/* Display the animation's own data; no duplicate dataset to maintain. */
window.showStudyData = function (getDatasets) {
  const panel = document.createElement('details');
  panel.className = 'study-data';
  panel.innerHTML = '<summary>Study data</summary><div class="study-data-body"></div>';
  const script = document.currentScript;
  script.parentNode.insertBefore(panel, script);
  const style = document.createElement('style');
  style.textContent = '.study-data{box-sizing:border-box;width:100%;max-width:1100px;margin:16px auto;padding:12px 16px;border:1px solid #ddd;border-radius:6px;background:#fff;color:#222;font:14px/1.5 system-ui,sans-serif}.study-data summary{cursor:pointer;font-weight:600}.study-data-body{overflow-x:auto}.study-data table{border-collapse:collapse;width:100%;margin:12px 0}.study-data caption{text-align:left;font-weight:600;padding-top:12px}.study-data th,.study-data td{padding:6px 12px;border-bottom:1px solid #ddd;text-align:right;white-space:nowrap}.study-data th:first-child,.study-data td:first-child{text-align:left}.study-data p{margin:10px 0}';
  panel.appendChild(style);
  const body = panel.querySelector('.study-data-body');
  const cell = (tag, value) => {
    const el = document.createElement(tag);
    el.textContent = typeof value === 'number' ? Number(value.toPrecision(8)).toString() : value;
    return el;
  };
  function update() {
    body.replaceChildren();
    for (const ds of getDatasets()) {
      const binary = ds.studies.some(s => s.arms.some(a => a.events !== undefined));
      const covariate = ds.studies.some(s => s.x !== undefined);
      const headers = ['Study', 'Treatment', ...(binary ? ['Events', 'Participants n'] : ['Arm mean y', 'Sampling variance v']), ...(covariate ? ['Covariate x', 'Centred x'] : []), ...(ds.group ? ['Heterogeneity group'] : [])];
      const table = document.createElement('table');
      table.appendChild(cell('caption', ds.title));
      const head = document.createElement('thead'), row = document.createElement('tr');
      headers.forEach(label => { const th = cell('th', label); th.scope = 'col'; row.appendChild(th); });
      head.appendChild(row); table.appendChild(head);
      const tbody = document.createElement('tbody');
      const center = covariate ? ds.studies.reduce((sum,s) => sum+s.x,0)/ds.studies.length : 0;
      for (const study of ds.studies) for (const arm of study.arms) {
        const tr = document.createElement('tr');
        const treatment = typeof arm.t === 'number' ? ds.treatments[arm.t] : arm.t;
        const values = [study.id, treatment, ...(binary ? [arm.events, arm.n] : [arm.y, arm.v]), ...(covariate ? [study.x, study.x-center] : []), ...(ds.group ? [ds.group(arm)] : [])];
        values.forEach(value => tr.appendChild(cell('td', value))); tbody.appendChild(tr);
      }
      table.appendChild(tbody); body.appendChild(table);
      body.appendChild(cell('p', ds.note || (binary ? 'Binomial observations: events out of n participants in each arm.' : 'v is the sampling variance of the arm mean, not the individual-outcome variance.')));
    }
  }
  update();
  // Run after the animation's own handlers update editable data.
  for (const event of ['input', 'change', 'click']) document.addEventListener(event, e => {
    if (!panel.contains(e.target)) setTimeout(update, 0);
  });
};
