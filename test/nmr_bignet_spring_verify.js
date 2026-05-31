const fs=require('fs');
const lines=fs.readFileSync('test/nmr_bignet_arms.csv','utf8').trim().split('\n');
const hdr=lines[0].replace(/"/g,'').split(',');
const idx=n=>hdr.indexOf(n);
const arms=lines.slice(1).map(l=>{const c=l.replace(/"/g,'').split(',');
  return{study:c[idx('study')],t:c[idx('treatment')],y:+c[idx('mean')],se:+c[idx('std.err')],x:+c[idx('x')]};});
const tset=[...new Set(arms.map(a=>a.t))].sort(); const ref='A';
const treats=[ref,...tset.filter(t=>t!==ref)];
const tIdx=Object.fromEntries(treats.map((t,i)=>[t,i]));
const studies=[...new Set(arms.map(a=>a.study))];
const sIdx=Object.fromEntries(studies.map((s,i)=>[s,i]));
const sx={}; arms.forEach(a=>sx[a.study]=a.x);
const xbar=studies.reduce((s,st)=>s+sx[st],0)/studies.length;
const T=treats.length,S=studies.length,nD=T-1,nMu=S,IB=nD+nMu,N=nD+nMu+1;
const A=Array.from({length:N},()=>new Float64Array(N)),b=new Float64Array(N);
for(const a of arms){const w=1/(a.se*a.se),co={};const ti=tIdx[a.t];
  if(ti!==0)co[ti-1]=(co[ti-1]||0)+1;
  co[nD+sIdx[a.study]]=(co[nD+sIdx[a.study]]||0)+1;
  if(ti!==0)co[IB]=(co[IB]||0)+(a.x-xbar);
  for(const i in co){b[i]+=w*co[i]*a.y;for(const j in co)A[i][j]+=w*co[i]*co[j];}}
function linSolve(Ain,bin){const n=bin.length,M=Ain.map((r,i)=>[...r,bin[i]]);
 for(let c=0;c<n;c++){let mx=Math.abs(M[c][c]),mr=c;for(let r=c+1;r<n;r++)if(Math.abs(M[r][c])>mx){mx=Math.abs(M[r][c]);mr=r;}[M[c],M[mr]]=[M[mr],M[c]];for(let r=c+1;r<n;r++){const f=M[r][c]/M[c][c];for(let k=c;k<=n;k++)M[r][k]-=f*M[c][k];}}
 const x=new Float64Array(n);for(let i=n-1;i>=0;i--){let s=M[i][n];for(let j=i+1;j<n;j++)s-=M[i][j]*x[j];x[i]=s/M[i][i];}return x;}
const x=linSolve(A,b);
const e=new Float64Array(N);e[IB]=1;const seB=Math.sqrt(linSolve(A,e)[IB]);
console.log("\n===== spring arm-level WLS (same data) =====");
for(let t=1;t<T;t++) console.log(`d.A.${treats[t]} = ${x[t-1].toFixed(3)}`);
console.log(`B (beta) = ${x[IB].toFixed(3)}   SE ${seB.toFixed(3)}`);
