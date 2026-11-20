#!/usr/bin/env python3
"""
mdblog — edit the meta-springs blog as Markdown.

  python3 tools/mdblog.py extract   # one-time: seed content/*.md from the current HTML
  python3 tools/mdblog.py build     # regenerate all HTML from content/*.md

You edit  blog/content/<slug>.md  (front-matter + prose). Structure (order,
nav, pager, breadcrumb, hero, series lists) is generated. Animations stay as
standalone HTML files and are re-inlined into each page at build time.

Requires: pandoc (brew install pandoc).
"""
import os, re, sys, subprocess, html as _html

BLOG   = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
POSTS  = os.path.join(BLOG, "posts")
CONTENT= os.path.join(BLOG, "content")

# ---------------------------------------------------------------- structure
# Linear reading order. Each entry: (slug, kind).  kind = hub | page | index.
ORDER = [
    ("index",            "index"),
    ("part1",            "hub"),
    ("1a-pairwise-fe",   "page"),
    ("1b-binomial-spring","page"),
    ("1c-random-effects","page"),
    ("1d-spring-reml",   "page"),
    ("part2",            "hub"),
    ("2a-xi-posterior",  "page"),
    ("2b-multiple-tau",  "page"),
    ("2c-exchangeability","page"),
    ("part3",            "hub"),
    ("3a-covariate-axis","page"),
    ("3b-ipd",           "page"),
    ("3c-mlnmr",         "page"),
]
# which hub each part's pages belong to, in order
PARTS = [
    ("part1", 1, ["1a-pairwise-fe","1b-binomial-spring","1c-random-effects","1d-spring-reml"]),
    ("part2", 2, ["2a-xi-posterior","2b-multiple-tau","2c-exchangeability"]),
    ("part3", 3, ["3a-covariate-axis","3b-ipd","3c-mlnmr"]),
]
KIND   = dict(ORDER)
def part_of(slug):
    for hub,num,pages in PARTS:
        if slug==hub or slug in pages: return (hub,num,pages)
    return (None,None,None)
def order_label(slug):                       # "1b-random-effects" -> "1b"
    m=re.match(r'([0-9]+[a-z])', slug); return m.group(1) if m else slug

MATHJAX = ('<script>\nwindow.MathJax = {\n'
 "  tex: { inlineMath: [['$','$'],['\\\\(','\\\\)']], displayMath: [['$$','$$'],['\\\\[','\\\\]']], tags: 'ams',\n"
 "    macros: { Var:'\\\\operatorname{Var}', Cov:'\\\\operatorname{Cov}', E:'\\\\operatorname{E}', tr:'\\\\operatorname{tr}', keff:'k^{\\\\text{eff}}' } },\n"
 "  svg: { fontCache: 'global' }\n};\n</script>\n"
 '<script async src="{home}vendor/mathjax/tex-chtml-full.js"></script>')

BLOGNAV = ('<nav class="blognav">\n  <div class="blognav-inner">\n'
 '    <a class="blognav-brand" href="{home}index.html">meta-springs</a>\n'
 '    <div class="blognav-links">\n'
 '      <a href="{home}index.html">Home</a>\n'
 '      <a href="{home}posts/part1.html">Part 1</a>\n'
 '      <a href="{home}posts/part2.html">Part 2</a>\n'
 '      <a href="{home}posts/part3.html">Part 3</a>\n'
 '    </div>\n  </div>\n</nav>')
FOOTER = ('<footer class="footer">\n  meta-springs &middot; '
 '<a href="https://github.com/tpapak/meta-springs">source on GitHub</a>\n</footer>')

def pandoc(text, frm, to, *extra):
    p = subprocess.run(["pandoc","-f",frm,"-t",to,*extra], input=text,
                       capture_output=True, text=True)
    if p.returncode: sys.stderr.write(p.stderr); raise SystemExit("pandoc failed")
    return p.stdout

# ---------------------------------------------------------------- front-matter
def parse_fm(md):
    fm={}; body=md
    if md.startswith("---"):
        _,fmtext,body = md.split("---",2)
        for line in fmtext.strip().splitlines():
            if ":" in line:
                k,v=line.split(":",1); fm[k.strip()]=v.strip()
        body=body.lstrip("\n")
    return fm, body
def dump_fm(fm):
    def one_line(v): return re.sub(r'\s+',' ',str(v)).strip()
    return "---\n"+"".join(f"{k}: {one_line(v)}\n" for k,v in fm.items() if v)+"---\n\n"

# ---------------------------------------------------------------- math protect
def _tok(i): return "MZZ%dZZM" % i          # citation-proof, pandoc-inert placeholder
def protect_math(s):
    store=[]
    def sub(m):
        store.append(m.group(0)); return _tok(len(store)-1)
    s=re.sub(r'\$\$.+?\$\$', sub, s, flags=re.S)
    s=re.sub(r'\$[^$\n]+?\$', sub, s)
    return s, store
def restore_math(s, store):
    for i,m in enumerate(store): s=s.replace(_tok(i), m)
    return s

# ================================================================ EXTRACT
def html_to_body_md(prose_html):
    # normalise <p class="callout"> -> <div class="callout"> so pandoc keeps it a Div
    prose_html = re.sub(r'<p class="callout"[^>]*>(.*?)</p>',
                        r'<div class="callout">\1</div>', prose_html, flags=re.S)
    prose_html, store = protect_math(prose_html)
    md = pandoc(prose_html, "html", "markdown-raw_html+fenced_divs+pipe_tables")
    md = restore_math(md, store)
    md = re.sub(r'\n{3,}', '\n\n', md).strip()+"\n"
    return md

def first(regex, s, default=""):
    m=re.search(regex, s, re.S); return m.group(1).strip() if m else default

LABELS = {  # nav_label (short) + blurb (one-line), used in the "in this part"/series lists
 "1a-pairwise-fe":   ("Pairwise &rarr; fixed-effect NMA", "Pairwise meta-analysis as two springs; fixed-effect NMA as a bipartite spring network."),
 "1b-binomial-spring":("Binomial: the nonlinear spring", "For binary outcomes the observation spring carries the exact binomial energy; its restoring force saturates; Newton/IRLS relaxes it to an equivalent Gaussian arm."),
 "1c-random-effects":("Random effects: a &tau;-spring per arm", "A soft $\\tau$-spring in series with each observation spring; the effective stiffness $\\keff = 1/(v+\\tau^2)$ (arm-level $\\tau^2$); raising $\\tau^2$ as literal mechanical shrinkage."),
 "1d-spring-reml":   ("Spring-REML: variance decomposition", "Estimating $\\tau^2$ inside the network by the law of total variance over the $\\tau$-spring lengths; the EM fixed point equals REML; the SSPI &harr; effective-$k$ correspondence."),
 "2a-xi-posterior":  ("$\\Xi$ gives the posterior", "Chain length = heterogeneity; $\\Xi = \\sum_N Z$; the $\\tau^2$ posterior and a mixture-of-Gaussians for every effect; any prior for free; agreement &amp; speed vs MCMC."),
 "2b-multiple-tau":  ("Multiple $\\tau^2$ posteriors", "One boson species per exchangeable group; marginalising the multi-species sum gives a separate heterogeneity posterior per group."),
 "2c-exchangeability":("Why the $\\tau$-springs are exchangeable", "One i.i.d. $\\tau$-spring per arm on a shared study anchor; the Lu&ndash;Ades $(I+J)/2$ correlation for free; indistinguishable elementary springs."),
 "3a-covariate-axis":("The covariate as a second axis", "The $\\beta$-spring carries each arm along the covariate axis; shared / unrelated / pooled slope; residual heterogeneity; a baseline-risk regression end to end."),
 "part1": (None, "Pairwise meta-analysis as two springs &rarr; fixed-effect NMA as a bipartite network &rarr; the binomial nonlinear spring &rarr; $\\tau$-springs for random effects &rarr; spring-REML."),
 "part2": (None, "Chain length = heterogeneity; $\\Xi = \\sum Z$ gives the $\\tau^2$ posterior and a mixture-of-Gaussians for every effect; any prior for free; multiple $\\tau^2$ posteriors; exchangeability."),
 "part3": (None, "The covariate as a second axis; the $\\beta$-spring; residual heterogeneity; a baseline-risk regression end to end, engine vs multinma."),
}

def extract():
    os.makedirs(CONTENT, exist_ok=True)
    for slug,kind in ORDER:
        path = os.path.join(BLOG,"index.html") if kind=="index" else os.path.join(POSTS,slug+".html")
        h = open(path).read()
        art = first(r'<article class="postbody">(.*)</article>', h) if kind!="index" \
              else first(r'<main class="wrap">(.*)</main>', h)
        fm={}
        if kind=="index":
            fm["site_title"]=first(r'<h1 class="site-title">(.*?)</h1>', h)
            fm["tagline"]=first(r'<p class="site-tagline">(.*?)</p>', h)
            lead=first(r'<section class="lead">(.*?)</section>', h)
            lead=re.sub(r'<p class="byline">.*?</p>','',lead,flags=re.S)
            papers=first(r'(<h2 class="section-h">Paper.*?</ul>)', h)
            body=html_to_body_md(lead+papers)
        else:
            fm["title"]=first(r'<h1 class="post-h1">(.*?)</h1>', h)
            fm["animation"]=first(r'anim-fulllink"><a href="\.\./([^"]+)"', h)
            fm["hero_title"]=first(r'<div class="posthero-title">(.*?)</div>', h)
            fm["hero_caption"]=first(r'<p class="anim-cap">(.*?)</p>', h)
            hub,num,pages=part_of(slug)
            if kind=="page":
                # prose = after the anim-fulllink </p> up to the in-part nav
                after=h.split('class="anim-fulllink"',1)[1].split('</p>',1)[1]
                prose=after.split('<div class="inpart">',1)[0]
            else:  # hub: intro paragraph(s) live BEFORE the hero (animbox/posthero/figure)
                seg=h.split('class="post-meta"',1)[1].split('</p>',1)[1]
                prose=re.split(r'<(?:div class="(?:posthero|animbox)"|figure)', seg, maxsplit=1)[0]
            body=html_to_body_md(prose)
        if slug in LABELS:
            navlab,blurb=LABELS[slug]
            if navlab: fm["nav_label"]=navlab
            if blurb:  fm["blurb"]=blurb
        open(os.path.join(CONTENT,slug+".md"),"w").write(dump_fm(fm)+body)
        print("  wrote content/%s.md" % slug)

# ================================================================ BUILD
def scope_css(css):
    css=re.sub(r'(?m)^(\s*)body(\b)', r'\1:scope\2', css)
    css=re.sub(r'(?m)^(\s*)html(\b)', r'\1:scope\2', css)
    css=re.sub(r'(?m)^(\s*):root(\b)', r'\1:scope\2', css)
    return "@scope (.animbox) {\n"+css+"\n}\n"
def widget(anim_file):
    h=open(os.path.join(BLOG,anim_file)).read()
    style=first(r'<style>(.*?)</style>', h)
    body=h.split('</nav>',1)[1].split('</body>',1)[0]
    body=body.replace('src="study-data.js"', 'src="../study-data.js"')
    body=re.sub(r'<h1>.*?</h1>\s*','',body,count=1,flags=re.S)
    body=re.sub(r'<div class="subtitle">.*?</div>\s*','',body,count=1,flags=re.S)
    return ('<div class="animbox">\n<style>\n'+scope_css(style)+'</style>\n'+body.strip()+'\n</div>')

def hero(fm):
    parts=[]
    if fm.get("hero_title"): parts.append(f'<div class="posthero-title">{fm["hero_title"]}</div>')
    parts.append(widget(fm["animation"]))
    if fm.get("hero_caption"): parts.append(f'<p class="anim-cap">{fm["hero_caption"]}</p>')
    parts.append(f'<p class="anim-fulllink"><a href="../{fm["animation"]}" target="_blank" rel="noopener">Open this animation in its own page &#8599;</a></p>')
    return '<div class="posthero">\n'+"\n".join(parts)+'\n</div>'

def body_html(md_body):
    # protect math -> pandoc (no math/citation parsing) -> restore raw $...$ for browser MathJax
    md_body, store = protect_math(md_body)
    out = pandoc(md_body, "markdown+fenced_divs+pipe_tables-citations-tex_math_dollars", "html")
    out = restore_math(out, store)
    out = out.replace("<table>", '<table class="tbl">')          # our table style
    return out

def inpart(hub, num, pages, here, fmcache):
    parttitle=fmcache[hub]["title"]
    lis=[]
    for p in pages:
        lab=fmcache[p].get("nav_label") or fmcache[p]["title"]
        cls=' class="here"' if p==here else ''
        lis.append(f'        <li{cls}><a href="{p}.html">{lab}</a></li>')
    return ('    <div class="inpart">\n      <div class="inpart-label">Part %d · %s</div>\n'
            '      <ol>\n%s\n      </ol>\n    </div>' % (num, parttitle, "\n".join(lis)))

def pager(slug):
    order=[s for s,_ in ORDER if s!="index"]
    i=order.index(slug); prev=order[i-1] if i>0 else "index"; nxt=order[i+1] if i+1<len(order) else None
    def title_of(s):
        if s=="index": return "meta-springs"
        return re.sub(r'<[^>]+>','',_titles.get(s,s))
    left =(f'<a class="pager-prev" href="{"../index.html" if prev=="index" else prev+".html"}">'
           f'<span class="pager-label">Previous</span><span class="pager-title">&larr; {title_of(prev)}</span></a>')
    right=('<span class="pager-empty"></span>' if not nxt else
           f'<a class="pager-next" href="{nxt}.html"><span class="pager-label">Next</span>'
           f'<span class="pager-title">{title_of(nxt)} &rarr;</span></a>')
    return f'  <nav class="pager">\n    {left}\n    {right}\n  </nav>'

def series_list(pages, fmcache):
    out=['<ol class="series-list">']
    for p in pages:
        lab=fmcache[p].get("nav_label") or fmcache[p]["title"]
        blurb=fmcache[p].get("blurb","")
        out.append(f'  <li><a href="{p}.html">{lab}</a>\n      <span class="series-blurb">{blurb}</span></li>')
    out.append('</ol>')
    return "\n".join(out)

def faq_html(body):
    if not body.strip(): return ""
    html=body_html(body)
    chunks=re.split(r'(<h3[^>]*>.*?</h3>)', html, flags=re.S)
    head=chunks[0]
    items=[]; i=1
    while i<len(chunks):
        q=re.search(r'<h3[^>]*>(.*?)</h3>', chunks[i], re.S).group(1)
        a=chunks[i+1] if i+1<len(chunks) else ""
        items.append(f'<details class="faq-item"><summary>{q}</summary>\n{a.strip()}\n</details>')
        i+=2
    return ('<details class="faq">\n<summary>FAQ</summary>\n<div class="faq-body">\n'
          +head.strip()+"\n"+"\n".join(items)+"\n</div>\n</details>")

_titles={}
def build():
    fmcache={}
    for slug,kind in ORDER:
        fm,body=parse_fm(open(os.path.join(CONTENT,slug+".md")).read())
        fmcache[slug]=fm; fmcache[slug]["_body"]=body
        _titles[slug]=fm.get("title", fm.get("site_title",slug))
    for slug,kind in ORDER:
        fm=fmcache[slug]; body=fm["_body"]
        if kind=="index": html=render_index(fm,body,fmcache); out=os.path.join(BLOG,"index.html"); home=""
        else:             html=render_post(slug,kind,fm,body,fmcache); out=os.path.join(POSTS,slug+".html")
        open(out,"w").write(html)
        print("  built", os.path.relpath(out,BLOG))

def page_head(title, home):
    return ('<!DOCTYPE html>\n<html lang="en">\n<head>\n<meta charset="UTF-8">\n'
     '<meta name="viewport" content="width=device-width, initial-scale=1">\n'
     f'<title>{title}</title>\n<link rel="stylesheet" href="{home}style.css">\n'+MATHJAX.replace("{home}", home)+'\n</head>\n<body>\n'
     +BLOGNAV.format(home=home)+"\n")

def render_post(slug,kind,fm,body,fmcache):
    sections = re.split(r"^## FAQ\s*$", body, maxsplit=1, flags=re.M)
    body = sections[0]
    faq = sections[1] if len(sections) > 1 else ""
    hub,num,pages=part_of(slug)
    H=page_head(fm["title"]+" — meta-springs","../")
    H+='<main class="wrap postwrap">\n  <article class="postbody">\n'
    if kind=="page":
        H+=f'    <p class="breadcrumb"><a href="{hub}.html">Part {num} &middot; {fmcache[hub]["title"]}</a> &rsaquo; {order_label(slug)}</p>\n'
        H+=f'    <h1 class="post-h1">{fm["title"]}</h1>\n    <p class="post-meta">Thodoris Papakonstantinou</p>\n\n'
        H+='    '+hero(fm)+'\n\n'
        H+=body_html(body)+'\n\n'
        H+=inpart(hub,num,pages,slug,fmcache)+'\n'
    else:  # hub
        H+=f'    <p class="post-kind">Part {num} of 3 &middot; meta-springs</p>\n'
        H+=f'    <h1 class="post-h1">{fmcache[hub]["title"]}</h1>\n    <p class="post-meta">Thodoris Papakonstantinou</p>\n\n'
        H+=body_html(body)+'\n\n'
        H+='    '+hero(fm)+'\n\n'
        H+='<h2 class="section-h">In this part</h2>\n'+series_list(pages,fmcache)+'\n'
    H+='\n'+faq_html(faq)+'\n  </article>\n'+pager(slug)+'\n</main>\n'+FOOTER+'\n</body>\n</html>\n'
    return H

def render_index(fm,body,fmcache):
    H=page_head(fm.get("site_title","meta-springs")+" — blog","")
    H+='<main class="wrap">\n'
    H+=f'  <h1 class="site-title">{fm.get("site_title","meta-springs")}</h1>\n'
    H+=f'  <p class="site-tagline">{fm.get("tagline","")}</p>\n\n'
    H+='  <section class="lead">\n'+body_html(body)
    H+='    <p class="byline">Thodoris Papakonstantinou</p>\n  </section>\n\n'
    H+='  <h2 class="section-h">The series</h2>\n'
    H+='  <p style="color:var(--muted);font-size:14px;margin:0 0 14px;">Every page carries its own interactive spring animation, all on the same 4-treatment network.</p>\n'
    H+='  <ol class="series-list">\n'
    for hub,num,pages in PARTS:
        H+=f'    <li><a href="posts/{hub}.html">Part {num} — {fmcache[hub]["title"]}</a>\n'
        H+=f'        <span class="series-blurb">{fmcache[hub].get("blurb","")}</span>\n'
        links=" &nbsp;·&nbsp; ".join(f'<a href="posts/{p}.html">{order_label(p)} · {fmcache[p].get("nav_label",p)}</a>' for p in pages)
        H+=f'        <span class="series-blurb">{links}</span></li>\n'
    H+='  </ol>\n</main>\n'+FOOTER+'\n</body>\n</html>\n'
    # section-h class on any body h2 (e.g. Paper & previous work)
    H=H.replace("<h2>","<h2 class=\"section-h\">")
    return H

if __name__=="__main__":
    cmd = sys.argv[1] if len(sys.argv)>1 else "build"
    if   cmd=="extract": extract()
    elif cmd=="build":   build()
    else: print(__doc__); raise SystemExit(1)
