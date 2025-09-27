#!/usr/bin/env python3
import gzip
import re
from pathlib import Path
ROOT=Path(__file__).parent
SRC=ROOT/"src"
DIST=ROOT/"dist"
DIST.mkdir(exist_ok=True)
html=(SRC/"index.html").read_text()
js=(SRC/"main.js").read_text()
def minify_js(code: str) -> str:
    code=re.sub(r"//.*","",code)
    code=re.sub(r"/\*.*?\*/","",code,flags=re.S)
    code=re.sub(r"\s+"," ",code)
    return code.strip()
def minify_html(text: str) -> str:
    text=re.sub(r">\s+"," >",text)
    text=re.sub(r"\s+<","<",text)
    text=re.sub(r"\s+"," ",text)
    return text.strip()
packed_html=html.replace('<script src="main.js"></script>',f'<script>{minify_js(js)}</script>')
packed_html=minify_html(packed_html)
(DIST/"index.html").write_text(packed_html)
with gzip.open(DIST/"index.html.gz","wb",compresslevel=9) as f:
    f.write(packed_html.encode("utf-8"))
print("minified bytes:",len(packed_html.encode('utf-8')))
print("gzipped bytes:",(DIST/"index.html.gz").stat().st_size)
