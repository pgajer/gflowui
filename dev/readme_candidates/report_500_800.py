"""Summarize completed range expansion from retained admission and fit records."""
from pathlib import Path
import json,collections,csv,subprocess
root=Path('/Users/pgajer/current_projects/suitesparse_embedding_comparison')
rows=[];terminations=collections.Counter()
for lo,hi in [(500,600),(600,700),(700,800)]:
 out=root/f'square_{lo}_{hi}'
 admission=json.loads((out/'admission.json').read_text())['records'];counts=collections.Counter(r['status'] for r in admission)
 results=list(csv.DictReader((out/'embedding_results.csv').open()))
 assert len(results)==counts['eligible']*3
 terminations.update(r['termination'] for r in results)
 rows.append((lo,hi,len(admission),counts,results))
manifest=json.loads((root/'viewer_manifest.json').read_text())
text='''# SuiteSparse gallery expansion: 501–800 vertices

The three additional size ranges supply 57 connected graphs for visual selection. Every graph has three 3D full metric-MDS embeddings computed with SGD, using unweighted undirected graph shortest-path targets, uniform pair weights, random initialization, seeds 11, 29 and 43, and a 1,000-iteration budget. No edge-KK refinement is applied.

The frozen SuiteSparse metadata was used to select square matrices before downloading. Each conversion sums duplicate matrix entries, drops resulting zeros and self-loops, symmetrizes the nonzero pattern, and retains all vertices. Disconnected graphs are excluded. Duplicate detection compares exact adjacency in the original vertex order; it does not establish equivalence under arbitrary relabeling. Matrix coefficients are not used as edge weights.

The ranges exclude their lower endpoint and include their upper endpoint: 500-vertex graphs were already covered, and graphs with 600 and 700 vertices each occur in exactly one range. Source archives, conversion records and excluded graphs remain available.

| Vertices | Downloaded | Connected distinct | Disconnected | Duplicate patterns | Fits |
|---|---:|---:|---:|---:|---:|
'''
for lo,hi,n,c,r in rows:text+=f"| {lo+1}–{hi} | {n} | {c['eligible']} | {c['disconnected']} | {c['duplicate']} | {len(r)} |\n"
text+='\n## Results and viewing\n\n'
text+=f"All 171 fits completed. Stopping statuses: {dict(terminations)}. An iteration limit is not a convergence claim. All unordered vertex pairs are evaluated for distance-preservation diagnostics. Up to four jobs ran concurrently, so fit times are descriptive rather than a controlled speed comparison.\n\nThe active project contains {len(manifest['graphs'])} graphs: the 22 user-selected favorites plus 57 new candidates. Favorites remain unchanged. [Open gflowui](http://127.0.0.1:3868/) and select **SuiteSparse 3D Embedding Comparison**. New candidates offer metric-MDS (SGD); previous methods remain available on retained graphs where they were computed. The Inspector exposes all three seeds.\n\n"
for lo,hi,*_ in rows:
 out=root/f'square_{lo}_{hi}'
 text+=f"- **{lo+1}–{hi}:** admission [Markdown]({out}/README.md) · [HTML]({out}/README.html); embeddings [Markdown]({out}/embeddings.md) · [HTML]({out}/embeddings.html).\n"
text+='\nUse Add to favorites while reviewing. Export favorites saves a JSON file to Downloads and displays its absolute path. Removing a graph from the active viewer does not delete its source assets. The earlier 37-graph compressed bundle is unchanged; a final package dataset can be built after selection.\n'
out=root/'size_500_800_summary.md';out.write_text(text)
subprocess.run(['pandoc',str(out),'--standalone','--metadata','title=SuiteSparse gallery expansion','-o',str(out.with_suffix('.html'))],check=True)
print('Verified summary:',len(manifest['graphs']),'active graphs;',dict(terminations))
