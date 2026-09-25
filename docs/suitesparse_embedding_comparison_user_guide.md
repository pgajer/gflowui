# SuiteSparse 3D Embedding Comparison

Open the saved **SuiteSparse 3D Embedding Comparison** project in gflowui.
The project reads prepared results; rotating the graph, resizing the Inspector,
or changing plot controls does not run an embedding algorithm or recompute scores.

## Explore a graph

Choose a graph and then an embedding/replicate in the left sidebar. Only completed
layouts can be selected. The Overview describes its original matrix, conversion,
unit-edge graph, connected components, isolated vertices, and source links.
The quality table also lists unsuccessful or excluded methods with their reasons.
Click **Load** or a comparison point to display that exact replicate. Gray dashed
guides identify its score; vertex selections persist between layouts of one graph.

Disconnected components are embedded separately and positioned along the x-axis
only for display. Distances between them have no scientific interpretation.
Isolates are retained; many isolates can make the packed overview elongated.
No minimum spanning tree repair edges were added.

## Read the comparisons

Euclidean distance error and relative stress fit separate scale factors per
connected component. Fixed-path and edge errors use the raw embedding scale.
They answer different questions and should not be combined into one ranking.
Fixed-path preservation alone does not show that a graph has unfolded well.
Neighborhood curves use the original unit-edge graph and stable vertex-ID ties.

The original pilot uses every within-component pair. Larger-graph results use up
to 20,000 uniform pairs per component, shared across methods and replicates.
The table and plots explicitly identify this sampled evaluation. Component sums
are weighted by their full pair populations. Edges and streamed neighborhood
rankings remain exact; cross-component pairs are never evaluated.

Brackets in the table and interval bars in the metric plot are approximate 95%
pair-sampling intervals, conditional on a fixed embedding. They are not uncertainty
over optimizer seeds or future graphs. The separate seed-range section describes
observed replicate variation, not confidence intervals. Sampling validation and
its empirical coverage are included in the exported findings.

Resource exclusions are not measured optimizer failures. A method may be excluded
before execution based on its measured memory/time projections or input contract.
Successful execution means finite saved coordinates, not convergence or good fit.
Metric-MDS retries on nemscem, sstmodel and circuit_2 use a separately authorized
30-GiB memory ceiling, no elapsed-time limit, and serial processing. Their settings
identify this budget; original 2-GiB attempts remain in the table. The optimizer's
iteration limits and convergence tolerances are unchanged. Edge-KK refinements
use the matching new MDS seed. Other methods retain their recorded allowances.
The landmark-feature and original-graph-distance TriMAP variants are distinct.
Experimental paper-form LGS has synthetic locality evidence but no completed
gallery layout under the current resource contract; its plots are explicitly
separate from the selected gallery graph.

## Save reproducible figures and results

Open **Export comparison bundle**, choose a directory, and click **Save ZIP
bundle**. The app reports the full ZIP path. The bundle contains all graphs and
run tables, saved coordinates, metrics, source manifests, definitions and figure
settings, including the separately selected LGS synthetic graph and measure.

PDF and SVG comparison figures use the currently selected gallery graph and
highlight its active run. The figure manifest records unavailable plots instead
of inventing data. To rebuild the figures after extraction, run from the bundle
directory:

```r
source("rebuild_publication_figures.R")
```

This requires base R and jsonlite, not a running Shiny session. Full tables retain
all recorded outcomes, including those omitted from figures because no finite
score exists. The ZIP is a reusable analysis bundle, not an installation of every
optimizer; rerunning optimizations requires the documented pinned environments.

Source design: [project specification](suitesparse_embedding_project_design_2026-09-24.md).
Reproducible processing scripts are under `dev/suitesparse_phase1`,
`dev/suitesparse_phase3`, `dev/suitesparse_phase4`, `dev/suitesparse_phase5`, and
`dev/suitesparse_project`. Large data and results are stored outside the package.
