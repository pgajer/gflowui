# SuiteSparse comparison viewer

The viewer reads saved, validated assets. Changing the graph, layout, metric or
camera never invokes an embedding optimizer or recomputes a quality score.
Phase 2 added the interface to the accepted four-graph pilot. Phase 3 provides
additional feature-based backends through a separate result index, preserving
all baseline results. Methods are available only when completed assets exist.

## Build and register

Run from the gflowui source checkout, using the Phase 1 environment:

    python dev/suitesparse_project/export_viewer.py /path/to/pilot-data
    Rscript dev/suitesparse_project/register_project.R /path/to/pilot-data
    Rscript -e 'pkgload::load_all("."); gflowui::run_gflowui(host="127.0.0.1",port=3868,launch.browser=FALSE)'

The default exporter combines pilot_results.json, lle_landmarks16.json and
lle_landmarks32.json. Explicit --indexes can select another complete set.
It writes viewer_manifest.json last, after checking graph/run identities and
creating display diagnostics. No original coordinates or scores are changed.
The display-only Shepard sample contains up to 2,000 uniformly sampled unordered
pairs per component with seed 2718. All pilot quality scores remain exact.
Edge histograms contain every edge, before component display packing.

After the complete Phase 3 matrix and its diagnostics are generated, include it
explicitly without dropping the original indexes:

    python dev/suitesparse_project/report_expansion.py /path/to/pilot-data
    python dev/suitesparse_project/export_viewer.py /path/to/pilot-data --indexes pilot_results.json lle_landmarks16.json lle_landmarks32.json phase03_results.json phase03_trimap_graph_results.json

This adds PaCMAP, LocalMAP, TriMAP, PHATE, LargeVis and NCVis results. Their main input
track is shared landmark-distance features; a separately labeled TriMAP variant
uses original graph distances and does not replace the feature-based results.
Backend preprocessing/settings and
limitations are described in ../suitesparse_phase3/README.md and each run record.
The seed-range panel groups different settings separately and includes the new
methods. LGS remains unavailable until its separate integration gate.

## Walkthrough

1. Select **SuiteSparse 3D Embedding Comparison** in Projects.
2. Select a graph, then a completed **3D embedding / replicate**. The Overview
   reports its conversion, component structure and full available metadata.
3. Rotate/zoom the graph and drag the divider. These operations retain Inspector
   scroll position and expanded sections. Vertex selection/color persist across
   layouts of one graph; vertex selection and camera reset when changing graphs.
4. Sort the quality table by a heading; **Load** selects that exact run. Failed,
   resource-limited and unsupported rows remain visible but cannot be loaded.
5. Compare metric-by-method points, trade-offs and neighborhood curves. Gray
   dashed guides identify the active run. Click a metric/trade-off point to load
   it. Undefined metrics are missing, never zero. Expand seed/tie sensitivity,
   distance diagnostics or the full run record for additional context.
6. In **Export comparison bundle**, choose a local bundle directory and save.
   The app returns the absolute ZIP path. The bundle contains every indexed
   graph/run, including failures, coordinates, metric results, manifests, report
   assets, definitions, checksums and figure settings—not only the displayed row.

## Versioned boundary

Viewer schema 1 has kind gflowui_embedding_comparison, graph identities,
unique run identities, statuses and relative SHA-256-addressed asset paths.
Absolute/traversing paths, modified files, cross-graph layouts, wrong vertex
order and nonfinite/non-3D coordinates are rejected. Geometry is revalidated at
load time. Export revalidates the loaded manifest and every included asset.
The custom project metadata is embedding_comparison with schema_version 1
and data_root; it does not pretend the arbitrary graphs are kNN graphs.

The R adapter/module are private package helpers. Plotly and htmlwidgets remain
optional: ordinary projects do not require them. Registration changes only the
project registry, not existing graph data. Future method placeholders express
availability gaps; they are not empty successful layouts.

Tests: python -m pytest dev/suitesparse_project/test_export_viewer.py and
testthat::test_file("tests/testthat/test-embedding-comparison.R") after loading
gflowui source.
