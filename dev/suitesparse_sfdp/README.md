# SFDP 3D comparison

Uses Graphviz's SFDP (Scalable Force-Directed Placement, Yifan Hu), the method
identified by [Hu's gallery](https://yifanhu.net/GALLERY/GRAPHS/index73.html).
The [Graphviz manual](https://graphviz.org/docs/layouts/sfdp/) describes the
multilevel force-directed algorithm. `dim=3` and `dimen=3` request three layout
coordinates; ordinary Graphviz rendering/overlap machinery is primarily 2D.
We consume node `pos` coordinates, not its drawn edge paths.

Use Graphviz 16.1.0 (or a separately validated version). Graphviz 15.1.1 has a
documented SFDP crash regression and failed 17 of the first 18 cohort jobs.
Seven failures emitted `Multilevel.c` assertions; seven segmentation faults and
three bus errors had empty Graphviz stderr. The logs do not establish the crash
locations of those latter ten failures or a shared cause for all seventeen.
Small complete-graph tests did not expose it; the sparse multilevel regression
test does. A verified official 16.1.0 source release can be built in a private
prefix without replacing the system installation. `GFLOWUI_SFDP` selects an
explicit binary. The adapter explicitly
passes `-Ksfdp` even when a symlink resolves to `dot`.

From the source checkout, using the project's isolated Python environment:

```sh
OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 NUMBA_NUM_THREADS=1 python dev/suitesparse_sfdp/run.py /path/to/comparison
python dev/suitesparse_sfdp/report.py /path/to/comparison
```

Requires clean committed source and the accepted six-graph combined cohort.
Seeds: 17, 29, 43. The outer supervisor covers preparation, the Graphviz child,
and evaluation with 30 GiB sampled RSS and no wall-clock timeout. Calls are serial;
the lock prevents competing copies of this runner, not unrelated jobs. Repeated
graphs are rejected; interrupted directories and prior method results are retained.
`sfdp_results.json` is a separate append-only result index; include it in the
existing viewer exporter alongside **all** historical indexes.
For the corrected backend, use `--index sfdp_graphviz16_results.json` and pass
that filename as the second argument of `report.py`. Include both SFDP indexes
in the viewer. Backend versions appear in the settings, separating old failures
and successes from the new cohort; the original requests are never rewritten.

Frozen backend settings: K=1, overlap=true (disable overlap-removal postprocessing),
smoothing=none, normalize=false; other backend defaults are version-identified.
All non-tiny connected components are independently laid out. The common pipeline
uses classical placement below five vertices, explicitly labeled and not attributed
to SFDP, and retains isolates. This differs from the gallery's largest-component-only
convention. No MST edges or inferred kNN edges are added.

Graphviz output points are divided by 72 to recover native layout inches. There is
no fitted coordinate normalization or post-hoc unit-edge scaling. K is a force-model
parameter, not a promise that every edge has length one. Identity-scale edge and
path errors depend on the chosen native scale; separately fitted chord/relative
scores retain their existing interpretation. Raw JSON and exact commands are saved.
Successful backend exit does not report convergence; rank/ranges are diagnostics,
not a demand that intrinsically low-dimensional graphs have full rank.

## Display color contract

Question: which edges are short or long in the displayed layout? Surface: existing
gflowui Plotly 3D graph, not a separate dashboard. Default stays uniform gray.
Optional scalar edge length is orange (#C7782A) at the minimum and blue (#3575B2)
at the maximum, with a labeled numeric color bar. Both endpoints of each straight
edge segment receive the same length. The mapping resets per layout, so color is
not a cross-method comparison. No scientific coordinates or scores change.
An equal-length set receives a centered color with a small padded legend range.
This two-root palette supplements geometry and numeric labels. Rotation must not
reload data or refresh Inspector content; visual QA uses the running app.
