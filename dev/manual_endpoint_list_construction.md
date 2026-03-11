# Manual Endpoint selection mechanism



# [GPT]

**Findings**

In AGP, [`17_evenness_endpoints_k7.R`](/Users/pgajer/current_projects/AGP/R/17_evenness_endpoints_k7.R#L1399) does exactly what you described, but with one extra mode. It first fits `evenness.hat` with `gflow::fit.rdgraph.regression()` on the current graph [`17_evenness_endpoints_k7.R`](/Users/pgajer/current_projects/AGP/R/17_evenness_endpoints_k7.R#L1381), then selects auto endpoints by `endpoint.mode`:
1. `evenness_minima` via `gflow::compute.gfc()` [`17_evenness_endpoints_k7.R`](/Users/pgajer/current_projects/AGP/R/17_evenness_endpoints_k7.R#L716)
2. `core_eccentricity` via `gflow::geodesic.core.endpoints()` [`17_evenness_endpoints_k7.R`](/Users/pgajer/current_projects/AGP/R/17_evenness_endpoints_k7.R#L765)
3. `major_arms`, which is also present in that script [`17_evenness_endpoints_k7.R`](/Users/pgajer/current_projects/AGP/R/17_evenness_endpoints_k7.R#L861)

AGP already has a manual curation layer, but it is not a third detector. It seeds a CSV from the auto endpoint set [`17_evenness_endpoints_k7.R`](/Users/pgajer/current_projects/AGP/R/17_evenness_endpoints_k7.R#L440), reads back rows with `include=TRUE` [`17_evenness_endpoints_k7.R`](/Users/pgajer/current_projects/AGP/R/17_evenness_endpoints_k7.R#L556), and if `manual.endpoints.use=TRUE` it replaces the auto set with that curated selection [`17_evenness_endpoints_k7.R`](/Users/pgajer/current_projects/AGP/R/17_evenness_endpoints_k7.R#L1520). Labels are then regenerated with `create.endpoint.labels.local()` and manual label edits override them [`17_evenness_endpoints_k7.R`](/Users/pgajer/current_projects/AGP/R/17_evenness_endpoints_k7.R#L327), [`17_evenness_endpoints_k7.R`](/Users/pgajer/current_projects/AGP/R/17_evenness_endpoints_k7.R#L1534).

Symptoms is simpler. [`20_evenness_endpoints_all_asv_k05.R`](/Users/pgajer/current_projects/symptoms/R/20_evenness_endpoints_all_asv_k05.R#L690) only uses the evenness-minima route on the largest connected component, maps local vertices back to global ids, deduplicates nearby endpoints in layout space, and uses a taxonomy-aware label generator [`20_evenness_endpoints_all_asv_k05.R`](/Users/pgajer/current_projects/symptoms/R/20_evenness_endpoints_all_asv_k05.R#L268), [`20_evenness_endpoints_all_asv_k05.R`](/Users/pgajer/current_projects/symptoms/R/20_evenness_endpoints_all_asv_k05.R#L733).

The current `gflowui` endpoint panel cannot represent this model yet. It resolves one endpoint run, expands it only by `k`, and unions checked rows from that single run [`R/app_server.R`](/Users/pgajer/current_projects/gflowui/R/app_server.R#L1069), [`R/app_server.R`](/Users/pgajer/current_projects/gflowui/R/app_server.R#L1113), [`R/app_server.R`](/Users/pgajer/current_projects/gflowui/R/app_server.R#L1432), [`R/app_server.R`](/Users/pgajer/current_projects/gflowui/R/app_server.R#L2817).

On RGL: `rgl` does have native `select3d()`, but in this Shiny/browser setup there is no practical browser-side point-picking path analogous to Plotly selection. For this app, Plotly should be the endpoint-picking surface.

**Revised Mechanism**

I would not make the editable table live under each source dataset. That creates an ambiguity the moment the same vertex appears in two datasets with two labels. The cleaner model is:

1. `Endpoint Datasets` at the top.
Each dataset is a first-class object tied to `(graph_set_id, k, method, source)`. Examples:
`evenness_k5` for Symptoms,
`evenness_k7` and `core_eccentricity_k7` for AGP.
This should be a one-column checkbox list, as you suggested.

2. `Working Endpoints` underneath.
This is the actual final overlay shown on the graph: the deduplicated union of checked datasets plus manual additions.
This is where the editable rows should live.
Columns:
`include`, `label`, `source`, `vertex`.
This avoids duplicate editing surfaces and makes the final state explicit.

3. `Manual` should be a separate editable dataset, not an automatic copy of the auto set.
My recommendation:
manual starts empty;
auto datasets remain read-only provenance sources;
if the user wants to fork the current selection, add an explicit `Clone current working set to manual` action.
That is cleaner than silently initializing manual from evenness/core endpoints.

4. Vertex picking should be Plotly-only.
I would support:
`Click` mode for single-vertex add,
`Box/Lasso` mode for candidate collection.
If box/lasso returns multiple vertices, do not auto-promote all of them. Show a small candidate table and let the user choose one or more to add to `manual`.

5. Manual label generation should reuse the existing project-specific label logic.
For a newly added manual endpoint, generate `auto_label` with the same logic as the upstream scripts, then initialize editable `label` from that value.
AGP and Symptoms do not use the same label style, so this needs to be a project-aware backend function, not a generic UI heuristic.

6. Persistence should be in `gflowui`, not in the external project directories.
Store manual datasets and per-vertex label/include overrides in the `gflowui` project state.
Optionally add `Export manual endpoints CSV` later, using the AGP-compatible schema already present in [`17_evenness_endpoints_k7.R`](/Users/pgajer/current_projects/AGP/R/17_evenness_endpoints_k7.R#L528).

**Recommendation**

The right abstraction is not “method + k rows” anymore. It is:
1. source endpoint datasets,
2. one deduplicated working endpoint set for the current graph,
3. one manual editable dataset layered on top.

If you want, the next concrete step should be to redesign the manifest/app model from `endpoint_runs` to `endpoint_datasets`, then wire the `Endpoints` panel to `Dataset list + Working Endpoints table` before doing any Plotly selection work.



# [Claude]

