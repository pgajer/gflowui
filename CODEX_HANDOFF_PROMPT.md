# Codex Handoff Prompt for gflowui

Use this prompt in a new Codex session with root set to:
`/Users/pgajer/current_projects/gflowui`

---

You are continuing development of `gflowui`, a standalone companion R package
for `gflow` that targets biologist-friendly workflows in the VMRC context.

## Project intent

`gflowui` should provide a simple interface to:

1. load and validate data,
2. build/select graphs,
3. compute conditional expectations for outcomes/features,
4. visualize smoothed responses/features in 3D,
5. detect and overlay graph endpoints.

## Current status

The repository currently contains an MVP scaffold:

- package metadata and app wiring,
- UI modules (`Data`, `Graph`, `CondExp`, `Visualize`),
- adapter stubs in `R/services_gflow_adapter.R`,
- a development `ROADMAP.md`.

The compute path is intentionally placeholder and must be replaced with real
`gflow` calls.

## Immediate tasks

1. Replace `gflow_build_graph_stub()` with real
   `gflow::build.iknn.graphs.and.selectk()` integration.
2. Replace `gflow_fit_condexp_stub()` with real
   `fit.rdgraph.regression()`/`refit.rdgraph.regression()` path.
3. Replace endpoint stub with `gflow::geodesic.core.endpoints()` using graph
   adjacency and edge-length structures.
4. Add a first working 3D output in the Visualize tab using
   `gflow::plot3D.cont.html()` or `gflow::plot3D.cltrs.html()`.
5. Keep heavy compute inside adapter/service functions to preserve module
   simplicity.

## Constraints

- Keep `gflow` as a dependency and do not duplicate core algorithms in `gflowui`.
- Keep UI code thin; place computational logic in service adapters.
- Favor robust defaults and explicit status/error messages for non-technical users.
- Preserve reproducibility by tracking selected parameters (k range, method,
  smoothing options) in returned objects.

## Definition of done for next milestone

- A user can load a CSV, choose k-range/method, run graph selection, fit one
  outcome conditional expectation, detect endpoints, and view a 3D HTML result
  in-app.
