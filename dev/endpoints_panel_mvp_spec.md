# Endpoints Panel MVP Spec

## Goal

Turn the `Endpoints` panel into a reusable endpoint-analysis workstation rather than a passive overlay picker.

The immediate design target is the AGP `k=6` embedding-geometry workflow:

- compare a small number of candidate endpoint configurations,
- inspect why a vertex was accepted or rejected,
- make manual corrections,
- save the curated result and the exact method state,
- reuse the same workflow in AGP, symptoms, and future projects.

This document is intentionally narrower than a full endpoint framework. It defines the smallest version that would already make the current AGP endpoint-tuning loop much faster and more reproducible.

## Problem

The current panel is too thin for endpoint work.

- It assumes one `endpoint_run`, expanded mostly by `k`.
- It can overlay precomputed endpoint labels, but it cannot compare multiple algorithm/parameter candidates as first-class objects.
- It has no concept of a working set, acceptance state, rejection state, or manual overrides.
- It cannot explain why a vertex is in or out.
- It does not treat expensive endpoint computations and evenness fits as cached assets.

For AGP `k=6`, that forces an external loop:

1. run scripts,
2. inspect counts,
3. generate another run,
4. inspect visually elsewhere,
5. export or hand-edit CSVs.

That loop should move into `gflowui`.

## Product Position

The MVP should be a visual review and curation tool for endpoint candidates, not a generic “run any graph algorithm” platform.

The core object is:

- a graph at `(project, graph_set_id, k)`,
- a small set of endpoint candidate datasets on that graph,
- one working endpoint set that the user can curate.

## Primary Workflow

### Workflow A: Compare Candidates

Target example:

- AGP, `graph_set = all`, `k = 6`
- candidate 1: geometry `msq=0.97, min.scale.stability=1, ssr=1`
- candidate 2: geometry `msq=0.98, min.scale.stability=1, ssr=1`
- candidate 3: geometry `msq=0.99, min.scale.stability=1, ssr=1`

User flow:

1. open a graph and choose `k`,
2. select or load 2-4 candidate endpoint datasets,
3. toggle them on and off independently,
4. inspect endpoint markers directly on the 3D embedding,
5. click a vertex to see its endpoint metrics and candidate membership,
6. decide which candidate is closest to the intended arm-tip behavior.

### Workflow B: Curate Working Endpoints

User flow:

1. choose one candidate as the starting working set,
2. remove false positives,
3. add missed arm tips manually,
4. adjust labels,
5. save the curated result as a reusable endpoint dataset.

### Workflow C: Recompute from Cached Inputs

User flow:

1. open the `Compute` section,
2. tweak a small number of method parameters,
3. reuse cached evenness fits and cached base endpoint scores when possible,
4. create a new candidate dataset without leaving the UI.

This workflow is phase 2 of the MVP, but the data model should support it from the beginning.

## MVP Scope

### In scope

- compare multiple endpoint datasets on one graph,
- expose endpoint metrics per vertex,
- support one working endpoint set,
- support manual accept/reject/add/edit,
- persist curation state inside `gflowui`,
- use cached fits and cached endpoint bundles,
- save curated datasets as named endpoint assets.

### Out of scope for MVP

- arbitrary algorithm composition,
- live recomputation for large sweeps in the first UI release,
- full undo/redo,
- collaborative multi-user editing,
- writing back into AGP or symptoms source directories automatically,
- large batch orchestration across all `k` graphs from the panel.

## UX Model

The current panel should evolve from:

- `method/k checkbox list`

to:

- `Candidate Datasets`
- `Working Endpoints`
- `Vertex Inspector`
- `Compute`
- `Layout`

### Section 1: Candidate Datasets

Purpose:

- show all available endpoint datasets for the current graph,
- support side-by-side comparison,
- keep source datasets read-only.

Each candidate row should show:

- visible toggle,
- dataset name,
- method badge,
- `k`,
- endpoint count,
- short parameter summary,
- provenance badge: `precomputed`, `derived`, `manual`, `ui-computed`,
- action buttons:
  - `Use as Working Set`
  - `Inspect Only`
  - `Duplicate`

Recommended row labels:

- `geom q99 stab=1 ssr=1`
- `geom q98 stab=1 ssr=1`
- `evenness minima`
- `core eccentricity`

### Section 2: Working Endpoints

Purpose:

- represent the currently editable endpoint set for the graph,
- make acceptance state explicit.

This should be a table, not just a marker overlay.

Columns:

- `show`
- `accept`
- `vertex`
- `label`
- `source`
- `score`
- `stability`
- `notes`
- `remove`

Behavior:

- rows imported from a candidate start as `accept = TRUE`,
- rows manually added also enter here,
- rows can be rejected without deleting provenance,
- labels are editable,
- accepted rows define the rendered “working endpoint” overlay.

### Section 3: Vertex Inspector

Purpose:

- explain endpoint calls.

When the user clicks a vertex in Plotly, show:

- vertex id,
- 3D coordinates,
- graph degree,
- component id,
- membership in each visible candidate,
- whether it is in the working set,
- endpoint metrics:
  - `s.min`
  - `s.q`
  - `m`
  - `score`
  - smoothed score
  - scale stability
  - scale coverage
- candidate-specific flags:
  - `is.local.max`
  - `passes score threshold`
  - `passes stability threshold`
- optional nearest accepted endpoint in graph distance.

This section is important. The panel is not useful if it only shows markers and counts.

### Section 4: Compute

Purpose:

- create candidate datasets for the current graph from cached assets.

For MVP, this should support one method first:

- `embedding_geometry`

Controls:

- `score.metric`
- `k scales`
- `q`
- `neighbor weighting`
- `smooth`
- `min.score.quantile`
- `min.scale.stability`
- `scale.stability.radius`
- `detect.max.radius`
- `detect.min.neighborhood.size`

Recommended presets for AGP `k=6`:

- `strict`: `q99 / stab=1 / ssr=1 / dmr=2`
- `moderate`: `q98 / stab=1 / ssr=1 / dmr=2`
- `balanced`: `q97 / stab=1 / ssr=1 / dmr=2`

The first UI version does not need to expose every possible parameter. It does need:

- named presets,
- exact parameter display,
- a `Run` action that reuses caches,
- a `Save as Candidate` action.

### Section 5: Layout

Keep the existing marker/label controls, but split the overlays:

- candidate overlay styling,
- working endpoint styling.

The working set should have a stronger visual emphasis than comparison candidates.

## Visual Behavior

### Overlay Strategy

The graph view should support these layers:

1. graph vertices/edges,
2. visible candidate endpoint sets,
3. working endpoints,
4. selected vertex,
5. optional local maxima or rejected candidates.

Recommended styling:

- candidates: smaller markers, semi-transparent, distinct colors per candidate,
- working endpoints: larger solid markers,
- selected vertex: halo or ring marker,
- rejected working rows: hidden by default or shown only in inspector mode.

### Comparison Rules

Do not merge candidate datasets before rendering.

The user must be able to see:

- which vertices are shared across candidates,
- which are unique to one candidate.

That implies candidate-aware color or shape encoding, not one flat union.

## Data Model

The current manifest exposes `endpoint_runs`. The UI should normalize those into a richer internal model without forcing an immediate breaking manifest change.

### Internal Objects

#### 1. `endpoint_dataset`

Read-only or user-created candidate on one graph.

Fields:

- `dataset_id`
- `project_id`
- `graph_set_id`
- `k`
- `method`
- `label`
- `origin`
- `status`
- `created_at`
- `source_files`
- `params`
- `summary`
- `vertices`
- `labels`
- `metrics_file`

Notes:

- `origin` in `{manifest, ui-computed, manual, imported}`
- `status` in `{ready, missing, computing, failed}`
- `metrics_file` should point to a per-vertex table when available

#### 2. `working_endpoint_set`

Editable curation state for one graph.

Fields:

- `working_id`
- `project_id`
- `graph_set_id`
- `k`
- `base_dataset_id`
- `rows`

Each row:

- `vertex`
- `accepted`
- `visible`
- `label`
- `auto_label`
- `source_type`
- `source_dataset_id`
- `manually_added`
- `manually_removed`
- `notes`
- `updated_at`

#### 3. `endpoint_compute_cache`

Metadata for reusable expensive computations.

Fields:

- `cache_key`
- `project_id`
- `graph_set_id`
- `k`
- `method`
- `fit_cache_key`
- `score_cache_key`
- `params`
- `files`
- `created_at`

## Persistence Strategy

### Guiding rule

Curated UI state belongs to `gflowui`, not to AGP or symptoms source trees.

Persist under the `gflowui` project data area, keyed by project id and graph identity.

Recommended directory shape:

```text
<gflowui-data>/projects/<project_id>/
  endpoint_state/
    graph_set=<id>/
      k=<kk>/
        candidates/
          <dataset_id>.rds
        working/
          current.rds
          snapshots/
            <timestamp>_<label>.rds
        compute_cache/
          fits/
          endpoint_scores/
```

This keeps:

- manifest-provided assets immutable,
- UI-generated candidates separate,
- manual curation durable across sessions.

## Caching Strategy

Caching the evenness conditional estimate is not optional. It should be a first-class part of the design.

### Cache layers

#### Layer 1: Conditional expectation fit cache

Cache the expensive graph regression fit separately.

Key should include:

- `project_id`
- `graph_set_id`
- `k`
- outcome id or method family
- graph identity hash
- fit parameters

This is the cache you already wanted for AGP threshold testing.

#### Layer 2: Base endpoint score cache

Cache the method output before final thresholding when that decomposition exists.

For `embedding_geometry`, this means caching a base result containing:

- per-vertex raw scores,
- smoothed scores,
- per-scale maxima,
- per-scale support,
- scale stability,
- score threshold metadata.

Then changing only `min.scale.stability` or other cheap filters does not rerun the whole method.

#### Layer 3: Candidate dataset cache

Saved candidate datasets should point back to the base score cache plus the final thresholds used.

This allows:

- exact provenance,
- easy duplication and retuning,
- no confusion between “base detect” and “accepted endpoints”.

## Backend Contract for Endpoint Geometry

To make the UI useful for the new method, one candidate dataset should be able to carry both endpoint ids and per-vertex diagnostics.

Minimum per-vertex metric table:

- `vertex`
- `s.min`
- `s.q`
- `m`
- `score`
- `score.smooth`
- `scale.stability`
- `scale.coverage`
- `is.local.max`
- `is.endpoint`

Optional but valuable:

- per-scale score columns,
- per-scale local-max flags,
- per-scale support flags,
- neighborhood sizes,
- nearest accepted endpoint distance.

If a dataset lacks metrics, the inspector can still work in reduced mode, but the geometry workflow should always ship them.

## Proposed Evolution of Asset Contract

Do not replace `endpoint_runs` immediately.

Instead:

1. keep `endpoint_runs` as the project-manifest source contract,
2. add an internal normalization layer to convert runs into `endpoint_dataset` objects,
3. allow UI-generated candidates and manual datasets to live beside manifest datasets in app state.

Later, if the model stabilizes, the external contract can evolve from `endpoint_runs` to `endpoint_datasets`.

## Candidate Comparison Semantics

Comparison should be graph-specific and explicit.

Rules:

- a candidate dataset is always tied to one `(graph_set_id, k)`,
- the panel may show only candidates matching the current graph,
- the panel should never silently map endpoint ids across different `k`,
- if the user changes `k`, the panel should preserve saved working sets per `k`.

## Manual Editing Rules

### Add

- Plotly-only for MVP
- click to add one vertex
- lasso/box to choose from multiple vertices

### Remove vs Reject

Keep both concepts separate.

- `remove` means delete a manual addition entirely
- `reject` means keep provenance but exclude from accepted working set

This matters when the starting point is a candidate dataset and the user wants an audit trail.

### Labels

Use project-aware label initialization if available.

Fields:

- `auto_label`
- `label`

The UI should never overwrite a user-edited label just because the source dataset changes.

## Why This MVP Is Enough

If this version exists, the AGP `k=6` question becomes a normal UI session:

1. load the three geometry candidates,
2. compare them visually,
3. inspect ambiguous arm tips,
4. choose a working base,
5. fix misses and false positives,
6. save a curated endpoint set.

That is already a large improvement over the current script-driven loop.

## Implementation Plan

### Milestone 1: Internal Model

- normalize manifest `endpoint_runs` into internal candidate datasets
- add persistent per-graph `working_endpoint_set`
- add storage helpers for UI-generated candidate datasets

### Milestone 2: Panel Redesign

- replace method/k table with `Candidate Datasets`
- add `Working Endpoints`
- add `Vertex Inspector`
- keep current layout controls

### Milestone 3: Plotly Selection

- add click and lasso selection
- add manual endpoint add/remove
- add working-set row editing

### Milestone 4: Metrics + Provenance

- surface per-vertex metrics in inspector
- show candidate membership and threshold provenance
- support saving working set snapshots

### Milestone 5: Compute from Cache

- add `embedding_geometry` compute form
- resolve fit cache and base score cache
- save UI-computed candidate datasets

## Concrete First Slice

The best first implementation slice is:

1. candidate datasets list,
2. working endpoints table,
3. vertex inspector driven by Plotly click,
4. import one candidate as working set,
5. manual add/remove/relabel,
6. save working set to `gflowui` state.

Do not start with live compute controls. The panel becomes valuable before that, and this slice will reveal what compute affordances are actually missing.

## AGP k=6 Defaults for Initial Demo

For the first real demo in `gflowui`, preload these candidate datasets:

- `geometry q97 stab=1 ssr=1 dmr=2`
- `geometry q98 stab=1 ssr=1 dmr=2`
- `geometry q99 stab=1 ssr=1 dmr=2`

Those are the three settings that came out of the focused sweep as the best visual-review candidates.

## Open Questions

Questions that should be answered after the first panel slice is usable:

- Should working-set curation support per-row comments beyond a free-text note?
- Should the UI expose rejected source endpoints as a separate overlay?
- Should candidate comparison support color-by-candidate, shape-by-candidate, or both?
- Should saved working sets be exportable to AGP/symptoms-compatible CSV immediately or later?
- Should the compute panel stay method-specific or become a plugin surface after geometry stabilizes?

## Recommendation

Proceed with the `Endpoints` panel MVP, but keep the first implementation tightly scoped:

- comparison,
- inspection,
- curation,
- persistence.

That is the smallest useful version that directly addresses the AGP endpoint-selection problem and sets up a reusable tool for future graph/dataset projects.
