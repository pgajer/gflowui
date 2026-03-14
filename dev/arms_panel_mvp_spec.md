# Arms Panel MVP Spec

## Reminder

`MVP` stands for `Minimum Viable Product`.

In this document, it means:

- the smallest `Arms` workflow that is already useful in `gflowui`
- narrow enough to implement safely
- structured so richer arm/branch/corridor workflows can be added later

## Goal

Add an `Arms` top-level accordion section to `gflowui` that lets the user:

1. select a pair of endpoints
2. build a shortest-path skeleton between them in the weighted graph
3. generate one or more thickened arm variants around that skeleton
4. compare, save, reload, and curate those arm variants

The panel should support both:

- vaginal-microbiome arm analyses such as `Li-Lj`, `Li-Lc`, `Li-Gv`
- AGP-style `center -> endpoint` arms using a virtual center endpoint

The MVP is intentionally pair-driven and single-arm focused. It is not yet a
general branch partition platform.

## Existing Computational Backbone

The design should wrap existing arm/tube functions rather than invent new ones.

Relevant current code:

- `ZB/analysis/asv/010-parametrize_arm.R`
- `ZB/analysis/workflows/chm_gateway/104-cmb_lilj_tube_counts_k10.R`
- `ZB/analysis/workflows/chm_gateway_prep/095-zmb_k10_endpoint_pair_branches_soft_hard.R`
- `gflow/R/gfc_flow.R`:
  - `construct.path.through.waypoints()`
- `gflow/R/harmonic_extension.R`:
  - `compute.harmonic.extension()`
- `gflow/R/graph_utils.R`:
  - `select.path.corridor()`
  - `select.path.neighborhood()`

This implies the MVP should support, at minimum:

- weighted shortest-path skeletons
- graph-metric corridor/tube selection
- harmonic-extension-based arm parameterization

## Core Concepts

### Arm Family

An `arm family` is defined by an endpoint pair.

Examples:

- `Li-Lj`
- `Li-Lc`
- `Li-Gv`
- `CENTER-Liners`

This is the stable semantic object.

### Arm Skeleton

An `arm skeleton` is an ordered path between the two arm endpoints.

In the MVP, this is:

- the weighted shortest path in the graph

Later, this can be extended to:

- waypoint-connected paths
- constrained shortest paths
- manually edited paths

### Arm Variant

An `arm variant` is a particular thickening/parameterization built on top of one
arm skeleton.

Examples:

- path only
- corridor
- hop tube
- geodesic tube
- harmonic extension

Different variants may exist for the same arm family, and one of them may be
marked as the default variant for that family.

## Product Position

The `Arms` panel should play the same role for quasi-1D corridor objects that
`Endpoints` now plays for terminal-tip objects:

- repository of saved arm datasets
- one current editable working copy
- inspection-first visual workflow
- explicit save/load actions
- clear provenance

The panel should not, in the MVP, try to solve:

- full graph partition into branches
- automatic discovery of all arms
- batch creation across all endpoint pairs

## Primary Workflows

### Workflow A: Build One Arm

1. choose endpoint A
2. choose endpoint B
3. choose path method
4. choose thickening method and parameters
5. preview the arm
6. add it to the working arm set
7. save it as a named arm dataset

### Workflow B: Compare Variants Of One Endpoint Pair

1. load an arm family such as `Li-Lj`
2. compare `path only`, `corridor`, `geodesic tube`, and `harmonic extension`
3. inspect the overlay in 3D
4. save the preferred variant as the default arm dataset for that family

### Workflow C: Build AGP Center Arms

1. choose virtual endpoint `CENTER`
2. choose one real endpoint
3. compute the center-to-tip path
4. thicken it
5. save the resulting arm family/variant

## MVP Scope

### In scope

- a new top-level `Arms` section
- endpoint-pair selection
- support for virtual endpoints
- weighted shortest-path computation
- 4 thickening choices:
  - `path`
  - `corridor`
  - `graph tube`
  - `harmonic extension`
- saved arm datasets
- one editable working arm set
- default dataset per arm family
- overlay visualization in the existing graph view

### Out of scope for MVP

- automatic arm discovery
- multi-waypoint interactive editing
- full branch partitioning like the ZMB `095` script
- bulk construction of all endpoint pairs
- statistical arm analyses
- automatic center detection algorithms

## UI Layout

Add `Arms` as a top-level accordion peer of:

- `Graphs`
- `Endpoints`
- `Conditional Expectations`

Within `Arms`, use this section order:

1. `Working Arms`
2. `Arm Builder`
3. `Arm Datasets`
4. `Arm Inspector`
5. `Arm Layout`

## Section 1: Working Arms

Purpose:

- hold the current editable set of arms
- control which arms are actively shown and curated

This should parallel the current `Working Endpoints` concept.

### Header

- `Working Arms (n)`
- `Show Working Arms` checkbox
- optional status badge:
  - `Clean`
  - `Modified`
  - `Recovered Draft`

### Table

Columns:

- `ARM`
- `PAIR`
- `METHOD`
- `N`
- `ACTIONS`

Where:

- `ARM` is the current display label, editable later
- `PAIR` is endpoint-pair label, e.g. `Li-Lj`
- `METHOD` is the thickening/parameterization method
- `N` is the number of vertices in the arm
- `ACTIONS` initially contains:
  - `Hide`

### Behavior

- selecting a row highlights that arm in the 3D graph
- selecting a row populates `Arm Inspector`
- hidden rows move to a collapsed `Hidden Arms (n)` section
- hidden rows support:
  - `Restore`
  - `Delete`

### Save Semantics

The working arm set is autosaved as a draft.

`Save Snapshot` in the builder/inspector creates a new arm dataset from the
current working set.

## Section 2: Arm Builder

Purpose:

- construct one new arm variant from a selected endpoint pair

### Controls

- `Endpoint A`
- `Endpoint B`
- `Swap` button
- `Use center endpoint` support via virtual endpoint choices

### Path Controls

- `Path method`

MVP choices:

- `Weighted shortest path`

Reserved but not implemented yet:

- `Waypoint-connected path`
- `Constrained path`

### Thickening Controls

- `Thickening method`

MVP choices:

1. `Path only`
2. `Corridor`
3. `Graph tube`
4. `Harmonic extension`

#### Method-specific parameters

For `Corridor`:

- `Relative tolerance`
- `Absolute tolerance`

For `Graph tube`:

- `Tube metric`
  - `hop`
  - `geodesic`
- `Tube radius`

For `Harmonic extension`:

- `Tube metric`
  - `hop`
  - `geodesic`
- `Tube radius`
- `Use edge weights`

### Actions

- `Preview Arm`
- `Add To Working Arms`
- `Save Snapshot`

### Preview Behavior

Preview should show:

- endpoints A and B
- the skeleton path
- the thickened arm vertices

The preview should not yet mutate the working set until the user explicitly
clicks `Add To Working Arms`.

## Section 3: Arm Datasets

Purpose:

- repository of saved arm variants for the current graph set

This is the `Arms` analogue of `Endpoint Datasets`.

### Table

Columns:

- `SHOW`
- `LOADED`
- `ARM`
- `PAIR`
- `METHOD`
- `k`
- `N`
- `ORIGIN`
- `ACTIONS`

### Behavior

- `SHOW` controls overlay only
- `LOADED` shows which dataset the current working arms were loaded from
- `ACTIONS`:
  - `Load`
  - `Rename`
  - `Delete`
  - `Set Default`

Manifest/external arm datasets should remain read-only source datasets in the
same way external endpoint datasets do.

### Defaulting

Default should be tracked per arm family, not only globally.

If there are several `Li-Lj` variants:

- one can be the default `Li-Lj` dataset
- another can exist as a stricter or broader alternative

## Section 4: Arm Inspector

Purpose:

- explain the currently selected arm

### Display

- arm label
- endpoint A / endpoint B
- source k
- path method
- thickening method
- parameter values
- path length
- arm size
- default status

### Optional tables

- `Skeleton Path`
  - ordered path vertices
- `Arm Vertices`
  - vertex ids in the thickened arm
- later:
  - parameterization coordinates
  - nearest-path projection data

The inspector should be collapsed-friendly because these arm objects can get
large.

## Section 5: Arm Layout

Purpose:

- visual control over how arms are drawn in the graph

This section should be collapsible and collapsed by default.

### Controls

- `Skeleton color`
- `Skeleton width`
- `Tube color`
- `Tube opacity`
- `Endpoint marker size`
- `Arm label size`
- `Arm label offset`
- `Show skeleton`
- `Show tube`
- `Show arm endpoints`

## Virtual Endpoints

The AGP use case requires a virtual `CENTER` endpoint.

The MVP should explicitly support a graph-set-level virtual endpoint registry.

### Virtual endpoint object

Fields:

- `virtual_endpoint_id`
- `label`
- `vertex`
- `type`

MVP only needs:

- `CENTER`

### Behavior

- virtual endpoints appear in endpoint selectors in `Arm Builder`
- they are rendered distinctly from ordinary endpoints
- they participate in arm-family naming

Example:

- `CENTER-Liners`

## Data Model

### Arm dataset row

Each saved arm dataset should store at least:

- `dataset_id`
- `label`
- `arm_family_id`
- `endpoint_a`
- `endpoint_b`
- `endpoint_a_label`
- `endpoint_b_label`
- `path_method`
- `thickening_method`
- `source_k`
- `n_vertices`
- `origin`
- `is_default`

### Arm payload

Each saved payload should contain:

- `path_vertices`
- `path_length`
- `arm_vertices`
- `parameterization`
- `params`
- `virtual_endpoint_info`

### Working arm state

The working draft should store:

- rows with:
  - `dataset_id`
  - `label`
  - `family_id`
  - `visible`
  - `accepted`
  - `is_hidden`
  - `updated_at`
- base dataset provenance
- dirty/recovered state

## Algorithms In MVP

### Path

Use weighted shortest path on the graph.

Edge weights are graph edge lengths and must be respected.

### Thickening 1: Path only

Arm vertices are exactly the skeleton path vertices.

### Thickening 2: Corridor

Use `select.path.corridor()`:

- graph distances from adjacent path segments
- corridor selected by relative and absolute tolerance

This is useful when one wants a soft corridor around the path rather than a
fixed-radius tube.

### Thickening 3: Graph tube

Use `select.path.neighborhood()` or equivalent shortest-distance-to-path logic.

Choices:

- hop-based radius
- geodesic radius

### Thickening 4: Harmonic extension

Use `compute.harmonic.extension()`:

- produces a tubular neighborhood
- produces an arm coordinate on that neighborhood

This should be the default method when the user wants a true quasi-1D arm with
parameterization rather than just a selected set of nearby vertices.

## Save / Load Rules

### Add To Working Arms

- inserts the current previewed arm variant into the working set
- does not automatically save a named dataset

### Save Snapshot

- creates a new saved arm dataset in `Arm Datasets`
- uses a generated temporary name such as:
  - `Arm snapshot 2026-03-12 10:15`
- resets dirty state if the working set becomes based on that snapshot

### Load

- replaces the working set with a copy of the chosen saved arm dataset
- does not edit the source dataset in place

### Dirty working-set prompt

If the working set is modified and the user tries to load another dataset:

- `Replace Working Arms`
- `Save Snapshot And Replace`
- `Cancel`

This should mirror the endpoint workflow.

## Visual Behavior

The graph view should support these arm overlay layers:

- skeleton path
- tube/corridor vertices
- arm label
- endpoints used by the arm

When both endpoints and arms are shown:

- endpoints remain visible
- arm overlays should be visually distinct from endpoint markers
- the selected arm should be highlighted more strongly than unselected arms

## Naming Rules

Arm family labels should default to:

- `<endpoint A label>-<endpoint B label>`

Examples:

- `Li-Lj`
- `Li-Gv`
- `CENTER-Liners`

Arm dataset labels should default to:

- `<family> | <method>`

Examples:

- `Li-Lj | corridor`
- `Li-Lj | harmonic_extension`
- `CENTER-Liners | geodesic_tube`

## Safe Implementation Plan

### Phase 1

- add `Arms` top-level section
- add `Working Arms`
- add `Arm Builder`
- implement:
  - endpoint-pair selectors
  - weighted shortest path
  - path only
  - corridor
  - graph tube

### Phase 2

- add `Arm Datasets`
- add save/load/default flows
- add virtual endpoint support

### Phase 3

- add `Harmonic extension`
- add `Arm Inspector`
- add parameterized-arm display

### Phase 4

- add richer path methods:
  - waypoint-connected
  - constrained path
- add arm-family comparison workflows

## Non-Goals For MVP

- full soft/hard branch partitioning like ZMB `095`
- automatic arm discovery from all endpoint pairs
- center detection algorithms
- batch generation across all k graphs
- downstream arm-statistics execution from the UI

## Acceptance Criteria

The MVP is successful if the user can:

1. choose two endpoints, including a virtual center endpoint
2. compute a weighted shortest path between them
3. build at least one thickened arm around that path
4. save that arm as a named dataset
5. reload that dataset later
6. compare two variants of the same arm family visually in the graph

