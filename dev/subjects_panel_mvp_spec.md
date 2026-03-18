# Subjects Panel MVP Spec

## Goal

Add a new top-level `Subjects` accordion section to `gflowui` that lets the
user inspect all graph vertices associated with a selected study subject and
view them as a dedicated overlay on top of the existing 3D graph view.

The first target is the `symptoms` project, where subject identity is already
scientifically meaningful and repeated samples from the same person can form
graph lanes, clusters, or transitions.

The panel should make it easy to answer questions like:

1. where do this subject's samples lie in the graph?
2. are this subject's samples concentrated in one region or spread across
   multiple graph neighborhoods?
3. do repeated samples for this subject visually form a lane or trajectory?

## Product Position

`Subjects` should be a top-level peer of:

- `Graphs`
- `Endpoints`
- `Arms`

It should not replace the existing graph styling pipeline. Instead, it should
add a subject-specific overlay on top of the current graph rendering.

This is the key design decision:

- base graph display remains controlled by the existing graph controls
- selected subject samples are shown as an additional overlay
- subject controls are independent from `Color by`, endpoint overlays, and arm
  overlays

## Naming

Use `Subjects` as the panel name.

Why:

- the workflow naturally starts from a subject selector
- it allows future multi-select
- it fits the existing plural top-level panel naming pattern better than
  singular `Subject`

Alternative names such as `Participants` can be revisited later, but `Subjects`
is the clearest MVP label.

## Core Concepts

### Subject

A `subject` is a study participant identifier shared by one or more samples.

### Subject Samples

The set of graph vertices belonging to a selected subject.

In the MVP, one graph vertex corresponds to one sample.

### Subject Overlay

The visual layer showing the selected subject's sample vertices, optional
within-subject edges, and optional labels.

## Data Assumptions

The `Subjects` panel needs project-level metadata that maps graph vertices to:

- `subject_id`
- `sample_id`
- optional `visit_order`
- optional `collection_time`

For MVP:

- implement a Symptoms-specific subject metadata provider first
- expose it behind a generic provider interface so other projects can opt in
  later

Minimum required fields:

- `vertex`
- `subject_id`

Strongly preferred fields:

- `sample_id`
- `visit_order`

## Scope

### In scope

- one new top-level `Subjects` panel
- single-subject selection
- subject sample table
- subject overlay in Plotly and RGL
- subject-specific marker, edge, and label styling controls
- optional dimming of the non-subject background graph

### Out of scope for MVP

- multi-subject comparison
- subject clustering summaries
- automatic lane detection
- statistical summaries over subjects
- editable subject groupings
- subject-path fitting algorithms

## UI Layout

Within `Subjects`, use this section order:

1. `Subject Selection`
2. `Subject Samples`
3. `Subject Layout`

Keep the panel collapsed by default.

## Section 1: Subject Selection

Purpose:

- choose one subject to inspect
- control the basic subject overlay mode

Controls:

- searchable `Subject ID` dropdown
- `Show Subject Overlay` checkbox
- optional `Dim Background` checkbox
- optional `Background opacity` slider

Behavior:

- selecting a subject does not change the base graph coloring
- selecting a subject populates `Subject Samples`
- if `Show Subject Overlay` is checked, the subject's vertices appear as an
  overlay on the graph

MVP rule:

- support one subject at a time
- later phases can add multi-select

## Section 2: Subject Samples

Purpose:

- show the vertex/sample membership for the selected subject
- make the overlay explainable in table form

Columns:

- `vertex`
- `sample_id`
- `visit_order`
- `notes`

Rendering rules:

- show only columns backed by real data
- if `visit_order` is unavailable, omit that column
- if `sample_id` is unavailable, fall back to `vertex`

Row interaction:

- clicking a row highlights the corresponding subject sample vertex on the graph
- clicking a row can later drive a subject/sample inspector, but that is not
  required for MVP

## Section 3: Subject Layout

Purpose:

- control how the selected subject appears on the graph

Controls:

- `Subject vertex color`
- `Subject vertex size`
- `Subject edge mode`
- `Subject edge color`
- `Subject edge width`
- `Label mode`
- `Label size`

### Edge mode

Important distinction:

- graph-induced edges among subject vertices are not the same thing as temporal
  visit order links

For MVP, support:

- `None`
- `Graph edges among subject vertices`

Do not yet support:

- visit-order polylines
- shortest-path connectors between subject samples

Those can be added later once the metadata contract is stable.

### Label mode

Support these modes:

- `None`
- `Vertex ID`
- `Sample ID`
- `Visit order`

Rules:

- show only modes backed by real metadata
- if `Visit order` is not available, omit it
- prefer meaningful labels over arbitrary running indices

## Overlay Semantics

The subject overlay should be implemented as a dedicated overlay layer in the
same spirit as endpoint and arm overlays.

This means:

- the base graph remains visible
- subject samples are rendered on top
- subject styling is independent of the base `Color by` source

Recommended rendering:

- base graph: existing style
- subject vertices: larger, more saturated markers
- optional subject edges: visually subordinate to subject vertices
- subject labels: off by default unless explicitly enabled

## Behavior By Renderer

### Plotly

Support:

- subject vertex overlay
- optional induced subject edges
- optional labels
- background dimming by lowering base graph opacity

### RGL

Support:

- subject vertex overlay
- optional induced subject edges
- optional labels where practical

The control surface should be renderer-neutral even if label fidelity differs
slightly between Plotly and RGL.

## Metadata Provider Design

Implement a project-specific provider interface similar to the existing label
provider pattern.

Recommended internal contract:

- `subject_ids()`
- `subject_rows(subject_id)`
- `subject_vertex_rows(subject_id)`
- `subject_label_modes(subject_id)`

For Symptoms, the provider should resolve:

- graph vertex to sample mapping
- sample to subject mapping
- optional visit/sample metadata

## State Model

Suggested state for the panel:

- `selected_subject_id`
- `show_subject_overlay`
- `dim_background`
- `background_opacity`
- `subject_vertex_color`
- `subject_vertex_size`
- `subject_edge_mode`
- `subject_edge_color`
- `subject_edge_width`
- `subject_label_mode`
- `subject_label_size`

This state should be graph-set scoped.

## Rendering Data Model

For a selected subject, build an overlay bundle with:

- `subject_id`
- `vertices`
- `coords`
- `sample_ids`
- `visit_order`
- `labels_by_mode`
- `induced_edges`

Where:

- `vertices` are graph vertex ids for the selected subject
- `induced_edges` are graph edges whose endpoints are both subject vertices

## Default Behavior

On first open:

- no subject selected
- `Show Subject Overlay` unchecked
- `Dim Background` unchecked
- `Label mode = None`
- edges off

Once a subject is selected:

- keep the overlay opt-in rather than auto-forced
- preserve subject selection and styling while the graph set remains active

## MVP Implementation Phases

### Phase 1

- add `Subjects` top-level panel
- add single-subject selector
- implement Symptoms subject metadata provider
- show subject sample table
- render subject vertices as overlay

### Phase 2

- add `Subject Layout`
- add edge mode: induced graph edges
- add label modes
- add background dimming

### Phase 3

- add row-click sample highlighting
- add optional subject/sample inspector
- prepare provider interface for non-Symptoms projects

## Non-Goals For This MVP

Do not yet attempt:

- arm-like lane construction for subject samples
- ordering subject samples when no real temporal metadata exists
- pairwise subject comparison
- persistence of manually curated subject groups

Those should wait until the simple subject overlay workflow is proven useful.

## Summary

The MVP `Subjects` panel should be a focused overlay tool:

- select one subject
- see that subject's vertices on the graph
- optionally show within-subject induced graph edges
- optionally label those subject samples with meaningful metadata

That is already immediately useful for the Symptoms project and sets up a clean
foundation for richer longitudinal or lane-oriented subject workflows later.
