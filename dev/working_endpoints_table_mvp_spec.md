# Working Endpoints Table MVP

## Goal

Turn `Working Endpoints` from a read-only summary into the primary lightweight
editing surface for endpoint curation.

The MVP must support:

1. editing the label of any working endpoint in place
2. removing any working endpoint directly from the table

## Scope

This spec applies to the `Working Endpoints` section of the `Endpoints` control
panel in `gflowui`.

It does not add bulk actions, drag reordering, multi-select, undo stacks, or
graph-driven row selection. Those can come later.

## Table Layout

Replace the current visible columns with:

- `VERTEX`
- `LABEL`
- `ACTIONS`

The `SOURCE` field remains in the underlying working-state data model, but it is
not shown in the MVP table.

Rationale:

- `VERTEX` is the stable identifier the user needs for orientation.
- `LABEL` is the field the user edits most often.
- `ACTIONS` is where the remove control belongs.
- `SOURCE` has low immediate value during manual curation and consumes scarce
  sidebar width.

## Row Behavior

### Vertex

- Read-only.
- Displayed as `v<vertex_id>`.

### Label

- Inline editable text input.
- Pre-filled from the current working-set label.
- Commit on input change.
  - For the MVP, `change` means blur or Enter in the browser control.
- If the edited value is blank or whitespace-only, restore the default label:
  - existing `auto_label` if present
  - otherwise `v<vertex_id>`

### Actions

- A single `Remove` button.
- Removes the row from the working set immediately.
- No confirmation modal in the MVP.
- Removal is persisted immediately.

## Persistence Model

Edits in the table are live edits to the current working set.

- Renaming a label updates the graph-set-wide working-state file immediately.
- Removing a row updates the graph-set-wide working-state file immediately.

The `Save Working Set` button remains useful, but as a snapshot/export action,
not as a requirement to preserve label edits or removals.

## Overlay Behavior

If `Show Working Set` is enabled:

- label edits should propagate to endpoint labels on the graph
- row removal should immediately remove the endpoint overlay entry from the graph

## Non-Goals for MVP

- inline editing of vertex ids
- editing `SOURCE`
- undo/redo history
- row-level tooltips or expanded metadata
- bulk rename/bulk remove

## Follow-on Ideas

- make the vertex id clickable to select/highlight that vertex in the graph
- add an `Undo remove` toast
- surface hidden metadata such as `source_type` in `Vertex Inspector`
- add keyboard navigation across working-endpoint rows
