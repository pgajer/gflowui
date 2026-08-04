# Basin Tree Event Navigator Performance Evidence

Date: 2026-08-04 (America/New_York)

## Reference context

- Project: `hmp_subject15_k03_heat_basin_path`
- Estimate: Subject 15 graph-heat occupation density, diffusion-time index 4
- Graph component vertices: 6,529
- Maximum basins: 352
- Initially displayed maximum basins: 17
- Renderer exercised live: Plotly
- `gflow` source commit: `5bcdba6f95a35d58d693de4dc668077f4bd48366`

## Event-domain size

| Domain | Positions |
|---|---:|
| Former all-vertex-level domain, including above-maximum sentinel | 6,385 |
| Current displayed proposal | 35 |
| Complete maximum-tree component | 677 |

The proposal domain therefore removes 99.45% of the old slider positions,
while the complete domain removes 89.40%. Both retain exact branch-birth,
merge-plateau, above-maximum, and component-floor events.

## Live timing and counter evidence

Times below are server-side elapsed measurements exposed only as diagnostic
`data-*` attributes on the navigator. They are single-run wall-clock evidence,
not a formal microbenchmark.

| Operation | Elapsed |
|---|---:|
| Proposal static structure and event table | 277 ms |
| Proposal initial above-maximum cut | 2 ms |
| Proposal fresh early-event cut | 3 ms |
| Proposal cached return to above-maximum | 0 ms |
| Complete static structure and event table | 305 ms |
| Complete initial above-maximum cut | 2 ms |
| Complete middle-event cut (event 339 of 677) | 49 ms |
| Complete floor cut (event 677 of 677) | 33 ms |

Initial panel construction performed one static build and one exact cut; it
did not precompute cuts for the other event heights.

For one fresh proposal event committed with **Next event**, counters changed
by exactly:

```text
accepted commits       +1
new canonical cuts     +1
merge-tree renders     +1
linked graph overlays  +1
static tree builds      0
```

Returning to the cached above-maximum event changed counters by:

```text
accepted commits       +1
new canonical cuts      0
merge-tree renders     +1
linked graph overlays  +1
static tree builds      0
```

With **Link h to the 3D graph** disabled, a complete-tree event commit changed
the event and tree render but left the graph-overlay counter unchanged.
Re-enabling the link applied that committed event without another cut or tree
render.

The terminal complete-domain event reported:

```text
Event 677 of 677 — 2 merge plateaus; component floor; complete component active — h = 0
Active: 6,529 vertices in 1 component
```

## Browser behavior exercised

- Horizontal event slider, Previous event, and Next event rendered at full
  tree width.
- Previous/next actions moved exactly one event and produced one accepted
  commit each.
- Previous event was disabled at event 1; Next event was disabled at event
  677.
- Switching proposal to complete scope preserved the exact committed height.
- Cached backtracking reused the exact cut.
- Link-disabled navigation did not compute a graph overlay; re-linking applied
  committed state.
- Browser console inspection returned no warnings or errors.
- A range-control fill generated one preview input followed by one native
  change and resulted in one accepted server commit.

The browser automation surface did not expose a way to pause a native pointer
drag before mouse release. The before-release zero-server-work contract is
therefore covered by the JavaScript regression that isolates the `input`
handler and proves it contains no `Shiny.setInputValue()` call, plus source
inspection of the client-only `Plotly.relayout()` path. A human-held drag is a
useful independent audit check.

## Availability limitation

The live session used Plotly. RGL-specific frame timing was not collected.
Both renderers consume the same committed `basin_tree_graph_overlay` reactive,
and server regressions cover renderer switching, but this report does not
claim a separate RGL browser timing measurement.
