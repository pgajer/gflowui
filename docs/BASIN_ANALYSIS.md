# Basin Analysis

This guide describes the scientific semantics and operator behavior of the
three-panel Basin Analysis workspace.

## Scientific source

Basin Analysis operates on the field currently selected in `Basins`:

- an occupation-density estimate; or
- a generic conditional-expectation estimate.

The graph, field vector, vertex weights, and source metadata are copied into
an immutable scientific bundle. A changed graph or field invalidates both the
current and retained analysis. Display-only controls do not mutate that
bundle.

Canonical maximum and minimum basins come from
`gflow::create.basin.complex()` with fixed CLOSEST flow and exact connected
plateau handling:

- ascending trajectories assign vertices to local maxima;
- descending trajectories assign vertices to local minima; and
- every extremum and basin retains its canonical internal identity.

The maximum-basin tree is the plateau-aware superlevel merge tree returned by
the public `gflow` API. Density-value elder-rule continuation determines which
branch survives a merge. Trajectory-flow mass and support annotate and filter
the tree; they do not alter canonical parentage.

## Compute and open

1. Open a registered project and select its graph.
2. Show the intended occupation-density or conditional-expectation field.
3. Open `Basins` and verify `Estimate source`.
4. Select `Compute & Open Basin Analysis`.

Reopening an analysis whose scientific fingerprint still matches does not
reconstruct the basin complex and does not duplicate the two default plots.

## Three linked panels

### Basin Superlevel-Set Merge Tree

The tree initially shows an adaptive proposal rather than every branch.
`Auto` uses the first strong mass gap when one satisfies the configured
threshold; otherwise it uses cumulative mass coverage. Alternative filters
include cumulative mass, minimum mass, Top K, and None.

The displayed branch set is:

1. the filter-derived core;
2. enabled peak, prominence, and support sentinels;
3. user-pinned branches; and
4. the exact ancestor closure required to preserve tree topology.

The status reports core and final counts, positive-mass coverage, render
outcome, and whether the current or retained proposal is shown. Overflow is
explicit: the app does not silently truncate core, sentinels, pins, or
closure.

`Labels` and `Important-label count` affect presentation only. A selected
hidden branch remains disclosed as hidden until it is pinned or otherwise
enters the display proposal.

### Basin Plot Workspace

The two seeded scatter plots contain every maximum basin in the selected
component:

- Extremum value rank versus Support rank; and
- Extremum value rank versus Mass rank.

Proposal membership changes point styling without dropping points. Additional
histograms, scatter plots, and metric matrices can use Support, Mass,
Extremum value, Prominence, and their ranks. Raw and log10 coordinates are
controlled per card. Duplicate plots are not added.

### Basin Inspector

The default row scope is the proposal-derived `Initial display`. Other scopes
expose Core, Sentinels, Pinned, Selected, all maximum basins, all minimum
basins, or the complete table.

M/m labels are stable canonical display labels and do not change with row
scope or sorting. Sorting is a view operation. The table reports:

- Support: number of primarily assigned vertices;
- Mass: normalized primary-support mass;
- Extremum value;
- Prominence: extremum-to-merge field difference;
- proposal membership and inclusion reasons; and
- visible/hidden, selected, and pinned state.

`Show` is transient selection. `Pin` protects a maximum basin in the proposal
and starts one new proposal attempt. Minimum rows are retained for inspection
and complete export but do not participate in the maximum-basin display
proposal.

## Linked selection and graph colors

Static-tree branches, complete-tree points, scatter points, Inspector rows,
and assigned graph vertices resolve through the same canonical maximum-basin
IDs.

Selection updates the linked views without reconstructing the proposal. Pin
and unpin are the only selection-driven reconstruction actions.

Basin Analysis preserves the current graph color source and user-selected
basin colors. Select `Show basin colors` when the graph should switch
explicitly to basin coloring. Density or conditional-expectation coloring is
not overwritten merely because an analysis is opened, selected, pinned,
cleared, recolored, or restored.

## Current, retained, and stale attempts

Proposal computation is attempt-scoped and deferred:

- a matching successful attempt becomes current;
- while a new attempt is pending or fails, the last valid proposal may remain
  visible and is identified as retained;
- all three panels report the same active and displayed attempt IDs; and
- a late completion from an older attempt or invalidated scientific context
  is rejected and cannot overwrite current or retained state.

Closing a panel changes no scientific, proposal, selection, pin, plot, or
graph-color state.

The current launcher uses `later`, so work is deferred relative to the
initiating observer but still executes on the R session event loop. It is not
an off-process compute backend. Operators should record perceptible input
latency, along with project, graph, source, branch counts, and elapsed time,
instead of assuming that deferred execution cannot block the session.

## Display recipes

Version 1 recipes contain validated display settings only:

- filter mode;
- final render budget;
- sentinel count and enabled measures;
- active mode-specific parameters;
- important-label count; and
- label mode.

Recipes never contain:

- bundle or component identity;
- canonical basin IDs;
- pins or transient selection;
- proposal results or outcomes;
- mass groups or coverage results; or
- merge-tree layout.

`Save current recipe` stores a versioned recipe in the browser and in the
current session. `Download recipe` and `Load recipe JSON` provide a portable
settings file. `Apply saved recipe` validates the settings against the active
scientific bundle, reruns automatic component selection, clears selection and
pins, and computes a new proposal attempt.

## Complete ZIP export

`Save full basin bundle` exports the unfiltered analysis, independent of
Inspector scope, sorting, selected rows, pins, and graph display filters. The
bundle includes the complete basin-characteristics CSV, internal canonical-ID
mapping, R-native tables, metadata, reconstruction fingerprint, checksums, and
a file guide.

The export index maps reconstruction fingerprints to full ZIP paths. A
reported match is accepted only after the ZIP's embedded fingerprint and
binary SHA-256 validate. The separate canonical-object disk cache is
versioned, written atomically, and invalidated when its construction
fingerprint or schema changes.

## Accessibility and narrow layouts

The General Inspector is an accessible complementary region. Its merge tree,
Plot Workspace, and Basin Inspector are named sibling regions with semantic
headings. Attempt/status changes use polite live regions. Basin row Show, Pin,
and Color controls include the stable display label in their accessible name,
and the characteristics table has a screen-reader caption.

The desktop resize separator accepts pointer/touch drag, Left/Right Arrow,
Shift+Arrow for larger steps, Home, End, and double-click reset. At narrow
widths the graph and General Inspector stack vertically and the horizontal
resize separator is hidden.

Tree plots can scroll horizontally when branch or label density requires more
width. All-label mode reports a crowding warning. Long labels, zero/one-branch
components, empty row scopes, retained proposals, and overflow states must
show explicit content rather than an unexplained blank panel.

## Performance expectations

The app measures proposal construction, filtered layout, static rendering, and
complete-tree preparation separately. These measurements are diagnostic
timings, not latency guarantees. Branch count, final display count, filter
outcome, render outcome, platform, `gflowui` commit, and `gflow` dependency
must accompany release measurements.

The default filtered Subject 15 view is expected to remain substantially
smaller than its 352-branch complete maximum tree. The complete interactive
tree remains available without mutating the filtered proposal. Any
perceptible event-loop blocking is a release observation to report, not a
reason to silently change the async architecture.

## Troubleshooting

### Compute is blocked

Confirm that a graph-backed estimate is selected and that its field length,
vertex weights, and graph identity match. The status message distinguishes
scientific input failures from proposal-control failures.

### A branch is selected but absent from the static tree

It is a hidden transient selection. Open the complete interactive tree or Pin
the basin if it must enter the proposal. Pinning can expand ancestor closure
and can produce an explicit overflow state.

### Basin colors are not visible

Selection intentionally preserves the current graph color source. Select
`Show basin colors`. To return to the density or conditional-expectation
field, use that section's show action or the graph `Color by` selector.

### A previous proposal remains visible

Read the linked status in any of the three panels. It identifies a retained
proposal while the active attempt is pending, blocked, or failed. A stale
completion is never installed.

### A recipe is rejected

Only version 1 settings-only recipes with valid parameter domains are
accepted. Recipes are revalidated against the current scientific bundle and
never restore old basin IDs or results.

## Maintainer reference

See [BASIN_ANALYSIS_DEVELOPER.md](BASIN_ANALYSIS_DEVELOPER.md) for package
ownership, state installation, performance telemetry, release QA, and
generated-file hygiene.
