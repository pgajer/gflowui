# Adaptive Initial Filtering for Basin Merge Trees

## Status

Revision 2, prepared after the 2026-07-31 specification audit.

This document specifies a deterministic initial display policy for maximum
basin merge trees in `gflowui`. It does not alter basin construction, basin
identity, merge topology, density estimation, or scientific selection. The
complete canonical merge tree remains authoritative and available.

Version 1 is deliberately limited to occupation-density superlevel trees with
`direction = "max"`, one graph component at a time. Minima and sublevel-tree
defaults are deferred.

## Motivation

A complete merge tree can be mathematically valid and visually unusable. The
Subject 15 selected density has 352 maximum basins and 351 merge events. Its
complete static rendering has severe branch, label, annotation, and barcode
compression, while a 17-branch view is readable.

The initial display should therefore be proposed from declared basin measures
while preserving:

1. the complete tree in memory;
2. stable canonical branch and merge identities;
3. explicit disclosure of hidden branches and overflow;
4. access to a complete interactive view;
5. protection for important low-mass branches; and
6. exact coordination with the Inspector, pair plots, and graph selection.

Automatic filtering is a display proposal, not an EOD decision or a claim that
hidden branches are spurious.

## Ownership and Invariants

### Package ownership

- `gflow` owns canonical merge-tree topology, component membership, ancestry,
  branch/event selection, ancestor closure, and crossing-free layout.
- `gflowui` owns adaptive proposal policy, controls, disclosure, linked
  selection, and construction-scoped session state.
- The scientific project owns Subject 15 evidence and EOD interpretation.

`gflowui` must not call a private `gflow` layout helper, copy tree-layout logic,
construct an induced graph, or recompute a basin complex for display filtering.

### Scientific and display invariants

1. `gflow::get.basin.merge.tree()` supplies the complete canonical tree.
2. Every eligible source field has exactly one finite value per graph vertex.
3. Filtering changes only initial presentation.
4. Births, deaths, parents, elder-rule survival, prominence, assignments,
   mass, support, and graph alignment remain unchanged.
5. A displayed branch subset is canonical-ID based and ancestor-closed.
6. Branch filtering and label filtering are separate.
7. Every panel validates the same active graph, vertex, field, source,
   trajectory-flow construction, canonical tree, direction, and component.
8. No ranking measure is inferred from a plotting default.

## Version 1 Measure Contract

The coordinated `gflowui` view intentionally combines declared measures from
two constructions. Their names and owners must remain visible.

| Purpose | Exact measure | Owning object |
|---|---|---|
| Automatic mass core and mass labels | `primary.support.mass` | trajectory-flow basin complex |
| Support sentinel and support labels | `primary.support.size` | trajectory-flow basin complex |
| Peak sentinel and peak labels | selected field value at the extremum vertex, descending for maxima | selected construction field |
| Prominence sentinel and labels | persistence/prominence | canonical superlevel merge tree |
| Birth, death, parent, survivor, events, layout | canonical tree values | canonical superlevel merge tree |

Tree-native support mass, when shown, must be labeled
`merge-tree primary support mass`. It must not be called simply `Mass`, and it
must not replace trajectory-flow mass in the version 1 proposal.

### Required mapping

For the selected direction and component, `gflowui` maps each trajectory-flow
basin to one canonical merge-tree branch by:

```text
(direction, extremum.vertex)
```

The mapping result is translated to the canonical merge-tree basin ID before
selection or plotting. The proposal is blocked with `mapping_invalid` when:

- a trajectory-flow basin has no canonical branch;
- a canonical branch has no trajectory-flow basin;
- either side maps more than once;
- direction or component differs; or
- the extrema disagree after graph-vertex identity validation.

No measure substitution or partial mapping is permitted. The proposal record
stores both construction identities, the mapping cardinality, the mapping
validation result, and all measure names.

## Direction and Component Scope

Version 1 applies only to `direction = "max"`.

Every proposal is scoped to one `(direction, graph component)`. Mass ranking,
sentinels, ancestor closure, coverage, labels, and budgets are computed within
that component. Its elder-rule survivor is mandatory.

The UI reports:

- the total maximum-basin count across the direction;
- the number of graph components;
- the selected component ID; and
- the maximum-basin count in that component.

When several components exist, the component selector is visible. The initial
component is the component with the greatest validated trajectory-flow
positive mass; ties use the stable canonical component ID. Changing direction
or component invalidates the proposal. A user may instead choose a component
explicitly.

## Source and Mass Validation

Source validation precedes adaptive filtering:

1. the source field must have one finite value per graph vertex;
2. graph, vertex, field, and construction fingerprints must match; and
3. the complete canonical tree must already be valid.

A missing or nonfinite source-field value produces `source_invalid`. Filtering
must not drop vertices or construct a partial graph.

The declared trajectory-flow mass vector is then validated for the selected
component:

- missing, negative, or nonfinite mass produces `mass_invalid`;
- exact zero is valid, excluded from logarithms, and counted separately;
- zero-mass branches remain eligible for Show All, sentinels, and ancestry;
- zero total positive mass produces `mass_unavailable`;
- normalization occurs only after the complete declared vector validates.

`mass_invalid` and `mass_unavailable` disable Auto, Cumulative Mass, and
Minimum Mass. A valid canonical tree may still use None/Show All.

Both mass-core coverage and final displayed-set coverage use the same declared
positive-mass denominator. The denominator, positive count, and exact-zero
count are recorded; it is never silently changed.

## Diagnostic

The selected component exposes:

1. a histogram of finite positive `log10(trajectory-flow mass)`;
2. a ranked positive log-mass curve with eligible boundaries marked; and
3. cumulative positive mass versus complete tie-group rank.

The histogram uses Freedman-Diaconis bins with a deterministic fallback for
constant or fewer than four positive values. Exact zeros are reported beside
the histogram and never placed on the log scale. The histogram is descriptive
and never selects the boundary.

## Tie Groups

Positive masses are sorted in descending order. Exact equality of the stored
validated numeric values defines an indivisible tie group; no tolerance-based
grouping is introduced. Tie groups are ordered by mass, and canonical branch
IDs are ordered lexicographically within each group.

All candidate boundaries occur after complete tie groups. The proposal record
stores the ordered groups, their exact mass values, member IDs, endpoints, and
cumulative coverage. Row order cannot affect the result.

The positive-mass denominator and cumulative sums are evaluated in this same
descending group order, with canonical-ID order inside groups. This fixes the
floating-point accumulation order and makes the recorded coverage invariant to
input row permutation.

## Automatic Mass-Core Algorithm

### Configurable defaults

```text
coverage.target       = 0.99
strong.gap.decades    = 3
minimum.core.branches = 3
core.branch.budget    = 50
final.render.budget   = 80
sentinel.top.n        = 10
important.label.n     = 6
```

These are display defaults, not scientific thresholds. They are recorded in
every proposal.

Let positive masses be grouped in descending order. At complete tie-group
endpoints define:

```text
C_j = positive mass through endpoint j / total positive mass
g_j = log10(mass at j) - log10(mass in the next positive group).
```

There is no gap after the final positive group.

### Bounded rule

1. Let `j.coverage` be the first tie-group endpoint with
   `C_j >= coverage.target`.
2. Let `j.minimum` be the first tie-group endpoint containing at least
   `min(minimum.core.branches, number of positive branches)` branches.
3. Let `j.required = max(j.coverage, j.minimum)`.
4. If there is one positive branch, select its complete tie group, return
   `single_positive`, and do not fabricate a gap.
5. If `j.required` exceeds `core.branch.budget`, do not search for a gap. If
   the tie group containing the budget boundary straddles that boundary,
   include the complete group and return `tie_overflow`; otherwise select the
   last complete tie-group endpoint not exceeding the budget and return
   `coverage_capped`.
6. Never split equal masses. When the complete budget-straddling group still
   misses the coverage target, add the `coverage_capped` warning to
   `tie_overflow`.
7. When `j.required` is within budget, inspect complete tie-group boundaries
   from `j.required`
   forward, stopping at `core.branch.budget`. Select the earliest boundary
   with `g_j >= strong.gap.decades` and return `strong_gap`.
8. If no eligible strong gap exists, select the prefix through `j.required`
   and return `coverage`.

The rule never accepts a gap boundary beyond the core budget. It therefore
cannot be driven to a negligible terminal gap after coverage has already been
achieved.

For `strong_gap`, the geometric midpoint

```text
sqrt(mass_at_boundary * next_positive_mass)
```

may be displayed as information. Membership is defined only by the recorded
tie-group boundary and canonical branch IDs.

### Other filter modes

- Cumulative Mass uses `j.coverage` and the same tie/budget statuses.
- Minimum Mass includes every complete tie group meeting the threshold.
- Top K includes the complete tie group containing rank K and reports
  `tie_overflow` when K is straddled.
- None/Show All uses every branch in the selected component.

No mode splits a mass tie.

## Mandatory Sentinels

Within the selected component, the mandatory union contains:

1. the mass core;
2. selected or pinned branches;
3. the component elder-rule survivor;
4. the top `sentinel.top.n` branches by descending maximum peak value;
5. the top `sentinel.top.n` branches by canonical prominence; and
6. the top `sentinel.top.n` branches by trajectory-flow support.

Sentinel Top-N boundaries include complete ties. Each branch records every
inclusion reason. For non-overlapping displayed counts, an added branch receives
the first applicable primary reason in this precedence:

```text
selected_or_pinned, component_survivor, peak, prominence, support
```

The UI reports core count and sentinel-only counts by primary reason. A
sentinel can still expose all of its reasons in details.

Canonical ancestor closure is then obtained from `gflow`. Closure-only branches
are reported separately.

## Final Rendering Budget and Overflow

The core budget limits mass selection. The separate final rendering budget
governs whether the mandatory ancestor-closed union is suitable for the
initial static tree.

Mandatory branches are never silently discarded.

- If a complete core tie group alone exceeds the final budget, return
  `tie_overflow`.
- If the pre-closure sentinel union exceeds it, return `sentinel_overflow`.
- If ancestor closure causes the excess, return `closure_overflow`.

In any overflow state, the panel initially shows the diagnostic, exact counts,
coverage, warnings, and a concise overflow explanation instead of compressing
hundreds of branches into a static tree. `Open complete interactive tree`
opens the canonical component in a zoomable, scrollable view with important
labels only by default. Show All remains available. The Plot Workspace still
uses all component basins.

The proposal stores and reports these non-overlapping counts:

```text
mass core
selected/pinned-only
survivor-only
peak-only
prominence-only
support-only
ancestor-only
final union
```

It also reports core coverage and final-union coverage against the same
positive-mass denominator.

## Label Policy

Showing a branch does not imply showing its text label.

For a renderable initial tree, Important labels are the union of:

- top `important.label.n` displayed branches by trajectory-flow mass;
- top `important.label.n` by maximum peak value;
- top `important.label.n` by canonical prominence;
- top `important.label.n` by trajectory-flow support;
- the component survivor; and
- selected or pinned branches.

All Top-N boundaries include complete ties. Label modes are Important,
Selected, Displayed, None, and All. All is permitted with an explicit crowding
warning. A label ID must be in the final displayed branch set.

## Required Public `gflow` Layout Contract

Implementation requires a reviewed public pure accessor with behavior
equivalent to:

```r
layout.basin.merge.tree(
  x,
  direction = "max",
  component,
  basin.ids = NULL,
  close.ancestors = FALSE
)
```

Here `x` is a complete canonical `basin.merge.tree`. `basin.ids = NULL`
selects the complete component. The accessor returns, without drawing:

- schema and canonical tree fingerprint;
- direction and component;
- requested canonical basin IDs;
- closure-added IDs;
- exact selected branch table;
- exact selected merge-event table;
- component root/survivor ID;
- crossing-free leaf order and branch/event coordinates; and
- validation status.

It rejects unknown IDs, mixed directions, mixed components, missing roots, and
non-closed selections. With `close.ancestors = TRUE`, it adds and reports
canonical ancestors instead of rejecting only the non-closure.

`plot.basin.merge.tree()` must consume this same accessor and expose matching
`basin.ids` and `close.ancestors` arguments. Static and interactive renderers
must therefore use identical selected branches, events, closure, and
coordinates.

## Versioned Proposal Record

Every proposal is serializable as
`gflowui_basin_merge_tree_display_proposal/1` and contains:

- algorithm name and version;
- creation time in ISO 8601;
- graph, topology, vertex, field, estimate, source, trajectory-flow
  construction, and canonical-tree fingerprints;
- direction and selected component;
- whole-direction and component basin counts;
- exact measure names and owning construction identities;
- source and mapping validation results;
- stable ordered mass tie groups;
- all parameter values;
- core status, warnings, boundary, gap, and informational cutoff;
- core canonical IDs;
- sentinel IDs, all inclusion reasons, and primary reasons;
- ancestor-only additions;
- final canonical IDs and label IDs;
- positive denominator, zero count, core coverage, and final coverage;
- non-overlapping category counts; and
- final render status.

Every coordinated panel validates this identity against its active graph,
field, source, constructions, direction, and component before use. A mismatch
produces `stale`; cached IDs are not rendered.

Adjusted settings persist only within the active session and construction
identity. They reset when graph, field, source, subject, project,
construction, direction, or component changes. Cross-context reuse requires
an explicit future opt-in mechanism and is outside version 1.

## User Interface

The initial panel order is:

1. Basin Superlevel-Set Merge Tree;
2. Basin Plot Workspace;
3. Basin Inspector.

An ordinary status line has this form:

```text
352 maxima across 1 component; component 1 has 352.
17 mass-core branches; 17 final branches.
Auto: earliest >=3-decade gap after 99% coverage, at rank 17.
Core/final mass coverage: 0.999999999999917 / 0.999999999999917.
Sentinel-only: selected 0, survivor 0, peak 0, prominence 0, support 0;
ancestor-only 0.
Mass: trajectory-flow primary support mass.
```

Controls:

- Component;
- Filter: Auto / Cumulative Mass / Minimum Mass / Top K / None;
- Mass coverage;
- Strong-gap threshold;
- Core branch budget;
- Final render budget;
- peak, prominence, and support sentinel toggles/counts;
- Labels: Important / Selected / Displayed / None / All;
- Show diagnostic;
- Open complete interactive tree; and
- Show all.

The Plot Workspace initially shows, on its existing default log10 rank scales:

- Extremum-value rank versus support rank; and
- Extremum-value rank versus mass rank.

It uses all maximum basins in the selected component, not only tree-displayed
branches. Clicking a branch, barcode, or pair-plot point selects the same
canonical basin in every panel and on the graph without reconstruction.
Automatic proposals do not alter Inspector Top-K or ranking controls.

## Subject 15 Evidence and Portable Fixture

The version 1 mass measure is trajectory-flow `primary.support.mass`. For the
Brier-selected Subject 15 occupation density on the symmetric `k=3` graph:

- maximum basins: 352;
- positive-mass denominator: `1.0000000000000087`;
- rank-17 mass: `0.0122134243817115`;
- rank-18 mass: `1.40305377913392e-15`;
- gap after rank 17: `12.9397631299771` decades;
- geometric midpoint: `4.13957621441213e-09`;
- rank-1:17 raw mass sum: `0.99999999999992595`;
- rank-1:17 normalized coverage: `0.99999999999991729`;
- rank-18:352 mass: `8.72873121029731e-14`.

The canonical merge-tree primary-support mass is materially different
(maximum absolute branch difference `0.158873`; Spearman rank correlation
`0.797341`) even though it also places this field's large gap after rank 17.
This agreement does not make the measures interchangeable.

A numerical-floor check on the same selected field also retains 17 branches.
It is corroborating same-field numerical-scale evidence, not independent
scientific validation and not a basis for universal defaults.

The clean-checkout fixture is:

- `tests/testthat/fixtures/basin_merge_tree_subject15_maxima.csv`
- `tests/testthat/fixtures/basin_merge_tree_subject15_maxima_provenance.csv`

It contains all 352 maxima, trajectory and canonical IDs, canonical parents,
component survivor, trajectory-flow mass/support, peak value, and canonical
prominence. Provenance pins:

- upstream revision
  `4615555547f3f406e79436c308d28fd78985b64e`;
- source ZIP SHA-256
  `15d575fea00267de49b12192060aeecdd373df6edfdea52cd250d68d2202c275`;
- topology RDS SHA-256
  `afb7863d761932e31f4f1816f95b496db16fc58028663f26cb036ec6aa1af000`;
- graph, field, trajectory construction, and canonical analysis fingerprints;
  and
- the exact four-measure contract.

The fixture is reproducible with:

```sh
Rscript dev/fixtures/derive_subject15_basin_merge_tree_adaptive_fixture.R
```

Full 352-branch rendering and source-value comparisons remain optional
integration checks when the complete upstream assets are available.

## Typed Status Summary

Core statuses:

```text
strong_gap
coverage
single_positive
coverage_capped
tie_overflow
mass_invalid
mass_unavailable
mapping_invalid
source_invalid
stale
```

Final rendering adds:

```text
renderable
sentinel_overflow
closure_overflow
tie_overflow
```

Warnings are additive, so a tie overflow may also report coverage capped.

## Required Validation

### Measures and identity

1. Exact measure names and owning identities are recorded and displayed.
2. One-to-one extrema mapping rejects missing, duplicate, mixed-direction, and
   mixed-component cases.
3. A fixture with different trajectory-flow and merge-tree mass rankings uses
   only the declared trajectory-flow mass.
4. Proposal serialization round-trips without changing ordered IDs or groups.
5. Stale graph, field, source, construction, direction, or component identity
   is rejected by every coordinated panel.

### Algorithm

6. Results are deterministic under row permutation.
7. Tests cover a strong gap, several comparable gaps, smooth heavy tail,
   extreme last-value gap, all-equal masses, coverage-boundary ties, and
   budget-boundary ties.
8. The extreme late-gap example selects a bounded core, never rank 199.
9. Tests cover negative, missing, nonfinite, all-zero, one-positive, and
   two-positive mass vectors with exact typed statuses.
10. Exact zeros never enter log calculations and remain available to Show All,
    sentinels, and closure.
11. Core and final coverage use the same recorded denominator.

### Topology and components

12. Complete-tree identity is unchanged by every filter.
13. Unknown, mixed, or nonclosed selections fail in the public `gflow`
    accessor unless ancestor closure is explicitly requested.
14. Filtered public layouts preserve exact complete-tree births, deaths,
    parents, events, survivor, and coordinates for the same selected IDs.
15. Multiple components have per-component roots, proposals, counts, and
    invalidation.
16. Every final branch exists in the canonical tree; every label refers to a
    final branch.

### Overflow and interaction

17. Sentinel reasons and non-overlapping counts are exact.
18. Deep, disjoint sentinel ancestry exercises `sentinel_overflow` and
    `closure_overflow`.
19. Mandatory branches are never silently discarded.
20. Desktop and narrow viewports remain usable in ordinary and overflow
    states.
21. Cross-panel selection highlights one canonical basin without
    reconstruction or Inspector-setting changes.

### Portable Subject 15 regression

22. A clean checkout validates all 352 fixture mappings and canonical parents.
23. The bounded algorithm returns the rank-17 strong-gap core and exact
    recorded coverage.
24. Show All exposes all 352 fixture branches when the full canonical object
    is available.

## Implementation Order

1. Add and review the public filtered-layout contract in `gflow`.
2. Implement the pure proposal/validation/serialization helpers in `gflowui`.
3. Add synthetic edge-case and portable Subject 15 tests.
4. Build the merge-tree panel and overflow presentation.
5. Wire linked selection and construction-scoped settings.
6. Run desktop and narrow-viewport visual QA.

Implementation acceptance does not imply scientific acceptance of adaptive
filtering or EOD interpretation.
