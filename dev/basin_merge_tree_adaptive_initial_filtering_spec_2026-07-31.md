# Adaptive Initial Filtering for Basin Merge Trees

## Status

Proposed Revision 9, prepared on 2026-08-01 after the 2026-07-31
specification audit series and updated in response to the fresh Revision 9
architecture audit.

Revision 9 supersedes Revision 8's portable, self-authenticating proposal and
view-state architecture for version 1. The earlier design remains available in
Git history and its audit records, but its wire schemas, proposal
deserialization, proposal/view fingerprints, and signed-64 wire-integer
contract are not part of this specification.

This document specifies a deterministic initial display policy for maximum
basin merge trees in `gflowui`. It does not alter basin construction, basin
identity, merge topology, density estimation, or scientific selection. The
complete canonical merge tree remains authoritative and available.

Version 1 is limited to occupation-density superlevel trees with
`direction = "max"`, one graph component at a time. Minima and sublevel-tree
defaults are deferred.

This is a proposed implementation contract. It does not claim implementation,
auditor acceptance, or scientific acceptance of adaptive filtering or EOD
interpretation.

## Version 1 Trust and Lifetime Model

Basin display proposals are created only by trusted `gflowui` constructors and
remain internal to the active application session. Version 1 does not
deserialize externally supplied or previously persisted proposals.

Saved settings are revalidated and proposals are recomputed against the
currently resolved graph, source field, trajectory-flow construction, and
canonical merge tree. A proposal is never restored as executable state.

The version 1 trust boundary is therefore:

1. validate scientific inputs, active context, and active settings;
2. construct one minimal authoritative runtime proposal;
3. enforce constructor postconditions before installation;
4. derive display summaries from canonical inputs and proposal IDs; and
5. discard and recompute the proposal whenever its context changes.

This trust model removes the need for a second implementation that
deserializes and revalidates every derived field produced by the first one. It
does not weaken validation of graph data, mappings, ranking vectors, topology,
settings, or constructor results.

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
construct an induced graph, or recompute a basin complex for display
filtering.

### Canonical merge-survivor rule

The canonical tree uses the **filtration-value elder rule**, specialized here
as the **density-value elder rule**. At a superlevel-set merge, the branch with
the larger birth-density value survives because it was born earlier in the
descending filtration. Equal birth-density values use the deterministic
`gflow` tie-break: the branch with the smaller canonical extremum-vertex index
survives.

This density-value rule alone determines canonical branch continuation,
parentage, and survivor identity in version 1. Trajectory-flow mass and support
rank branches for display but do not alter canonical merge survival. A future
mass-priority or support-priority rule would be a separately named
merge-survivor policy with its own measurement and tie contract.

### Scientific and display invariants

1. `gflow::get.basin.merge.tree()` supplies the complete canonical tree.
2. Every eligible source field has exactly one finite value per graph vertex.
3. Filtering changes only initial presentation.
4. Births, deaths, parents, survival under the density-value elder rule,
   prominence, assignments, mass, support, and graph alignment remain
   unchanged.
5. A displayed branch subset is canonical-ID based and ancestor-closed.
6. Branch filtering and label filtering are separate.
7. Every panel uses the same active graph, vertex map, field, source,
   trajectory-flow construction, canonical tree, direction, and component.
8. No ranking measure is inferred from a plotting default.
9. Context changes clear active and retained proposals before recomputation.

## Version 1 Measure Contract

The coordinated `gflowui` view combines declared measures from two
constructions. Their names and owners must remain visible.

| Purpose | Exact measure | Owning object |
|---|---|---|
| Automatic mass core and mass labels | `primary.support.mass` | trajectory-flow basin complex |
| Support sentinel and support labels | `primary.support.size` | trajectory-flow basin complex |
| Peak sentinel and peak labels | selected field value at the extremum vertex, descending for maxima | selected construction field |
| Prominence sentinel and labels | persistence/prominence | canonical superlevel merge tree |
| Birth, death, parent, survivor, events, layout | canonical tree values | canonical superlevel merge tree |

After canonical mapping, every ranking vector has one value per branch in the
complete selected direction and satisfies:

- trajectory-flow `primary.support.mass`: finite and nonnegative;
- trajectory-flow `primary.support.size`: finite, nonnegative whole numbers;
- selected-field peak value: finite; and
- canonical persistence/prominence: finite and nonnegative.

No missing value, implicit omission, coercion, rounding, or replacement is
permitted. Exact zero support and zero prominence are valid.

For a non-surviving maximum branch, canonical prominence is
`birth.level - death.level`. For the component survivor, death is the
component minimum of the selected field and prominence is
`birth.level - component.minimum`. Thus the survivor also has finite,
nonnegative prominence; infinity and `NA` are not survivor conventions.

Tree-native support mass, when shown, is labeled
`merge-tree primary support mass`. It does not replace trajectory-flow mass in
the version 1 proposal.

## Required Mapping

Across the complete selected direction, before automatic component selection,
`gflowui` maps each trajectory-flow basin to one canonical merge-tree branch
by:

```text
(direction, extremum.vertex)
```

The mapping result is translated to the canonical merge-tree basin ID before
selection or plotting. Proposal construction is blocked with
`mapping_invalid` when:

- a trajectory-flow basin has no canonical branch;
- a canonical branch has no trajectory-flow basin;
- either side maps more than once;
- direction or component differs; or
- extrema disagree after graph-vertex identity validation.

No measure substitution or partial mapping is permitted. The trusted
constructor receives both construction identities, the complete mapped ID
universe, and the four declared ranking vectors.

## Runtime Scientific Bundle and Context Identity

### Immutable session bundle

Every construction-scoped session context owns one validated
`runtime.scientific.bundle`. It is an immutable session snapshot containing:

- graph topology and graph-vertex map;
- selected field/source values and estimate identity;
- complete trajectory-flow-to-canonical mapping;
- complete trajectory-flow mass and support vectors;
- complete canonical tree, including peak and prominence values;
- direction; and
- all whole-direction inputs used for automatic component selection.

The bundle receives an opaque, session-unique `bundle.id` when it is created.
The token is an identity/version token, not a content digest or portable
fingerprint.

After installation, bundle-owned objects are not mutated in place. Any change
to a covered object or value creates a replacement bundle with a new
`bundle.id`, even when human-readable project, subject, source, estimate, and
construction names are unchanged. Implementation may enforce this rule with
validated private copies or a locked/private replacement-only container.
Merely retaining an R reference to caller-owned mutable data is insufficient.

All proposal derivations receive the bundle explicitly and require exact
identity equality between `proposal.bundle.id` and `bundle.id`. A mismatch is
`stale`; no diagnostic, coverage, label, layout, or retained proposal may be
derived from mismatched objects.

### Context key and generation

Each construction-scoped session context has an internal `context.key`
covering:

- project and subject identity;
- required `bundle.id`;
- graph identity;
- topology and vertex-map identity;
- selected field and source identity;
- estimate identity;
- trajectory-flow construction identity;
- canonical-tree construction identity;
- direction; and
- selected component.

Existing graph/source/construction fingerprints may help build the bundle, but
they do not replace the required `bundle.id`. The context key exists only to
detect stale or changed runtime context. It is not a portable proposal digest
and does not authenticate proposal content.

The session maintains a monotonically increasing `context.generation`.
Replacing the bundle or changing any other context identity:

1. increments the generation;
2. clears active and retained proposals;
3. clears transient selection and context-scoped pins;
4. invalidates pending asynchronous work; and
5. requires complete validation and recomputation from the replacement bundle.

## Direction and Component Scope

Version 1 applies only to `direction = "max"`.

Every proposal is scoped to one `(direction, graph component)`. Mass ranking,
sentinels, ancestor closure, coverage, labels, and budgets are computed within
that component. Its density-value-elder-rule survivor is mandatory.

The UI reports:

- total maximum-basin count across the direction;
- number of graph components;
- selected component ID; and
- maximum-basin count in that component.

Component selection follows a whole-direction pre-pass:

1. enumerate components in stable canonical component-ID order;
2. validate the whole-direction trajectory-flow-to-canonical mapping;
3. validate every declared ranking vector across all maximum branches;
4. for each component, sum validated positive masses in ascending canonical
   branch-ID order using ordinary binary64 addition;
5. when mass is valid and at least one component has positive mass, compare
   those fixed-order binary64 totals exactly and select the greatest, breaking
   an exact tie by stable component ID;
6. when all mass is exactly zero, select the smallest stable component ID and
   record `smallest_component_mass_unavailable`;
7. when mapping is invalid, select the smallest stable component ID only for
   deterministic error presentation and expose `mapping_invalid`; and
8. when mass is missing, negative, or nonfinite, select the smallest stable
   component ID, record `smallest_component_mass_invalid`, and disable
   mass-based modes.

The user may explicitly select another valid component. Component switching
creates a new runtime context and recomputes the proposal.

## Source and Ranking Validation

Source validation precedes adaptive filtering:

1. the source field has one finite value per graph vertex;
2. graph, vertex, field, source, and construction identities match the active
   context; and
3. the complete canonical tree is valid.

A missing or nonfinite source-field value produces `source_invalid`.
Filtering never drops vertices or constructs a partial graph.

After one-to-one mapping, all four ranking vectors are validated across the
whole maximum direction before component selection. The validation statuses
are:

```text
trajectory_flow_mass:
  valid | mass_invalid | mass_unavailable
trajectory_flow_support:
  valid | support_invalid
source_peak:
  valid | peak_invalid
canonical_prominence:
  valid | prominence_invalid
```

Mass rules:

- missing, negative, or nonfinite mass produces `mass_invalid`;
- exact zero is valid and excluded from logarithms;
- zero-mass branches remain eligible for Filter None, sentinels, and ancestry;
- no positive mass produces `mass_unavailable`; and
- normalization occurs only after the complete vector validates.

`mass_invalid` and `mass_unavailable` disable Auto, Cumulative Mass, Minimum
Mass, and mass-ranked Top K. Filter None remains available when mass alone is
invalid or unavailable. In that state, mass diagnostics, coverage, mass-rank
plots, and mass-label contributions are unavailable with the corresponding
typed reason.

Missing, nonfinite, negative, or fractional support produces
`support_invalid`; zero is valid. Missing or nonfinite mapped peak produces
`peak_invalid` and `source_invalid`. Missing, nonfinite, or negative canonical
prominence produces `prominence_invalid`.

Invalid support, peak, or prominence blocks every filter mode because the
sentinel, label, and Plot Workspace contracts require those vectors.

## Parameter Validation

The proposal constructor and presentation controls use ordinary R-compatible
values:

| Parameter | Valid domain |
|---|---|
| `filter.mode` | `auto`, `cumulative_mass`, `minimum_mass`, `top_k`, or `none` |
| `coverage.target` | finite scalar with `0 < value <= 1` |
| `strong.gap.decades` | finite nonnegative scalar |
| `core.branch.budget` | positive whole-number scalar not less than the fixed `minimum.core.branches` policy |
| `final.render.budget` | positive whole-number scalar |
| `sentinel.top.n` | nonnegative whole-number scalar |
| `important.label.n` | nonnegative whole-number scalar |
| `top.k` | positive whole-number scalar not exceeding selected-component branch count |
| `minimum.mass` | finite nonnegative scalar in raw trajectory-flow mass units |
| sentinel toggles | nonmissing logical scalars |
| `label.mode` | `important`, `selected`, `displayed`, `none`, or `all` |

Every whole-number control must also satisfy:

```text
value <= .Machine$integer.max
```

It is converted to an R integer only after validation. Values outside the
supported range are rejected; they are not rounded, clamped, or represented
by a custom integer carrier.

Validation is mode-aware:

- Auto validates coverage, gap, and core-budget settings;
- Cumulative Mass validates coverage and core-budget settings;
- Minimum Mass validates `minimum.mass`;
- Top K validates `top.k`;
- None has no mode-specific mass-selection setting; and
- common final-budget, sentinel, and toggle settings are always
  validated.

Inactive mode-specific values remain ordinary construction-scoped UI state.
They are not validated and cannot block the active mode. Later activation
validates the retained value before recomputation.

Invalid active proposal settings return `settings_invalid` with field-specific
messages. The UI may continue showing the last valid same-context proposal,
clearly labeled as retained, until a valid recomputation succeeds. Label mode
and Important-label count are validated independently as presentation state;
an invalid presentation value keeps the last valid presentation setting and
does not invalidate or reconstruct the proposal.

### Accepted parameter projection

The runtime proposal stores only validated settings that affected branch
membership or overflow construction:

| Mode | Mode-specific accepted settings |
|---|---|
| Auto | `coverage.target`, `strong.gap.decades`, `core.branch.budget` |
| Cumulative Mass | `coverage.target`, `core.branch.budget` |
| Minimum Mass | `minimum.mass` |
| Top K | `top.k` |
| None | none |

Every mode also stores the common final budget, sentinel settings and toggles,
and `filter.mode`. Raw or invalid inactive controls never enter the proposal.
Label settings remain validated presentation state and recipe settings; they
are not proposal-construction parameters.

## Tie Groups and Diagnostics

Two deterministic groupings are computed from the validated component mass
vector:

1. positive-mass groups drive Auto, Cumulative Mass, logarithms, gaps, and
   normalized coverage; and
2. all-mass groups drive Top K and Minimum Mass and include the complete
   zero-mass group.

Exact equality of validated numeric mass defines an indivisible group. Groups
are ordered by descending mass and canonical IDs lexicographically within a
group. No selection splits a group.

The positive denominator and cumulative sums use fixed descending-group and
lexicographic-ID order. Core and final coverage use the same denominator.

Group tables, endpoints, denominator, counts, and coverage are derived from
the canonical mass vector and proposal IDs. They are not duplicated as
authoritative proposal fields. The UI may cache them for rendering, but such
caches are disposable and recomputable.

Diagnostics expose:

1. a histogram of finite positive `log10(trajectory-flow mass)`;
2. a ranked positive log-mass curve with eligible boundaries; and
3. cumulative positive mass versus complete tie-group rank.

Exact zeros are reported separately and never placed on a log scale.

## Automatic Mass-Core Algorithm

### Defaults

```text
coverage.target       = 0.99
strong.gap.decades    = 3
minimum.core.branches = 3
core.branch.budget    = 50
final.render.budget   = 80
sentinel.top.n        = 10
important.label.n     = 6
top.k.initial         = min(10, selected component branch count)
minimum.mass.initial  = 0 raw trajectory-flow mass
```

The adjustable values are display defaults, not scientific thresholds.

`minimum.core.branches = 3` is a fixed version 1 policy constant, not a user
control or persisted recipe value. It is a presentation policy guard rather
than a scientific threshold, and changing it requires a future specification
revision. `important.label.n = 6` is the initial value of the user-adjustable
Important-label count.

At each complete positive tie-group endpoint define:

```text
C_j = positive mass through endpoint j / total positive mass
g_j = log10(mass at j) - log10(mass in the next positive group)
```

There is no gap after the final positive group.

### Bounded rule

1. `j.coverage` is the first endpoint with
   `C_j >= coverage.target`.
2. `j.minimum` is the first endpoint containing at least
   `min(minimum.core.branches, number of positive branches)` branches.
3. `j.required = max(j.coverage, j.minimum)`.
4. One positive branch selects its complete group and returns
   `single_positive`.
5. If `j.required` exceeds `core.branch.budget`, no gap is searched. Include
   a complete budget-straddling tie group with warning `tie_overflow`; return
   `coverage` if it reaches target and `coverage_capped` otherwise. Without a
   straddling tie, select the last complete endpoint within budget and return
   `coverage_capped`.
6. If `j.required` is within budget, inspect complete boundaries from
   `j.required` through the core budget. Select the earliest with
   `g_j >= strong.gap.decades` and return `strong_gap`.
7. If no eligible strong gap exists, select through `j.required` and return
   `coverage`.

`tie_overflow` is an additive warning, not a core outcome. A strong-gap
geometric midpoint may be displayed as information, but membership is defined
only by the selected tie-group boundary and canonical IDs.

### Other modes

- **Minimum Mass:** compare the threshold with raw trajectory-flow mass and
  select complete all-mass groups at or above it. No match returns
  `threshold_empty`; threshold zero includes the zero group.
- **Top K:** include the complete all-mass group containing rank K. A boundary
  tie adds `tie_overflow`.
- **Filter None:** select every canonical branch and return `complete`.

Auto and Cumulative Mass use `core.branch.budget`. Minimum Mass, Top K, and
None preserve their explicitly requested core. Every mode is evaluated
against `final.render.budget`.

### Cumulative Mass budget rule

For Cumulative Mass, let `j.coverage` be the first complete positive tie-group
endpoint reaching `coverage.target`.

1. If `j.coverage <= core.branch.budget`, select through `j.coverage` and
   return `coverage`. Equality with the budget is within budget.
2. Otherwise, identify the unique positive tie group, if any, whose first rank
   is at or below the budget and whose endpoint is above it.
3. If such a budget-straddling group exists, include the complete group and add
   `tie_overflow`. Return `coverage` when its endpoint reaches the target and
   `coverage_capped` otherwise.
4. If no group straddles the budget, select the last complete endpoint within
   budget and return `coverage_capped`.
5. A positive budget always either contains a complete endpoint or intersects
   the first positive tie group. Therefore the rule cannot produce an empty
   core after mass validation.

The rule stops at the selected capped or straddling endpoint; it does not scan
past the core budget for a later group. Complete ties are never split.

## Mandatory Sentinels and Closure

Within the selected component, the mandatory pre-closure union contains:

1. mass core;
2. pinned branches;
3. component survivor;
4. top `sentinel.top.n` branches by peak when the peak sentinel is enabled;
5. top `sentinel.top.n` branches by canonical prominence when the prominence
   sentinel is enabled; and
6. top `sentinel.top.n` branches by trajectory-flow support when the support
   sentinel is enabled.

Top-N boundaries include complete ties. Each sentinel retains all inclusion
reasons. For non-overlapping display counts, primary-reason precedence is:

```text
pinned, component_survivor, peak, prominence, support
```

Canonical ancestor closure is obtained from the public `gflow` accessor.
Closure-only IDs are the final IDs not present in the pre-closure union.

### Transient selection and pinning

`selected.ids` and `pinned.ids` have different lifetimes:

- `selected.ids` is transient linked-view presentation state. It never changes
  proposal membership and is not stored in the proposal.
- `pinned.ids` is construction-scoped session state. Every pin or unpin event
  is proposal-affecting and reconstructs the mandatory union, closure, layout,
  counts, and overflow outcome.

Selecting a hidden branch in the all-basin Plot Workspace highlights it in
that workspace and in any complete interactive view, but does not silently add
it to the filtered static tree. The merge-tree panel reports that the selected
branch is hidden and offers an explicit Pin action. Pinning starts a new
proposal attempt; opening the complete interactive tree remains nonmutating.

## Rendering Budget and Overflow

Mandatory branches are never silently discarded. Core outcome and render
outcome are distinct.

Render outcome follows:

1. complete core exceeds final budget: `core_overflow`;
2. pre-closure mandatory union exceeds it: `sentinel_overflow`;
3. ancestor closure causes the excess: `closure_overflow`; or
4. otherwise: `renderable`.

Render outcome is retained as a canonical constructor result because it
identifies which mandatory stage exceeded the budget. Category counts,
coverage, and status text are derived from the canonical ID sets and reason
maps.

An overflow panel shows diagnostics, exact derived counts, coverage, warnings,
and a cause-specific explanation. `Open complete interactive tree` remains
available. Filter None routes directly to the complete interactive
presentation when its complete core exceeds the final budget.

## Label Policy

Showing a branch does not imply showing its text label.

For a renderable initial tree, Important labels are derived as the union of:

- top displayed branches by trajectory-flow mass;
- top branches by peak;
- top branches by canonical prominence;
- top branches by trajectory-flow support;
- component survivor; and
- displayed selected branches and pinned branches.

Top-N boundaries include complete ties. Label modes are Important, Selected,
Displayed, None, and All. All has an explicit crowding warning. Every label ID
belongs to the final displayed set.

Label IDs, contribution counts, and omission text are derived on demand and
are not authoritative proposal fields. When Filter None has invalid or
unavailable mass, the mass contribution is omitted with its typed reason while
the other contributions remain available.

## Required Public `gflow` Layout Contract

Implementation requires a reviewed public pure accessor equivalent to:

```r
get.basin.merge.tree.layout(
  x,
  direction = "max",
  component,
  basin.ids = NULL,
  close.ancestors = FALSE
)
```

`basin.ids = NULL` selects the complete component. Without drawing, the
accessor returns:

- direction and component;
- requested canonical IDs;
- closure-added IDs;
- exact selected branch and merge-event tables;
- component root/survivor ID;
- crossing-free leaf order and branch/event coordinates; and
- validation status.

The accessor validates complete-tree finite births, deaths, merge levels, and
prominence, plus nonnegative prominence. It rejects unknown IDs, mixed
directions/components, missing roots, and nonclosed selections.

With `close.ancestors = TRUE`, it adds and reports canonical ancestors.
Canonical vertical values remain unchanged; horizontal coordinates are
deterministically compressed for the restricted canonical leaf order.

`plot.basin.merge.tree()` consumes this accessor and exposes matching
`basin.ids` and `close.ancestors` arguments. Static and interactive renderers
of the same selection use identical selected branches, events, closure, order,
and coordinates.

## Runtime Proposal Contract

### Constructor boundary

The pure proposal constructor receives:

- active runtime context;
- immutable validated `runtime.scientific.bundle`;
- selected component;
- accepted active/common parameters;
- pinned canonical IDs; and
- current `context.generation` and `attempt.id`.

It returns either:

- a successful internal `basin_display_proposal`; or
- one typed blocked/stale attempt result with no proposal IDs.

Validation occurs once at this boundary. Before installation, constructor
postconditions assert:

- context generation and attempt ID remain current;
- proposal and active runtime `bundle.id` are identical;
- every proposal ID belongs to the mapped component;
- mass ties are unsplit;
- sentinel reasons refer to sentinel IDs;
- final IDs equal the ancestor-closed mandatory union;
- core IDs are a subset of final IDs;
- overflow outcome agrees with the three mandatory set sizes and final budget;
  and
- deterministic ID ordering is canonical.

There is no public proposal validator or proposal deserializer in version 1.

### Minimal authoritative fields

The internal proposal contains:

```text
context.key
bundle.id
context.generation
attempt.id
accepted.parameters
component:
  id
  ids
  selection.rule
  fallback.reason
pinned.ids
mass.status
core:
  ids
  outcome
  warnings
  boundary
  gap.decades
  informational.cutoff
sentinels:
  ids
  reasons
ancestor.only.ids
final.ids
render.outcome
```

Nullable core diagnostics are absent or `NULL` when not applicable. The object
is construction-scoped and in memory only.

The proposal does not authoritatively store:

- tie-group tables or endpoints;
- denominator, positive/zero counts, or cumulative coverage;
- category or primary-reason counts;
- label IDs or label contribution counts;
- status text;
- layout coordinates;
- proposal fingerprints;
- creation timestamps; or
- serialized validation mirrors.

These values are derived from the exact matching immutable scientific bundle
and authoritative proposal fields. Every derivation helper checks
`proposal.bundle.id == bundle.id` before use. Rendering code may cache derived
values, but invalidating the proposal or replacing the bundle also discards
its caches.

### Attempt and display state

The session owns:

```text
bundle.id
context.generation
next.attempt.id
active.attempt
current.proposal
retained.last.valid.proposal
pinned.ids
selected.ids
display.source:
  current | retained_last_valid | none
```

Every proposal-affecting event synchronously allocates and installs a new
monotonically increasing `active.attempt.id` before validation or asynchronous
work begins. This invalidates every earlier pending result even when the new
input is invalid and launches no constructor.

Proposal-affecting events are:

- bundle replacement or any other context change;
- explicit component change;
- Filter mode change, including Show all;
- change to an active mode-specific selection setting;
- change to core or final budget;
- change to `sentinel.top.n`;
- change to any peak, prominence, or support sentinel toggle; and
- pin or unpin.

Transient selection, label mode/count, diagnostic visibility, and opening a
viewer are presentation-only events. They do not allocate a proposal attempt.
Inactive mode-specific values allocate an attempt only when their mode becomes
active.

An asynchronous result is installed through one atomic compare-and-swap
operation requiring equality of:

```text
result.bundle.id          == active bundle.id
result.context.generation == active context.generation
result.attempt.id         == active attempt.id
```

Any mismatch discards the result as stale without changing display state.

### Proposal-slot transition table

`current.proposal` and `retained.last.valid.proposal` never alias the same
owned slot. Their transitions are:

| Event | Synchronous state transition | Result handling |
|---|---|---|
| Proposal-affecting same-context input change | allocate active attempt; move owned `current.proposal`, when present, to `retained.last.valid.proposal`; clear `current.proposal`; display retained when present | continue to validation |
| Valid construction start | keep current empty and retained unchanged; mark active attempt pending | await only the matching result |
| Invalid active setting | mark active attempt `blocked/settings_invalid`; keep current empty and retained unchanged | display retained when present, otherwise none |
| Invalid source, mapping, or ranking bundle | replace/invalidate bundle as appropriate; increment context generation; allocate active attempt; clear current, retained, pins, selection, and pending state | display none |
| Context or explicit component change | install the new context and replacement bundle when scientific data changed; increment generation; allocate active attempt; clear current, retained, pins, selection, and pending state | validate and recompute in the new context |
| Stale result arrival | no slot change | discard |
| Successful current result | atomically verify bundle, generation, and attempt; install as `current.proposal`; clear obsolete retained slot | display current |
| Failed current construction | mark matching attempt `construction_failed`; keep current empty and retained unchanged | display retained when present, otherwise none |

For a same-context event when current is already empty, an existing retained
proposal remains retained until a current result succeeds or scientific/context
validity is lost. Moving a current proposal to retained transfers ownership;
it does not copy or alias the slot.

The active-attempt status describes current controls and validation. Displayed
status is always derived from the proposal actually shown. The two are never
blended.

## Persistence Contract

Version 1 persists recipes, not results.

The version 1 recipe is a generic supported-feature preference recipe. It is
not construction-scoped and carries no graph, source, subject, component, or
bundle identity.

A saved settings recipe contains:

```text
recipe.version
filter.mode
common proposal settings:
  final.render.budget
  sentinel.top.n
  peak.sentinel.enabled
  prominence.sentinel.enabled
  support.sentinel.enabled
active mode-specific validated settings
presentation settings:
  important.label.n
  label.mode
```

The recipe contains no canonical IDs, component selection, mass groups,
coverage, outcomes, layout, proposal object, or proposal fingerprint.

Restoration:

1. validates the recipe version and values;
2. confirms that the active context supports a maximum occupation-density
   merge tree and every recipe feature;
3. resolves and validates the active graph, source, mappings, constructions,
   and immutable scientific bundle;
4. reruns automatic component selection;
5. applies the recipe to that runtime context;
6. runs the ordinary proposal constructor; and
7. refuses restoration only for an unknown recipe version, unsupported
   direction/feature/mode, invalid parameter value, or unavailable required
   runtime context.

An explicit component choice is not persisted or restored.

Inactive mode-specific UI values need not be persisted in version 1. Session
state may retain them while the context remains active.

## Audit Export Boundary

Portable audit export is a separate future feature, not part of proposal
restoration. A future one-way `export_basin_proposal_audit()` may record
context identities, accepted settings, canonical IDs, outcomes, and derived
diagnostics for review.

Such an export requires its own versioned interchange contract and audit. It
must not be accepted by `gflowui` as executable proposal state unless a future
specification explicitly introduces and secures that import boundary.

## User Interface

The initial panel order is:

1. Basin Superlevel-Set Merge Tree;
2. Basin Plot Workspace;
3. Basin Inspector.

An ordinary status line reports component counts, core/final counts, selected
filter outcome, coverage, sentinel-only counts, ancestor-only count, warnings,
and exact mass ownership. Every displayed summary is derived from the current
runtime proposal and its exact matching immutable scientific bundle.

Controls:

- Component;
- Filter: Auto / Cumulative Mass / Minimum Mass / Top K / None;
- Mass coverage;
- Strong-gap threshold;
- Minimum raw trajectory-flow mass;
- Top K;
- Core branch budget;
- Final render budget;
- peak, prominence, and support sentinel toggles/counts;
- Important-label count;
- Labels: Important / Selected / Displayed / None / All;
- Pin/unpin selected basin;
- Show diagnostic;
- Open complete interactive tree; and
- Show all.

Complete-tree actions:

- **Filter = None** is persistent session filter state and constructs a
  `complete` core.
- **Show all** sets Filter to None and recomputes; it is not a temporary visual
  override.
- **Open complete interactive tree** is a nonmutating viewer action.

The Plot Workspace initially shows, on log10 rank scales:

- extremum-value rank versus support rank; and
- extremum-value rank versus mass rank.

It uses all maximum basins in the selected component. Cross-panel selection
uses canonical basin IDs as a display-only overlay and does not reconstruct
the proposal. Selecting a hidden branch exposes its hidden status and Pin
action under the transient-selection contract above.

## Proposal State Model

Attempt and proposal outcomes remain distinct:

```text
active.attempt.validation:
  identity: current | stale
  bundle: valid | bundle_invalid | bundle_mismatch
  source: valid | source_invalid
  mapping: valid | mapping_invalid
  ranking.measure:
    trajectory_flow_mass:
      valid | mass_invalid | mass_unavailable
    trajectory_flow_support:
      valid | support_invalid
    source_peak:
      valid | peak_invalid
    canonical_prominence:
      valid | prominence_invalid
  proposal.settings: valid | settings_invalid

active.attempt.outcome:
  pending | proposal_created | blocked | construction_failed | stale

proposal.core.outcome:
  strong_gap | coverage | single_positive | coverage_capped |
  minimum_mass | threshold_empty | top_k | complete

proposal.core.warnings:
  tie_overflow

proposal.render.outcome:
  renderable | core_overflow | sentinel_overflow | closure_overflow

display.source:
  current | retained_last_valid | none
```

Blocking precedence:

1. stale context;
2. invalid or mismatched scientific bundle;
3. invalid source or mapping;
4. invalid support, peak, or prominence;
5. invalid active proposal settings;
6. invalid/unavailable mass in a mass-based mode; and
7. otherwise proposal construction.

Filter None may construct `complete` when mass alone is invalid or unavailable,
with mass-derived views disabled. No proposal exists for a blocked or stale
attempt.

## Subject 15 Evidence and Portable Test Fixture

For the Brier-selected Subject 15 occupation density on the symmetric `k=3`
graph:

- maximum basins: 352;
- positive-mass denominator: `1.0000000000000087`;
- rank-17 mass: `0.0122134243817115`;
- rank-18 mass: `1.40305377913392e-15`;
- gap after rank 17: `12.9397631299771` decades;
- geometric midpoint: `4.13957621441213e-09`;
- rank-1:17 raw mass sum: `0.99999999999992595`;
- rank-1:17 normalized coverage: `0.99999999999991729`; and
- rank-18:352 mass: `8.72873121029731e-14`.

The canonical merge-tree primary-support mass is different from
trajectory-flow mass and is not substituted for it.

The clean-checkout test fixture is:

- `tests/testthat/fixtures/basin_merge_tree_subject15_maxima.csv`
- `tests/testthat/fixtures/basin_merge_tree_subject15_maxima_provenance.csv`

It contains all 352 maxima, trajectory and canonical IDs, canonical parents,
component survivor, trajectory-flow mass/support, peak value, and canonical
prominence. Provenance pins the upstream revision and source assets. Fixture
portability does not imply runtime proposal serialization.

## Required Tests

### Scientific bundle, inputs, and mapping

1. Validate complete finite source fields and exact graph-vertex alignment.
2. Reject incomplete, duplicate, mixed-direction, or mixed-component mapping.
3. Validate all four whole-direction ranking vectors before component
   selection.
4. Exercise positive, all-zero, invalid-mass, invalid-support, invalid-peak,
   and invalid-prominence states.
5. Confirm Filter None's mass-only exception without enabling mass-derived
   displays.
6. Confirm every installed proposal and derivation bundle has the same
   `bundle.id`.
7. Replace source values, mass/support vectors, mappings, and canonical trees
   while retaining human-readable names; each replacement must mint a new
   bundle ID, advance context generation, and invalidate proposals.
8. Confirm caller-side in-place mutation cannot alter an installed bundle;
   attempt mutation through every supported construction path.
9. Reject a derivation request that pairs a proposal with a different bundle.
10. Confirm context and bundle changes clear current, retained, cached, and
    pending state.

### Parameters, controls, and recipes

11. Reject missing, nonfinite, fractional, negative, and out-of-domain active
    settings.
12. Reject every whole-number control above `.Machine$integer.max`.
13. Reject `top.k` above selected-component size.
14. Confirm inactive mode-specific settings cannot block the active mode.
15. Confirm accepted proposal parameters contain only active/common
    membership or overflow settings.
16. Confirm `minimum.core.branches` is fixed at 3 and is neither a UI control
    nor a recipe field.
17. Confirm `important.label.n` is a validated presentation control and recipe
    field but not an authoritative proposal field.
18. Confirm generic saved recipes are revalidated and recomputed rather than
    restoring a proposal.
19. Confirm recipe restoration reruns automatic component selection and does
    not restore an explicit component.
20. Reject unknown recipe versions, unsupported features, invalid parameters,
    or unavailable required runtime context.

### Selection algorithm

21. Verify row-permutation determinism and complete tie handling.
22. Verify automatic component totals use canonical branch-ID summation order
    with near-equal totals and exact stable-component-ID tie breaking.
23. Cover strong gap, comparable gaps, smooth tail, terminal gap, all-equal
    masses, coverage ties, and budget ties.
24. Cover negative, missing, nonfinite, all-zero, one-positive, and
    two-positive mass vectors.
25. Confirm exact zeros never enter logarithms.
26. Confirm core and final coverage derive from the same denominator.
27. For Cumulative Mass, cover target reached below budget, exactly at budget,
    beyond budget with a straddling tie group, and beyond budget without a
    straddling group.
28. Confirm Cumulative Mass returns the specified `coverage` or
    `coverage_capped` outcome and adds `tie_overflow` only for a complete
    budget-straddling group.
29. Confirm Minimum Mass uses raw mass units.
30. Confirm Top K and Minimum Mass include complete zero ties when applicable.
31. Confirm Filter None returns `complete`.

### Constructor and runtime state

32. Assert every installed proposal ID belongs to the mapped component.
33. Assert final IDs equal the ancestor-closed mandatory union.
34. Assert sentinel reasons and primary-reason precedence are exact.
35. Assert each disabled sentinel family contributes no IDs or reasons.
36. Assert render outcome agrees with core, pre-closure, closure, and budget.
37. Assert category counts, labels, coverage, and status text derive from
    canonical proposal fields and cannot drift independently.
38. Confirm every proposal-affecting event advances active attempt ID
    synchronously before validation.
39. While a valid construction is pending, enter an invalid active setting and
    confirm the earlier result is rejected and the retained display remains.
40. While a valid construction is pending, replace the bundle or context and
    confirm the earlier result is rejected and no retained proposal remains.
41. Exercise every row of the proposal-slot transition table, including a
    failed matching construction and a stale result.
42. Confirm successful atomic installation compares bundle ID, context
    generation, and attempt ID, then clears retained state.
43. Confirm current and retained slots never alias.
44. Confirm recomputation with the same bundle, context, and recipe is
    deterministic.

### Topology and interaction

45. Confirm complete-tree identity is unchanged by every filter.
46. Confirm the public `gflow` accessor rejects unknown, mixed, and nonclosed
    selections unless closure is requested.
47. Confirm filtered layouts preserve canonical vertical values, parentage,
    events, and restricted leaf order.
48. Exercise `core_overflow`, `sentinel_overflow`, and `closure_overflow`.
49. Confirm mandatory branches are never silently discarded.
50. Confirm Filter None, Show all, and Open complete interactive tree have
    distinct persistent, shortcut, and viewer semantics.
51. Confirm selecting a hidden workspace branch changes only the transient
    overlay and exposes hidden/Pin status without changing proposal IDs.
52. Confirm pin and unpin each allocate a new attempt and recompute membership,
    closure, layout, counts, and overflow.
53. Confirm each sentinel toggle allocates a new attempt and conditionally
    changes the mandatory union.
54. Confirm label and diagnostic controls remain presentation-only.
55. Confirm desktop and narrow viewports remain usable.

### Subject 15 regression

56. Validate all 352 fixture mappings and canonical parents.
57. Reproduce the rank-17 strong-gap core, exact coverage, and final IDs.
58. Confirm Filter None exposes all 352 branches when the complete canonical
    object is available.

### Explicitly removed Revision 8 tests

Version 1 does not require tests for:

- signed-64 wire boundaries or custom integer carrier classes;
- canonical proposal/view wire-token digests;
- proposal, attempt, or view-state fingerprints;
- proposal/view-state serialization round trips;
- external proposal deserialization;
- consistently re-fingerprinted proposal mutation; or
- portable exact proposal restoration.

These tests become relevant only if a future audited import/export contract
introduces an external proposal boundary.

## Implementation Order

1. Review and implement the public pure filtered-layout accessor in `gflow`.
2. Implement the immutable scientific-bundle builder and replacement-only
   ownership boundary in `gflowui`.
3. Implement the minimal runtime context, generic recipe, proposal
   constructor, and single attempt/display transition reducer.
4. Implement bundle-bound derivation helpers for counts, coverage, labels,
   diagnostics, and status text.
5. Replace Revision 8 wire-schema tests with the Revision 9 constructor,
   recipe, range, lifecycle, and derivation tests.
6. Build the merge-tree panel and overflow presentation.
7. Wire linked transient selection, pinning, and construction-scoped settings.
8. Run desktop and narrow-viewport visual QA.

Implementation acceptance does not imply scientific acceptance of adaptive
filtering or EOD interpretation.
