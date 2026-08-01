# Adaptive Initial Filtering for Basin Merge Trees

## Status

Revision 7, prepared after the 2026-07-31 specification audit and subsequent
re-audits through the Revision 6 re-audit dated 2026-08-01.

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
merge-survivor policy with its own noncircular measurement and tie contract,
not a reinterpretation of the density-value elder rule.

### Scientific and display invariants

1. `gflow::get.basin.merge.tree()` supplies the complete canonical tree.
2. Every eligible source field has exactly one finite value per graph vertex.
3. Filtering changes only initial presentation.
4. Births, deaths, parents, survival under the density-value elder rule,
   prominence, assignments, mass, support, and graph alignment remain
   unchanged.
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

After canonical mapping, every ranking vector has one value per branch in the
complete selected direction and must satisfy these exact domains:

- trajectory-flow `primary.support.mass`: finite and nonnegative;
- trajectory-flow `primary.support.size`: finite, nonnegative whole numbers;
- selected-field peak value: finite;
- canonical persistence/prominence: finite and nonnegative.

No missing value, implicit omission, coercion, rounding, or replacement is
permitted. Exact zero support and zero prominence are valid.

For a non-surviving maximum branch, canonical prominence is
`birth.level - death.level`. For the survivor under the density-value elder
rule in each component, death is the component minimum of the selected field
and prominence is `birth.level - component.minimum`. Thus the survivor also
has one finite, nonnegative prominence; infinity and `NA` are not survivor
conventions.

Tree-native support mass, when shown, must be labeled
`merge-tree primary support mass`. It must not be called simply `Mass`, and it
must not replace trajectory-flow mass in the version 1 proposal.

### Required mapping

Across the complete selected direction, before automatic component selection,
`gflowui` maps each trajectory-flow basin to one canonical merge-tree branch
by:

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
that component. Its survivor under the density-value elder rule is mandatory.

The UI reports:

- the total maximum-basin count across the direction;
- the number of graph components;
- the selected component ID; and
- the maximum-basin count in that component.

When several components exist, the component selector is visible. Component
selection follows a whole-direction pre-pass:

1. enumerate components in stable canonical component-ID order;
2. validate the whole-direction trajectory-flow-to-canonical mapping;
3. validate every declared ranking vector across all maximum branches;
4. when the mapping and mass vector are valid and at least one component has
   positive mass, select the component with greatest positive-mass total,
   breaking ties by stable component ID; a separate invalid ranking vector
   still blocks proposal construction;
5. when the mapping and mass vector are valid but all component totals are zero,
   select the smallest stable component ID and record
   `smallest_component_mass_unavailable`; and
6. when the mapping is invalid, select the smallest stable component ID for
   deterministic error presentation, record
   `smallest_component_mapping_invalid`, and expose `mapping_invalid`;
7. when any mass is missing, negative, or nonfinite, disable mass-based
   component selection, select the smallest stable component ID, and record
   `smallest_component_mass_invalid`.

The all-zero fallback exposes `mass_unavailable`; the invalid-mass fallback
exposes `mass_invalid`. Neither attempts a mass proposal. The user may choose
another component explicitly, but the mass status remains in force.

The proposal records all valid component totals or records that totals were
unavailable, the component-selection rule, selected component, tie-break, and
fallback reason. Changing direction or component invalidates the proposal.

## Source and Ranking-Measure Validation

Source validation precedes adaptive filtering:

1. the source field must have one finite value per graph vertex;
2. graph, vertex, field, and construction fingerprints must match; and
3. the complete canonical tree must already be valid.

A missing or nonfinite source-field value produces `source_invalid`. Filtering
must not drop vertices or construct a partial graph.

After one-to-one mapping, all four ranking vectors are validated across the
whole maximum direction before automatic component selection. This avoids
component-dependent acceptance and means component switching does not expose
previously unchecked values.

The ranking-validation map is:

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

The declared trajectory-flow mass vector is then summarized for the selected
component:

- missing, negative, or nonfinite mass produces `mass_invalid`;
- exact zero is valid, excluded from logarithms, and counted separately;
- zero-mass branches remain eligible for Filter None, sentinels, and ancestry;
- zero total positive mass produces `mass_unavailable`;
- normalization occurs only after the complete declared vector validates.

`mass_invalid` and `mass_unavailable` disable Auto, Cumulative Mass, Minimum
Mass, and mass-ranked Top K. A valid canonical tree may still use Filter None
when mass alone is invalid or unavailable. In that canonical-only mode,
mass-derived annotations, coverage, diagnostics, and the mass-rank pair plot
are unavailable and explicitly identified as such.

Support-size validation requires one numeric value per mapped branch. Missing,
nonfinite, negative, or fractional values produce `support_invalid`; zero is
valid. Peak validation is inherited from the selected source field and
rechecked after extremum mapping; a missing or nonfinite mapped peak produces
both `peak_invalid` and `source_invalid`. Canonical prominence requires one
finite, nonnegative value per mapped branch under the survivor convention
above; failure produces `prominence_invalid`.

Version 1 does not degrade around invalid support, peak, or prominence. Any of
`support_invalid`, `peak_invalid`, or `prominence_invalid` blocks the current
coordinated proposal in every filter mode, regardless of sentinel toggles or
label mode, because mandatory sentinels, Important labels, and the Plot
Workspace share those vectors. No sentinel, label, final-ID, or layout subset
is computed from an invalid ranking vector.

Both mass-core coverage and final displayed-set coverage use the same declared
positive-mass denominator. The denominator, positive count, and exact-zero
count are recorded; it is never silently changed.

### Typed mass-derived availability

Every successful proposal records:

```text
mass.derived.available:
  true | false
mass.derived.unavailable.reason:
  null | mass_unavailable | mass_invalid
```

These fields govern every mass-derived proposal field. They are not inferred
from a numeric sentinel. `NaN`, `Inf`, and fabricated zero coverage are
forbidden. The exact proposal representations are:

| Mass state | Positive groups | All-mass groups | Denominator | Positive count | Zero count | Core/final coverage |
|---|---|---|---|---|---|---|
| `valid` | complete ordered list | complete ordered list | finite and positive | exact nonnegative integer | exact nonnegative integer | finite values in `[0, 1]` |
| `mass_unavailable` | empty list | one complete zero-mass group containing every component branch | exact `0` | exact `0` | component branch count | null with reason `mass_unavailable` |
| `mass_invalid` | null | null | null | null | null | null with reason `mass_invalid` |

For Filter None with `mass_unavailable` or `mass_invalid`, the complete core
and final canonical IDs remain available. The mass-ranked Important-label
contribution is an empty ID list and records omission reason
`trajectory_flow_mass:<mass state>`. Valid peak, prominence, support,
component-survivor, and selected-or-pinned label contributions are retained.
Mass diagnostics, the mass-rank pair plot, and mass coverage are unavailable
with the same typed reason. Other proposal fields remain present.

A change in the mass vector first invalidates any displayed or retained
proposal whose context includes the prior mass-source fingerprint. After that
invalidation, a fresh Filter None attempt may install a new canonical-only
proposal under the contract above. A mass-based mode remains blocked and
leaves no displayed proposal.

## Parameter Validation

The pure proposal helper and Shiny controls use the same strict domains:

| Parameter | Valid domain |
|---|---|
| `filter.mode` | one of Auto, Cumulative Mass, Minimum Mass, Top K, None; serialized as `auto`, `cumulative_mass`, `minimum_mass`, `top_k`, `none` |
| `coverage.target` | finite scalar with `0 < value <= 1` |
| `strong.gap.decades` | finite nonnegative scalar |
| `minimum.core.branches` | positive whole-number scalar |
| `core.branch.budget` | positive whole-number scalar and not less than `minimum.core.branches` |
| `final.render.budget` | positive whole-number scalar |
| `sentinel.top.n` | nonnegative whole-number scalar |
| `important.label.n` | nonnegative whole-number scalar |
| `top.k` | when Top K is active: positive whole-number scalar not exceeding selected-component branch count |
| `minimum.mass` | when Minimum Mass is active: finite nonnegative scalar in raw trajectory-flow mass units |
| sentinel toggles | nonmissing logical scalars |

There is no required ordering between `core.branch.budget` and
`final.render.budget`. A smaller final budget is valid and may produce
`core_overflow`.

Sentinel and label counts define nominal rank boundaries, not hard maximum
counts. When fewer than N branches are available, all are considered. When the
Nth rank belongs to a tie extending beyond N, the complete tie is included,
the result may exceed N, and both requested N and tie-expanded count are
recorded.

Validation is mode-aware:

- Auto validates coverage, gap, minimum-core, and core-budget settings;
- Cumulative Mass validates coverage and core-budget settings;
- Minimum Mass validates `minimum.mass`;
- Top K validates `top.k`;
- None has no mode-specific mass-selection input; and
- common final-budget, sentinel, label, and toggle settings are always
  validated.

Inactive mode-specific values are retained in construction-scoped session
state but are not validated and cannot block the active mode. On first
activation, Top K initializes to `min(10, component branch count)` and Minimum
Mass initializes to `0`. Later switches restore the retained value and
validate it before recomputation. A valid Minimum Mass threshold matching no
branch returns `threshold_empty`.

Invalid, missing, nonfinite, fractional whole-number, or out-of-domain settings
return `settings_invalid` with field-specific messages. The pure helper does
not coerce or clamp them. The UI retains and continues displaying the last
valid proposal, marks it as retained rather than current, shows the invalid
input, and does not recompute until all settings validate.

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

Two exact groupings are stored:

1. **Positive-mass groups** contain only masses greater than zero. They drive
   Auto, Cumulative Mass, logarithms, gaps, and normalized coverage.
2. **All-mass ranking groups** contain every validated nonnegative mass. They
   drive Top K and Minimum Mass and include one complete zero-mass group when
   zeros are present.

Within either grouping, exact equality of stored validated numeric values
defines an indivisible group; no tolerance is introduced. Groups are ordered
by descending mass, and canonical branch IDs are ordered lexicographically
within a group.

All rank boundaries occur after complete groups. The proposal records both
ordered groupings, exact mass values, member IDs, endpoints, and, for positive
groups, cumulative coverage. Row order cannot affect the result.

The positive-mass denominator and cumulative sums are evaluated in descending
positive-group order, with canonical-ID order inside groups. This fixes the
floating-point accumulation order and makes coverage invariant to input row
permutation.

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
top.k.initial         = min(10, selected component branch count)
minimum.mass.initial  = 0 raw trajectory-flow mass
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
   include the complete group, add warning `tie_overflow`, and return
   `coverage` if the expanded group reaches the target or `coverage_capped`
   otherwise. Without a straddling tie, select the last complete group
   endpoint not exceeding the budget and return `coverage_capped`.
6. Never split equal masses. `tie_overflow` is an additive warning, not a
   substitute for the primary core-selection outcome.
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

- **Cumulative Mass:** uses positive-mass groups, selects the first complete
  group reaching `coverage.target`, applies `core.branch.budget`, and returns
  `coverage`, `coverage_capped`, or `single_positive`. A budget-straddling
  group is included completely with warning `tie_overflow`.
- **Minimum Mass:** compares `minimum.mass` directly with raw validated
  trajectory-flow `primary.support.mass`, not component-normalized share.
  It selects every complete all-mass group with mass greater than or equal to
  the threshold and returns `minimum_mass`; no match returns
  `threshold_empty`. Therefore `minimum.mass = 0` includes the complete zero
  group and every branch in the selected component.
- **Top K:** ranks all-mass groups and includes the complete group containing
  rank `top.k`. It returns `top_k`; if the boundary group extends beyond K,
  including when K enters the zero group, it adds warning `tie_overflow`.
- **Filter None:** selects every canonical branch in the selected component
  and returns `complete`.

The Auto and Cumulative Mass modes use `core.branch.budget`. Minimum Mass,
Top K, and None preserve the explicitly requested core even when it exceeds
that automatic-selection budget. Every mode is evaluated against
`final.render.budget`; no mode splits a mass tie or silently truncates a core.

## Mandatory Sentinels

Within the selected component, the mandatory union contains:

1. the mass core;
2. selected or pinned branches;
3. the component survivor under the density-value elder rule;
4. the top `sentinel.top.n` branches by descending maximum peak value;
5. the top `sentinel.top.n` branches by canonical prominence; and
6. the top `sentinel.top.n` branches by trajectory-flow support.

Sentinel Top-N values are nominal rank boundaries. Boundary ties are included
completely and may expand the sentinel set beyond N. Each measure records
requested N, tie-expanded count, and every branch inclusion reason. For
non-overlapping displayed counts, an added branch receives the first applicable
primary reason in this precedence:

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

Core selection outcome and final render outcome are independent. For a current
core, rendering uses this cause-based precedence:

1. if the complete core exceeds the final budget, return `core_overflow`;
2. otherwise, if the pre-closure sentinel union exceeds it, return
   `sentinel_overflow`;
3. otherwise, if ancestor closure causes the excess, return
   `closure_overflow`; and
4. otherwise return `renderable`.

A core can therefore have outcome `top_k`, warning `tie_overflow`, and render
outcome `core_overflow`. The same `core_overflow` rule applies to Auto,
Cumulative Mass, Minimum Mass, Top K, and Filter None.

In any overflow state, the panel initially shows the diagnostic, exact counts,
coverage, warnings, and a concise overflow explanation instead of compressing
hundreds of branches into a static tree. `Open complete interactive tree`
opens the canonical component in a zoomable, scrollable view with important
labels only by default. The Show all shortcut remains available. The Plot
Workspace still uses all component basins.

Filter None routes directly to the complete interactive presentation when
its complete-component core exceeds the final budget. It never attempts to
compress that core into the initial static tree.

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

Important-label Top-N values are nominal rank boundaries. Boundary ties are
included completely and may expand labels beyond N; requested N and expanded
count are reported per measure. Label modes are Important, Selected,
Displayed, None, and All. All is permitted with an explicit crowding warning.
A label ID must be in the final displayed branch set.

When Filter None is valid except for `mass_invalid` or `mass_unavailable`, the
trajectory-flow-mass item contributes no labels and records its typed omission
reason. The other five contributions are evaluated normally. The union is not
discarded merely because its mass-ranked contribution is unavailable.

## Required Public `gflow` Layout Contract

Implementation requires a reviewed public pure accessor with behavior
equivalent to:

```r
get.basin.merge.tree.layout(
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

Before returning a layout, the public accessor validates finite branch birth,
death, and persistence values and nonnegative persistence for the complete
canonical tree, including component survivors under the finite component-floor
convention. An invalid canonical prominence is a tree-validation error; it is
not omitted or repaired by the layout accessor.

Canonical vertical values and display-layout horizontal values are distinct:

- branch births, branch deaths, merge levels, persistence, parent identity,
  and survivor identity are exact canonical values;
- the selected leaf order is the complete canonical crossing-free leaf order
  restricted to the selected canonical IDs; and
- filtered leaf/trunk x coordinates are deterministically compressed and
  reindexed for that restricted order.

It rejects unknown IDs, mixed directions, mixed components, missing roots, and
non-closed selections. With `close.ancestors = TRUE`, it adds and reports
canonical ancestors instead of rejecting only the non-closure.

`plot.basin.merge.tree()` must consume this same accessor and expose matching
`basin.ids` and `close.ancestors` arguments. Static and interactive renderers
of the same filtered selection must therefore use identical selected branches,
events, closure, restricted order, and filtered coordinates. Filtered x
coordinates are not required to equal positions in the complete layout.

## Versioned Proposal Record

Every successfully constructed algorithm proposal is immutable and
serializable as `gflowui_basin_merge_tree_display_proposal/3`. Revision 3
supersedes the unimplemented proposal-schema revision 2 by moving transient
attempt and display state into the separate view-state envelope below.

### Closed wire-schema rules

The context, proposal, and view-state schemas below are closed. A conforming
object has every listed key exactly once and no unlisted key. A deserializer
rejects a missing key, additional key, wrong scalar/container type, invalid
cardinality, invalid enum, noncanonical ID order, or disallowed null before
checking fingerprints. It returns `schema_invalid`; it does not drop,
default, or reinterpret fields.

The type notation is:

```text
string       one nonmissing UTF-8 string
integer      one nonmissing whole number in signed 64-bit range
number       one finite binary64 value
logical      one nonmissing Boolean
array<T>     ordered, possibly empty array of T
object       closed named object
nullable<T>  either typed null or T
enum{...}    one string from the listed values
id-array     lexicographically sorted unique array<string>
```

Schema evolution uses a new terminal schema version. Proposal/3 and context/1
do not admit extension keys from later versions.

### Context/1 schema

`gflowui_basin_merge_tree_context/1` has this exact field set:

| Key | Type |
|---|---|
| `schema` | string, exactly `gflowui_basin_merge_tree_context/1` |
| `project_identity` | string |
| `subject_identity` | string |
| `graph_identity` | string |
| `topology_fingerprint` | string |
| `vertex_map_fingerprint` | string |
| `selected_field_identity` | string |
| `selected_field_fingerprint` | string |
| `selected_source_identity` | string |
| `selected_source_fingerprint` | string |
| `estimate_identity` | string |
| `trajectory_flow_construction_identity` | string |
| `trajectory_flow_construction_fingerprint` | string |
| `canonical_tree_construction_identity` | string |
| `canonical_tree_construction_fingerprint` | string |
| `direction` | enum{`max`} |
| `component` | positive integer |

Both construction identities and both construction fingerprints are required.

### Proposal/3 schema

`gflowui_basin_merge_tree_display_proposal/3` has this exact top-level field
set:

| Key | Type |
|---|---|
| `schema` | string, exactly `gflowui_basin_merge_tree_display_proposal/3` |
| `context` | context/1 object |
| `context_fingerprint` | 64-character lowercase hexadecimal string |
| `proposal_fingerprint` | 64-character lowercase hexadecimal string |
| `creation_time` | ISO-8601 string with numeric UTC offset |
| `algorithm` | Algorithm object |
| `component_selection` | ComponentSelection object |
| `measures` | Measures object |
| `validation` | ProposalValidation object |
| `mapping` | Mapping object |
| `accepted_parameters` | Parameters object |
| `mass_derived` | MassDerived object |
| `core` | Core object |
| `sentinels` | Sentinels object |
| `ancestor_only_ids` | id-array |
| `final` | Final object |

The closed nested objects are:

| Object | Exact keys and types |
|---|---|
| Algorithm | `name`: string; `version`: positive integer |
| ComponentSelection | `rule`: string; `component_totals`: nullable<array<ComponentTotal>>; `tie_break`: string; `fallback_reason`: nullable<string>; `direction_basin_count`: nonnegative integer; `graph_component_count`: positive integer; `selected_component_basin_count`: nonnegative integer |
| ComponentTotal | `component`: positive integer; `mass_total`: finite nonnegative number; ordered by component |
| Measures | `trajectory_flow_mass`, `trajectory_flow_support`, `source_peak`, `canonical_prominence`: each a Measure object |
| Measure | `name`: string; `owner_identity`: string |
| ProposalValidation | `identity`: enum{`current`}; `source`: enum{`valid`}; `mapping`: enum{`valid`}; `ranking_measure`: RankingValidation object; `settings`: enum{`valid`} |
| RankingValidation | `trajectory_flow_mass`: enum{`valid`, `mass_invalid`, `mass_unavailable`}; `trajectory_flow_support`, `source_peak`, `canonical_prominence`: each enum{`valid`} |
| Mapping | `cardinality`: nonnegative integer; `direction`: enum{`max`}; `component`: positive integer |
| Parameters | `filter_mode`: enum{`auto`, `cumulative_mass`, `minimum_mass`, `top_k`, `none`}; `coverage_target`: number in `(0,1]`; `strong_gap_decades`: nonnegative number; `minimum_core_branches`, `core_branch_budget`, `final_render_budget`, `sentinel_top_n`, `important_label_n`, `top_k`: nonnegative integer, with the mode-specific positive constraints above; `minimum_mass`: nonnegative number; `include_peak_sentinel`, `include_prominence_sentinel`, `include_support_sentinel`: logical; `label_mode`: enum{`important`, `selected`, `displayed`, `none`, `all`} |
| MassDerived | `available`: logical; `unavailable_reason`: nullable<enum{`mass_unavailable`, `mass_invalid`}>; `positive_groups`: nullable<array<PositiveMassGroup>>; `all_mass_groups`: nullable<array<AllMassGroup>>; `denominator`, `core_coverage`, `final_coverage`: nullable<number>; `positive_count`, `zero_count`: nullable<nonnegative integer> |
| PositiveMassGroup | `mass`: positive number; `ids`: nonempty id-array; `endpoint`: positive integer; `cumulative_coverage`: number in `(0,1]` |
| AllMassGroup | `mass`: nonnegative number; `ids`: nonempty id-array; `endpoint`: positive integer |
| Core | `outcome`: enum{`strong_gap`, `coverage`, `single_positive`, `coverage_capped`, `minimum_mass`, `threshold_empty`, `top_k`, `complete`}; `warnings`: array<enum{`tie_overflow`}>; `boundary`: nullable<nonnegative integer>; `gap_decades`: nullable<nonnegative number>; `informational_cutoff`: nullable<nonnegative number>; `ids`: id-array |
| Sentinels | `ids`: id-array; `inclusion_reasons`: ReasonMap; `primary_reasons`: PrimaryReasonMap; `counts`: CategoryCounts object |
| Final | `ids`: id-array; `label_ids`: id-array; `label_contributions`: LabelContributions object; `label_omission_reasons`: array<string>; `category_counts`: CategoryCounts object; `render_outcome`: enum{`renderable`, `core_overflow`, `sentinel_overflow`, `closure_overflow`} |

`ReasonMap` has canonical basin IDs as dynamic keys in lexicographic order;
each value is a nonempty array of
`selected_or_pinned`, `component_survivor`, `peak`, `prominence`, or
`support`. `PrimaryReasonMap` uses the same key domain and one such enum per
key. These are the only dynamic-key objects in proposal/3.

`CategoryCounts` has exactly `mass_core`, `selected_or_pinned_only`,
`survivor_only`, `peak_only`, `prominence_only`, `support_only`,
`ancestor_only`, and `final_union`, each a nonnegative integer.
`LabelContributions` has exactly `trajectory_flow_mass`, `source_peak`,
`canonical_prominence`, `trajectory_flow_support`, `component_survivor`, and
`selected_or_pinned`, each an id-array.

MassDerived obeys the field-level availability table above. Group arrays are
ordered by descending mass, group IDs are lexicographic, and endpoints are
strictly increasing cumulative member counts. Positive-group cumulative
coverage is nondecreasing. IDs in `core`, `sentinels`, ancestors, labels, and
final records must satisfy the canonical subset and closure rules already
declared.

An invalid active attempt is not an algorithm proposal and never receives
canonical core, sentinel, label, final, or layout IDs.

Every coordinated panel validates this identity against its active graph,
field, source, constructions, direction, and component before use. A mismatch
marks the proposal stale; cached IDs are not rendered.

Adjusted settings persist only within the active session and construction
identity. They reset when graph, field, source, subject, project,
construction, direction, or component changes. Cross-context reuse requires
an explicit future opt-in mechanism and is outside version 1.

### Fingerprint contract

The context fingerprint is SHA-256 over the complete closed context/1 object
above, including its `schema` value.

The proposal fingerprint is SHA-256 over
`gflowui_basin_merge_tree_display_proposal_content/1`, containing the context
fingerprint and every deterministic scientific or display field in
proposal/3. It excludes only `proposal.fingerprint` itself and the creation
time. Thus timestamp-only differences do not change proposal content identity.

The active-attempt fingerprint is SHA-256 over
`gflowui_basin_merge_tree_active_attempt/1`, containing the context
fingerprint, the exact serialized filter mode, all active input values, and
every validation-relevant toggle or setting. It excludes computed validation
results, creation time, and any retained/displayed proposal.

The view-state fingerprint is SHA-256 over
`gflowui_basin_merge_tree_view_state_content/1`, containing every view-state/1
field except `view_state_fingerprint` itself. Unlike the attempt fingerprint,
it therefore covers stored validation, attempt and render outcomes, display
source, displayed-proposal fingerprint, and the embedded proposal.

All three hashes use this versioned canonical UTF-8 text serialization:

1. each schema fixes its field set, and named fields/maps are emitted in
   lexicographic UTF-8 key-byte order;
2. canonical-ID sets are sorted lexicographically and ordered branch/event
   tables use their declared canonical order;
3. strings carry a type token and unsigned decimal UTF-8 byte-length prefix;
4. integers use a type token and canonical base-10 ASCII representation;
5. finite numeric values use a type token and lowercase C99 hexadecimal
   floating-point representation, with negative zero normalized to positive
   zero;
6. logical true, logical false, and null use distinct typed tokens;
7. lists and maps carry an unsigned decimal element count, use distinct type
   tokens, and preserve the order fixed by rules 1 and 2; and
8. no optional whitespace is emitted.

Nonfinite numeric values are not hashable proposal content. Invalid active
input values are represented in the attempt fingerprint by typed raw-input
tokens for missing, nonfinite, and unparsable values rather than proposal
numeric fields.

On deserialization, `gflowui` first validates every closed schema, then
independently recomputes the context, proposal, active-attempt, and view-state
fingerprints. A mismatch between the embedded proposal, the envelope, or the
active context returns `fingerprint_invalid`, clears the display, and does not
rewrite or repair any fingerprint.

### View-state envelope

Transient UI state is serializable separately as
`gflowui_basin_merge_tree_view_state/1`. Its exact closed top-level schema is:

| Key | Type |
|---|---|
| `schema` | string, exactly `gflowui_basin_merge_tree_view_state/1` |
| `view_state_fingerprint` | 64-character lowercase hexadecimal string |
| `context_fingerprint` | 64-character lowercase hexadecimal string |
| `active_attempt` | ActiveAttempt object |
| `display_source` | enum{`current`, `retained_last_valid`, `none`} |
| `display_proposal_fingerprint` | nullable<64-character lowercase hexadecimal string> |
| `display_proposal` | nullable<proposal/3 object> |

ActiveAttempt has exactly:

| Key | Type |
|---|---|
| `fingerprint` | 64-character lowercase hexadecimal string |
| `input_values` | ActiveInput object |
| `validation` | AttemptValidation object |
| `outcome` | enum{`proposal_created`, `blocked`, `stale`} |
| `render_outcome` | nullable<enum{`unavailable`, `stale`}> |

ActiveInput has exactly the same keys as Parameters. Each value preserves the
typed control input before validation; invalid numeric controls may therefore
use a number outside the valid proposal domain or a typed raw-input string
token. Proposal `accepted_parameters` always satisfies the stricter Parameters
domains.

AttemptValidation has exactly `identity`, `source`, `mapping`,
`ranking_measure`, and `settings`. Its enums are those in the Proposal State
Model, including invalid alternatives.

`active.attempt.validation` includes identity, source, mapping, the complete
ranking-measure validation map, and settings validation. The active attempt
describes the current controls and their validation only. It never borrows IDs,
outcomes, or settings from `display.proposal`.

`active.attempt.render.outcome` is null when a proposal is successfully
created, `unavailable` when validation blocks construction, and `stale` for an
identity mismatch. It describes the attempt, not the visible retained
proposal.

`display.proposal` is either one complete immutable proposal or null. Its
fingerprint must equal `display.proposal.fingerprint` and its context must
equal `context.fingerprint`. The valid combinations are:

| Active attempt | Display source | Display proposal |
|---|---|---|
| valid and proposal constructed | `current` | newly constructed proposal |
| invalid active settings, same context, prior valid proposal | `retained_last_valid` | prior immutable proposal |
| invalid active settings, same context, no prior proposal | `none` | null |
| invalid or unavailable mass in an active mass mode | `none` | null |
| invalid or unavailable mass with Filter None and all other validation valid | `current` | newly constructed canonical-only complete proposal |
| invalid source, mapping, support, peak, or prominence | `none` | null |
| stale identity | `none` | null |
| context changed and recomputation not yet valid | `none` | null |

The deserializer enforces this table after fingerprint validation:

- `proposal_created` requires null attempt render outcome,
  `display_source = current`, and a nonnull conforming proposal whose
  validation and accepted parameters equal the active attempt;
- `stale` requires attempt render outcome `stale`, stale identity validation,
  `display_source = none`, and null display fields;
- `blocked` requires attempt render outcome `unavailable`;
- `retained_last_valid` is allowed only for `blocked` with
  `settings_invalid`, otherwise-valid attempt identity/source/mapping/ranking,
  and a nonnull independently valid same-context proposal;
- `none` requires both display fields to be null; and
- `current` or `retained_last_valid` requires
  `display_proposal_fingerprint` to equal the embedded proposal fingerprint.

Any disagreement returns `view_state_invalid`; no field is normalized or
rewritten. The view-state fingerprint detects isolated envelope mutation, and
the matrix validation rejects a consistently re-fingerprinted but
semantically impossible combination.

Retention is allowed only for invalid parameter edits within the unchanged
context fingerprint and while the retained proposal independently revalidates
against that context. Invalid source data, mapping, mass, support, peak,
prominence, or identity first clears the retained proposal; these are not
presentation-only input errors. After that clear, mass invalidity or
unavailability alone may produce a new current Filter None proposal. The same
mass state in a mass-based mode leaves the display empty.

A later valid recomputation atomically installs the new immutable proposal,
sets `display.source = current`, and replaces the retained candidate. A change
of graph, vertex map, field, source, subject, project, construction, direction,
or component first clears both the active attempt and retained proposal, then
starts a new context. No retained proposal crosses that invalidation edge.

The status presentation has two explicitly labeled parts:

1. **Active inputs** reports the current control values and validation, for
   example, `Top K must be a whole number; showing the last valid result`.
2. **Displayed proposal** reports the immutable visible proposal's own
   algorithm, validated settings, core/final counts, warnings, coverage,
   render outcome, and fingerprint.

When `display.source = none`, there is no visible merge-tree subset. When it is
`retained_last_valid`, the displayed proposal's prior render outcome remains
unchanged and is never relabeled as the active attempt's outcome.

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
- Minimum raw trajectory-flow mass, visible only for Minimum Mass;
- Top K, visible only for Top K;
- Core branch budget;
- Final render budget;
- peak, prominence, and support sentinel toggles/counts;
- Labels: Important / Selected / Displayed / None / All;
- Show diagnostic;
- Open complete interactive tree; and
- Show all.

These three complete-tree controls have distinct semantics:

- **Filter = None** is the persistent filter-state value, serialized as
  `filter.mode = "none"`. It constructs a
  `complete` core for the selected component and remains selected until the
  user chooses another filter.
- **Show all** is a shortcut that sets Filter to None and recomputes the
  proposal. It is not a temporary visual override. In an overflow state it
  therefore produces the declared complete-core overflow presentation.
- **Open complete interactive tree** is a viewer action. Opening it does not
  change filter mode, manual settings, selected canonical IDs, active attempt,
  retained proposal, or static-panel render outcome. The launch itself is
  nonmutating; later explicit branch selections inside the viewer use the
  ordinary linked-selection mechanism.

Each action has the same state-transition semantics in renderable and overflow
states; only the resulting proposal's presentation differs.

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

## Proposal State Model

Validation, measurement, selection, rendering, and visible-view provenance are
separate:

```text
active.attempt.validation:
  identity: current | stale
  source:   valid | source_invalid
  mapping:  valid | mapping_invalid
  ranking.measure:
    trajectory_flow_mass:
      valid | mass_invalid | mass_unavailable
    trajectory_flow_support:
      valid | support_invalid
    source_peak:
      valid | peak_invalid
    canonical_prominence:
      valid | prominence_invalid
  settings: valid | settings_invalid

active.attempt.outcome:
  proposal_created | blocked | stale

active.attempt.render.outcome:
  null | unavailable | stale

proposal.core.selection.outcome:
  strong_gap | coverage | single_positive | coverage_capped |
  minimum_mass | threshold_empty | top_k | complete

proposal.core.warnings:
  tie_overflow

proposal.render.outcome:
  renderable | core_overflow | sentinel_overflow | closure_overflow

view.display.source:
  current | retained_last_valid | none
```

The proposal record also preserves its accepted creation-time validation:

```text
identity: current
source: valid
mapping: valid
ranking.measure:
  trajectory_flow_mass:
    valid | mass_invalid | mass_unavailable
  trajectory_flow_support:
    valid
  source_peak:
    valid
  canonical_prominence:
    valid
settings: valid
```

No proposal exists when blocking state prevents a core. `tie_overflow` never
replaces the mode-specific proposal core outcome.

Active-attempt blocking precedence is:

1. stale identity returns `stale`;
2. invalid source or mapping returns `unavailable`;
3. `support_invalid`, `peak_invalid`, or `prominence_invalid` returns
   `unavailable` for every mode;
4. invalid active settings returns `unavailable`;
5. an active mass-based mode with `mass_invalid` or `mass_unavailable` returns
   `unavailable`;
6. otherwise a current core proceeds through core, sentinel, and closure
   budget checks.

Filter None is canonical-only after identity, source, mapping, non-mass ranking,
and settings validation. It may return core outcome `complete` and a current
render outcome despite separately recorded `mass_invalid` or
`mass_unavailable`; mass-derived views are disabled and disclosed.

In those two Filter None states, the active attempt outcome is
`proposal_created`, its render outcome is null, `display.source` is `current`,
and the embedded immutable proposal has the exact mass-derived field
representations defined above. `complete` is never a filter-state value.

The active-attempt matrix is:

| State | Auto/Cumulative/Minimum/Top K | Filter None |
|---|---|---|
| all validation and measures valid | compute mode-specific core | `complete` |
| mass invalid or unavailable | null core; render `unavailable` | `complete`; disable mass-derived views |
| support invalid | null core; render `unavailable` | same |
| peak invalid | null core; render `unavailable` | same |
| prominence invalid | null core; render `unavailable` | same |
| source invalid | null core; render `unavailable` | null core; render `unavailable` |
| mapping invalid | null core; render `unavailable` | null core; render `unavailable` |
| active settings invalid | null current core; render `unavailable` | same |
| stale identity | null current core; render `stale` | null current core; render `stale` |

This matrix describes only the active attempt. The view-state envelope
separately decides whether a same-context prior immutable proposal is displayed
as `retained_last_valid`. It never copies that proposal's IDs into the blocked
attempt.

## Required Validation

### Measures and identity

1. Exact measure names and owning identities are recorded and displayed.
2. One-to-one extrema mapping rejects missing, duplicate, mixed-direction, and
   mixed-component cases.
3. A fixture with different trajectory-flow and merge-tree mass rankings uses
   only the declared trajectory-flow mass.
4. All four ranking vectors validate across the whole mapped direction before
   component selection.
5. Missing and nonfinite mapped peak values produce `peak_invalid` and
   `source_invalid`.
6. Missing, nonfinite, negative, and fractional support sizes each produce
   `support_invalid`; exact zero and positive whole numbers remain valid.
7. Missing, nonfinite, and negative canonical prominence each produce
   `prominence_invalid`; exact zero and the finite survivor convention remain
   valid.
8. Invalid support, peak, or prominence blocks all modes with no sentinel,
   label, final, or layout IDs and render outcome `unavailable`.
9. Mass-invalid and mass-unavailable behavior retains the declared Filter None
   exception while disabling and disclosing every mass-derived view. Tests
   assert the complete field-level availability table, retained non-mass label
   contributions, mass-label omission reason, complete core/final IDs,
   `proposal_created`, current display source, and render outcome for both
   states.
10. Immutable proposal and view-state serialization round-trip both Filter
    None mass-failure states without changing validation, typed null/empty
    mass fields, ordered IDs, settings, independently recomputed fingerprints,
    or render outcome. Fingerprint tests cover reordered named inputs,
    timestamp-only changes, one-field tampering, wrong-context proposals, and
    corrupted serialized view state. Context/1 and proposal/3 tests assert
    every exact field set and permitted type for valid, mass-invalid, and
    mass-unavailable proposals, and reject missing, additional, mistyped, and
    wrong-version fields with `schema_invalid`.
11. Stale graph, field, source, construction, direction, or component identity
   is rejected by every coordinated panel.
12. Whole-direction mapping and ranking validation precede deterministic
    component selection; positive, all-zero, invalid-mapping, invalid-mass,
    invalid-support, and invalid-prominence cases record the specified
    validation map, totals or their unavailability, rule, and fallback.

### Algorithm

13. Results are deterministic under row permutation.
14. Tests cover a strong gap, several comparable gaps, smooth heavy tail,
   extreme last-value gap, all-equal masses, coverage-boundary ties, and
   budget-boundary ties.
15. The extreme late-gap example selects a bounded core, never rank 199.
16. Tests cover negative, missing, nonfinite, all-zero, one-positive, and
   two-positive mass vectors with exact typed statuses.
17. Exact zeros never enter log calculations and remain available to Filter
    None, sentinels, and closure.
18. Positive-only and all-mass ranking groups are exact, deterministic, and
    separately serialized.
19. Core and final coverage use the same recorded denominator.
20. Pure helpers and UI inputs reject every invalid active parameter boundary
    with
    `settings_invalid`, retain the last valid proposal, and never coerce.
21. Inactive mode-specific inputs cannot block recomputation; first activation,
    switching, retention, and reactivation follow the declared initialization
    and validation rules.
22. Minimum Mass uses raw trajectory-flow units: masses `0.4, 0.3` with
    threshold `0.5` return `threshold_empty`, not a component-normalized
    selection.
23. Masses `0.6, 0.4, 0, 0` with Minimum Mass zero select all four IDs and
    return `minimum_mass`.
24. The same masses with Top K three include the complete zero group, return
    `top_k`, and record warning `tie_overflow`; a non-tied Top K returns
    `top_k` without that warning.
25. Filter None returns `complete`.
26. The full mode-by-validation-state matrix is tested for valid,
    mass-invalid, mass-unavailable, source-invalid, mapping-invalid,
    support-invalid, peak-invalid, prominence-invalid, settings-invalid, and
    stale states.

### View-state transitions

27. An invalid active attempt has input and validation fields but no canonical
    IDs or algorithm proposal.
28. Initial valid computation stores a current immutable proposal and matching
    display fingerprint.
29. A same-context invalid setting retains the prior immutable proposal,
    records `display.source = retained_last_valid`, and keeps the invalid
    attempt separate.
30. An invalid setting without a prior proposal records
    `display.source = none`.
31. A later valid recomputation atomically replaces the retained proposal and
    records `display.source = current`.
32. Source, mapping, ranking-measure, and stale-identity failures clear the
    retained proposal.
33. Graph, field, source, subject, project, construction, direction, and
    component changes clear both active and retained state before recomputation.
34. View-state serialization round-trips the active-attempt fingerprint,
    view-state fingerprint, active inputs and validation, display source,
    display fingerprint, and complete immutable display proposal.
    Deserialization independently recomputes all fingerprints and rejects,
    rather than repairs, every mismatch with `fingerprint_invalid`.
    Independent mutations of validation, attempt outcome, attempt render
    outcome, and display source fail that check; even after deliberate
    re-fingerprinting, every impossible state-matrix combination fails with
    `view_state_invalid`.
35. Active-input status text describes the invalid attempt while displayed
    status text is derived only from the retained proposal.

### Topology and components

36. Complete-tree identity is unchanged by every filter.
37. Unknown, mixed, or nonclosed selections fail in the public `gflow`
    accessor unless ancestor closure is explicitly requested.
38. The public `gflow` accessor rejects nonfinite branch birth, death, or
    persistence and negative persistence, including invalid survivor values.
39. Filtered public layouts preserve exact canonical IDs, parents, events,
    births, deaths, merge levels, persistence, and survivor identity.
40. Filtered leaf order is the complete canonical order restricted to selected
    IDs; filtered x coordinates are deterministic under row permutation.
41. Static and interactive renderers of the same selection use identical
    filtered coordinates; no test equates them to complete-layout x positions.
42. Multiple components have per-component roots, proposals, counts, and
    invalidation.
43. Every final branch exists in the canonical tree; every label refers to a
    final branch.

### Overflow and interaction

44. Sentinel reasons and non-overlapping counts are exact.
45. Every sentinel and Important-label measure tests a nominal Top-N boundary
    straddled by a tie, including expanded count disclosure.
46. Non-tied Auto, Minimum Mass, Top K, and Filter None cores exercise
    `core_overflow`.
47. A tie-expanded Top K exceeding the final budget retains outcome `top_k`,
    warning `tie_overflow`, and render outcome `core_overflow`.
48. Deep, disjoint sentinel ancestry exercises `sentinel_overflow` and
    `closure_overflow`.
49. Mandatory branches are never silently discarded.
50. Filter None persists as serialized filter state `none` while its proposal
    core outcome is `complete`; Show all sets Filter to None and installs the
    same complete immutable proposal; Open complete interactive tree mutates
    none of the declared filter, selection, attempt, or display state.
51. Each of those three UI actions is tested in renderable and overflow states.
52. Desktop and narrow viewports remain usable in ordinary and overflow
    states.
53. Cross-panel selection highlights one canonical basin without
    reconstruction or Inspector-setting changes.

### Portable Subject 15 regression

54. A clean checkout validates all 352 fixture mappings and canonical parents.
55. The fixture validates finite peaks, nonnegative whole-number support sizes,
    and finite nonnegative canonical prominence, including the survivor.
56. The bounded algorithm returns the rank-17 strong-gap core and exact
    recorded coverage.
57. The reference regression asserts exact tie groups, eligible boundaries,
    first qualifying boundary, canonical core/final IDs, and `strong_gap`.
58. Filter None exposes all 352 fixture branches when the full canonical object
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
