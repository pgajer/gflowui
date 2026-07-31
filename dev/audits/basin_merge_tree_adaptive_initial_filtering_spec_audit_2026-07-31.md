# Adaptive Initial Filtering for Basin Merge Trees: Specification Audit

Date: 2026-07-31

Auditor role: independent specification auditor

Audited specification:
`/Users/pgajer/current_projects/gflowui/dev/basin_merge_tree_adaptive_initial_filtering_spec_2026-07-31.md`

Auditor handoff:
`/Users/pgajer/current_projects/gflowui/dev/basin_merge_tree_adaptive_initial_filtering_auditor_handoff_2026-07-31.md`

## Verdict

**Revise before acceptance; the specification is not ready for
implementation.**

The central design decision is sound: compute and retain the complete canonical
merge tree, then filter only its initial presentation. The Subject 15 evidence
also reproduces exactly, and visual inspection confirms that its 17-branch
figure is readable whereas its 352-branch static figure is not.

The current text nevertheless leaves essential semantics unresolved. In
particular, `gflowui` and `gflow` currently expose two materially different
basin-mass measures; the proposed largest-gap rule can select an irrelevant
gap at the end of a negligible tail and bypass its 50-branch cap; and the
public `gflow` API cannot produce a filtered canonical layout without new API
work. Forests, ties, invalid values, sentinel overflow, and proposal identity
also lack implementable contracts. These are not matters that should be left
to UI implementation judgment.

No scientific acceptance of a filtering strategy or EOD interpretation is
made here. The audited rule is a display heuristic, not an estimator, test, or
scientific selection procedure.

## Audit Charter Coverage

The worker-auditor charter was applied from data outward:

- **Data-generating process:** no synthetic data-generating process, train/test
  split, or model comparison is claimed. Subject 15 is an existing field and
  was treated as one empirical display case, not population validation.
- **Measurement:** the headline mass values were recomputed from row-level ZIP
  contents. The trajectory-flow and merge-tree measures were compared rather
  than assumed equivalent.
- **Estimation and selection:** the proposed display-selection rule was
  falsified with late-gap, heavy-tail, and tied-mass distributions. It is not a
  fair method-comparison or scientific-selection protocol.
- **Statistical inference:** none is performed. The specification must not
  imply inferential or universal support from one subject or from two checks
  on the same field.
- **Artifacts and provenance:** source revisions, dirty/untracked artifacts,
  the Subject 15 digest, and clean-checkout reproducibility were checked.
- **Implementation correctness:** current `gflow`/`gflowui` objects, mass
  ownership, public API boundaries, forest behavior, state identity, and edge
  cases were inspected.
- **Rendering fidelity:** filtered and unfiltered PDFs were rendered and
  visually inspected only after the preceding checks.

## Blocking Findings

### A-01 — BLOCKER — “Basin mass” does not identify a unique source or measure

Affected sections: **Subject 15 Evidence**, **Recommended Diagnostic**,
**Provisional Automatic Proposal**, **Mandatory Sentinel Union**, and
**Coordination with Other Basin Panels**.

`gflowui` currently combines two basin constructions:

- the trajectory-flow complex used by `all_table`, the Inspector, and the
  pair-plot mass and support rankings; and
- the superlevel merge-tree complex used for exact prominence and the
  canonical merge tree.

`gflowui_basin_table()` begins with the trajectory-flow basin table and maps
only persistence from the merge-tree complex by `(type, extremum.vertex)`.
Conversely, `gflow::plot.basin.merge.tree()` defaults to the merge-tree
object's own `primary.support.mass`. These measures are not interchangeable.

For the audited Subject 15 artifact, the 352 maxima align one-to-one by
extremum, and both mass vectors sum to one, but:

```text
maximum absolute per-basin mass difference: 0.158873
Spearman mass-rank correlation:              0.797341
trajectory-flow gap after rank 17:           12.939763 decades
merge-tree gap after rank 17:                12.138371 decades
```

Subject 15 happens to place both boundaries after rank 17, but that agreement
does not establish equivalence on another field. The same ambiguity applies
to the support sentinel.

Risk: two conforming implementations could choose different branches while
reporting the same “Auto by mass distribution” rule. The tree could also show
one mass annotation while filtering on another measure.

Required correction:

1. Name every ranking measure and its owning object explicitly.
2. For the current coordinated `gflowui` design, define the v1 proposal as
   trajectory-flow `primary.support.mass` and the support sentinel as
   trajectory-flow `primary.support.size`; define prominence as the canonical
   merge-tree persistence/prominence value; and define peak value from the
   selected construction field with direction-specific ordering.
3. Require a one-to-one mapping from the trajectory-flow basin to the
   merge-tree branch by `(direction, extremum.vertex)`, then translate the
   result to the canonical merge-tree basin ID. Missing, duplicate, or
   direction-inconsistent mappings must block the proposal rather than fall
   back silently.
4. Label tree-native and trajectory-flow measures distinctly wherever both
   are exposed. Do not inherit `plot.basin.merge.tree()`'s default mass measure
   without an explicit override.
5. Record the exact measure names, construction identities, and mapping
   validation in the proposal record and visible details.

This choice preserves the audited Subject 15 evidence and makes the tree,
Inspector, and pair-plot mass rankings refer to the same UI concept. If the
project instead chooses merge-tree-native mass, the specification and Subject
15 acceptance values must be regenerated and the Inspector distinction made
explicit.

### A-02 — BLOCKER — The gap rule can select an irrelevant late-tail gap and evade the cap

Affected sections: **Gap proposal**, **Fallback proposal**, and **Required
Validation**.

The rule searches *all* boundaries after 0.99 cumulative mass and chooses the
largest gap. The 50-branch cap applies only when no strong gap is found. A
negligible terminal value can therefore force a very large displayed core.

An exact synthetic counterexample is:

```r
c(0.33, 0.33, 0.331, rep(0.009 / 196, 196), 1e-100)
```

The first three values already contain 99.1% of the mass, but the specified
rule selects the 95.66-decade gap after rank 199 and displays 199 branches.
This result satisfies the written rule and bypasses the fallback cap.

The fallback is also undefined at ties. With 100 equal masses, 99 branches are
needed to reach 0.99 and the cap arbitrarily splits an equal-mass group at 50.
The chosen set then depends on the basin-ID tie breaker despite no mass
evidence distinguishing its members.

Risk: the default can be unreadable on precisely the heavy-tail cases it is
intended to control, and equal evidence can yield unequal visibility.

Required correction:

1. Sort stable equal-mass tie groups, not individual rows. Candidate
   boundaries may occur only after a complete tie group.
2. Let `j.coverage` be the first tie-group endpoint reaching the coverage
   target.
3. Choose one bounded rule and specify it exactly. The recommended v1 rule is
   the earliest strong gap at or after `j.coverage`, subject to the initial
   branch budget. A simpler defensible alternative is to test only the gap
   immediately after `j.coverage`.
4. Never accept a gap boundary beyond the initial branch budget. The cap must
   constrain both the gap and coverage paths, not only fallback.
5. If the coverage boundary exceeds the budget, return a typed
   `coverage_capped` status with achieved core coverage; do not search farther
   into the tail for a qualifying gap.
6. If a tie group straddles the budget, never split it. Either include the
   complete group and report `tie_overflow`, or decline an automatic static
   tree proposal when the resulting view would exceed the rendering budget.
7. Add adversarial tests for an extreme last-bin gap, several comparable
   gaps, a smooth heavy tail, all-equal masses, a tie at the coverage boundary,
   and a tie at the branch budget.

The numerical cutoff may remain informational, but the authoritative result
must be the recorded tie-group boundary and canonical basin-ID set.

### A-03 — BLOCKER — The required filtered canonical layout has no public `gflow` API

Affected sections: **Scientific and Display Invariants**, **Mandatory Sentinel
Union**, **Proposed User Interface**, and **Coordination with Other Basin
Panels**.

The ownership invariant correctly forbids `gflowui` from reconstructing a
visually similar topology. The present public API cannot satisfy that
invariant for a branch subset:

- `get.basin.merge.tree()` returns the complete canonical object;
- `plot.basin.merge.tree()` selects a direction and graph component but has no
  canonical branch-ID subset argument; and
- the crossing-free layout routine is private and lays out every branch in the
  selected component.

Although `plot.basin.merge.tree()` invisibly returns its complete layout after
drawing, this does not provide a pure, filtered plot-data API suitable for an
interactive Shiny panel. Calling the private layout from `gflowui` would create
an unsupported cross-package dependency; copying it would violate the stated
ownership invariant.

Risk: implementation would either render the full unreadable tree, duplicate
canonical topology/layout logic in `gflowui`, or rely on a private `gflow`
function.

Required correction:

- Assign canonical branch selection validation, ancestor closure, event
  selection, and crossing-free layout to `gflow`.
- Add a reviewed public pure accessor, or an equivalent public filtered-layout
  method, that accepts canonical basin IDs plus direction and component and
  returns the exact selected branch table, selected merge events, closure
  additions, layout, and coordinates without drawing.
- Require that it reject unknown IDs, mixed directions/components, missing
  roots, and non-closed selections unless an explicit `close.ancestors = TRUE`
  policy is requested and reported.
- Make the plotting method consume the same public layout result so static and
  interactive renderers cannot drift.
- Keep adaptive proposal policy, controls, disclosure, linked selection, and
  session state in `gflowui`; keep Subject 15 evidence and EOD interpretation
  in the upstream scientific project.

This is presentation filtering of a complete canonical tree. It must not
construct an induced graph or recompute a basin complex.

### A-04 — BLOCKER — Merge forests and “global” roots are undefined

Affected sections: **Scientific and Display Invariants**, **Mandatory Sentinel
Union**, **Label Policy**, and **Proposed User Interface**.

The specification refers to “the global elder-rule survivor,” “the root,” and
one complete tree. `gflow` explicitly represents disconnected inputs as a
merge forest and requires a component for plotting when more than one is
available. Each component has its own survivor/root.

Risk: a literal global top-10 or single-root rule can hide an entire graph
component, produce a non-closed branch set, or pass an invalid forest to the
plotter.

Required correction:

- Scope v1 to one selected `(direction, graph component)` at a time.
- Compute the mass proposal, sentinels, closure, coverage, and labels within
  that component; its elder-rule survivor is mandatory.
- Report both whole-direction counts and selected-component counts.
- Provide a component selector when the direction is a forest, and specify
  deterministic initial-component selection or require an explicit user
  choice.
- Invalidate the proposal when direction or component changes.

An alternative forest-wide UI is possible, but then every component root must
be mandatory and each component must have a separate panel/tab and explicit
per-component disclosure. A single-root contract is not valid for a forest.

### A-05 — BLOCKER — Invalid, zero, singleton, and unavailable mass behavior is not a contract

Affected sections: **Recommended Diagnostic**, **Provisional Automatic
Proposal**, and **Required Validation**.

The diagnostic says to count finite nonnegative masses and disclose zero,
missing, negative, or nonfinite values. The validation section says invalid
values should “fail or disclose according to contract,” but no choice is made.
The formulas silently discard all nonpositive values.

Risk: implementations can silently change the denominator, propose a cutoff
from partial evidence, or fail differently for the same canonical object.

Required correction:

- Preserve the existing basin-construction source invariant: every eligible
  source must provide one finite value for every graph vertex. A missing or
  nonfinite source-field value is a blocking source error; adaptive filtering
  must not create an induced subgraph or partial-field workaround.
- For the derived ranking measure, treat missing, negative, or nonfinite basin
  masses as `mass_invalid` and make Auto/Cumulative/Minimum-mass proposals
  unavailable. Keep `None`/Show All available for a valid canonical tree and
  show the error prominently.
- Treat exact zero as valid but exclude it from logarithms and adjacent-gap
  calculations. Report zero counts separately and retain zero-mass branches
  under Show All or mandatory sentinel/ancestor rules.
- Define `mass_unavailable` when total positive mass is zero.
- Define one-positive-mass and no-gap behavior explicitly; the coverage core,
  not a fabricated gap, is the only possible mass proposal.
- Normalize only after validating the declared measure, and report both the
  mass-core coverage and final displayed-set coverage.
- Add exact tests for each status and prove no denominator is silently
  changed.

### A-06 — BLOCKER — Mandatory sentinels and ancestor closure have no overload policy

Affected sections: **Fallback proposal**, **Mandatory Sentinel Union**,
**Label Policy**, **Proposed User Interface**, and **Required Validation**.

The text permits sentinels and their ancestors to exceed the 50-branch cap.
That is scientifically safer than silently dropping them, but it does not
guarantee a usable initial view. Ten branches from each of three rankings can
have long, mostly disjoint ancestor paths; the final union can contain hundreds
of branches even when the mass core is small.

Risk: the specification promises a readable default but mandates a branch set
that can reproduce the unreadable all-branch view. The implementation has no
defined response when scientific disclosure and rendering capacity conflict.

Required correction:

- Distinguish and report non-overlapping counts for mass core, sentinel-only
  additions by reason, ancestor-only additions, and final union.
- Define a final rendering budget separately from the mass-core budget.
- Never silently discard a mandatory branch. If closure exceeds the final
  budget, return a typed `sentinel_overflow` or `closure_overflow` state and
  use a specified usable presentation, such as component tabs plus zoom/scroll
  or a diagnostic summary that asks the user to open the complete interactive
  view.
- Require viewport tests with deep trees and largely disjoint sentinel paths,
  not only Subject 15 where the sentinel union adds zero branches.

### A-07 — BLOCKER — Direction, settings scope, and proposal identity remain underspecified

Affected sections: **Mandatory Sentinel Union**, **Label Policy**, **Proposed
User Interface**, **Questions Requiring Audit**, and **Required Validation**.

The document is motivated by a superlevel tree of maximum basins but asks
whether minima should use the same policy. “Top extremum value” reverses
meaning between maxima and minima. Settings persistence is also left open, and
the specification has no serializable proposal/view record.

Risk: maximum and minimum branches can be ranked in the wrong direction;
settings can leak across fields or projects; and a cached proposal cannot be
proven to belong to the displayed tree.

Required correction:

1. Scope v1 to `direction = "max"` for occupation-density superlevel trees.
   Defer minima/sublevel defaults until separately specified and tested.
2. Keep adjusted settings session- and construction-scoped by default. Do not
   carry them across fields, subjects, or projects unless the user explicitly
   opts in.
3. Define a versioned proposal record containing at least:
   construction/tree fingerprint, direction, component, measure names and
   source identities, stable ordered tie groups, parameter values, algorithm
   version, core IDs, sentinel IDs with inclusion reasons, ancestor additions,
   final IDs, label IDs, core and final coverage, typed status/warnings, and
   creation time.
4. Require all coordinated panels to validate the proposal identity against
   their active graph, field, source, construction, direction, and component.

### A-08 — BLOCKER — The required Subject 15 acceptance fixture is not portable provenance

Affected sections: **Subject 15 Evidence** and **Required Validation**.

The acceptance numbers are reproducible from the local ZIP, but the audited
specification, handoff, ZIP, and unfiltered validation artifact are currently
untracked in their respective repositories. The ZIP therefore has no durable
repository identity. Its independently measured digest is:

```text
SHA-256 15d575fea00267de49b12192060aeecdd373df6edfdea52cd250d68d2202c275
```

The scientific figure builder also has uncommitted support for `--no-filter`.
An implementation test that depends on these local paths cannot run on a clean
checkout or establish which evidence revision was accepted.

Risk: the required rank-17 regression can pass locally while being absent or
irreproducible in CI and future audits.

Required correction:

- Commit a compact, non-sensitive derived fixture sufficient to reproduce the
  display proposal: stable extrema/branch mapping, parent IDs, mass, support,
  peak value, prominence, direction/component, and source digest; or arrange a
  versioned fixture download with integrity verification.
- Record the upstream repository revision, construction fingerprint, ZIP
  digest, derivation script, and exact measure contract.
- Keep the full 352-branch Show All and canonical-value tests where the full
  artifact is available, but make the core algorithm regression runnable from
  a clean checkout.

## Nonblocking Findings

### A-09 — MAJOR — “Independent peak-scale evidence” overstates the corroboration

Affected section: **Subject 15 Evidence**.

The figure builder retains branches whose peak birth exceeds
`global.birth * sqrt(.Machine$double.eps)`. For Subject 15 this threshold is
`2.01027093840957e-10`; it retains 17 branches, with minimum retained birth
`0.00370553122708482` and maximum excluded birth
`1.49521469771946e-16`.

This is useful corroborating numerical-scale evidence, but it is derived from
the same field and is not an independent scientific criterion or validation
sample. Replace “independent peak-scale evidence” with “a corroborating
numerical-floor check on the same field,” and do not use the agreement to tune
or validate universal defaults.

Implementation blocker: **No**, provided the wording is corrected before the
specification is treated as scientific evidence.

### A-10 — PROCESS — The handoff improperly prescribes audit questions and verdicts

The worker-auditor workflow requires the auditor to derive scope, questions,
acceptance criteria, and verdict independently. The handoff includes **Claims
to Verify**, **Thresholds Requiring Judgment**, **Acceptance-Test Review**, and
a constrained verdict list. Those sections are auditee-supplied audit
instructions rather than neutral implementation facts.

This audit remains valid because those prompts were not used as its charter;
the specification, code, artifacts, adversarial cases, rendering, and package
boundaries were reviewed independently. Future handoffs should contain only
the change summary, repository/revision facts, evidence locations, known
limitations, and reproduction commands.

Implementation blocker: **No** for this audit; fix the handoff template for
future worker-auditor cycles.

## Independently Confirmed Evidence

### Subject 15 numerical reproduction

Using the maximum-basin rows of `basin_characteristics.csv` from the audited
ZIP produced:

| Quantity | Independent result |
|---|---:|
| Maximum basins | 352 |
| Total trajectory-flow maximum-basin mass | 1.0000000000000087 |
| Rank-17 mass | 0.0122134243817115 |
| Rank-18 mass | 1.4030537791339202e-15 |
| Gap after rank 17 | 12.9397631299771 decades |
| Geometric midpoint | 4.13957621441213e-09 |
| Rank-1:17 mass fraction | 0.99999999999992595 |
| Rank-18:352 mass fraction | 8.7287312102973072e-14 |
| Mass at least 0.05 | 6 basins; 0.748420727643718 |
| Mass at least 0.02 | 12 basins; 0.92822230720332 |
| Mass at least 0.01 | 17 basins; 0.999999999999926 |

For this field, the union of the 17-branch trajectory-flow mass core with the
root and the top-10 peak, prominence, and support sets remains 17 branches.
Ancestor closure also remains 17. Thus the proposed Subject 15 status line's
zero sentinel and ancestor additions is correct for the stated measures.

### Rendering review

Both reviewed PDFs were rendered to PNG and inspected, rather than assessed
from text extraction alone:

- the filtered 17-branch tree and barcode are readable at the reviewed page
  size; and
- the unfiltered 352-branch figure has severe label, top-annotation, branch,
  and barcode compression and is not a defensible default static view.

This supports adaptive initial presentation. It does not validate a universal
cutoff rule.

### Adversarial distribution checks

The written rule was also exercised on distributions not resembling Subject
15:

| Distribution | Result under current text | Audit implication |
|---|---|---|
| 200 values with 99.1% in first 3 and one `1e-100` tail value | selects rank 199 | largest eligible tail gap defeats readability and bypasses cap |
| `m_i = 1/i`, 400 values | no strong gap; 99% needs 375; cap 50 retains 0.684818 | truthful typed cap status is essential |
| 100 equal masses | 99% needs 99; cap splits the tie at 50 and retains 0.5 | tie groups must be indivisible |

### Focused repository checks

Pinned source revisions during audit:

```text
gflowui: 925ed84bb6d4ab70efc0b7ebf5bc21979ee0c670
gflow:   24a671c4927df6ab6e5ac10361aecfd87cfaa0cb
upstream scientific repository:
         4615555547f3f406e79436c308d28fd78985b64e
```

The repositories contained pre-existing/untracked work as described above;
no claim is made that these hashes alone contain every audited artifact.

Checks performed:

- focused `gflow` public merge-tree tests: passed;
- focused `gflowui` occupation-density tests: passed;
- `gflow` `make audit-cleanup-boundary`: passed, covering 111 exports, 99 S3
  methods, 44 dependency declarations, and 81 native registrations; and
- Markdown whitespace validation for the specification and handoff: passed.

These are baseline/API checks, not implementation acceptance tests: the
adaptive filtering implementation does not yet exist.

## Threshold and UI Recommendations

After the blocking algorithm corrections:

- **0.99 mass coverage:** reasonable as a configurable conservative display
  default, not a scientific adequacy threshold.
- **3-decade gap:** reasonable as a configurable strong-separation heuristic,
  provided it is evaluated only at bounded, tie-safe candidate boundaries.
- **Minimum core of 3:** acceptable as a provisional display preference, but
  singleton and two-branch components must not be padded or mishandled.
- **50 branches:** treat as a render-mode/viewport budget, not a scientific
  threshold. It may need different desktop and narrow-viewport defaults.
- **Top 10 sentinels and top 6 labels:** reasonable provisional UI defaults,
  but configurable and never described as complete protection against all
  important low-mass branches.
- **Histogram:** use finite positive masses only on the log scale, report zero
  counts separately, use Freedman-Diaconis bins with a deterministic fallback
  for constant or small samples, and never use the histogram to select the
  boundary.
- **Geometric midpoint:** show only as informational detail. The tie-group
  boundary and basin IDs are authoritative.
- **Settings:** persist within the active construction/session by default;
  require explicit opt-in for cross-field, cross-subject, or cross-project
  reuse.
- **Direction:** implement maxima/superlevel occupation-density trees first;
  defer minima until a direction-specific policy is reviewed.

## Required Test Additions

The original validation list should remain, with the following additions or
clarifications:

1. exact measure-source and extrema-mapping validation;
2. different trajectory-flow and merge-tree mass rankings;
3. extreme late-tail gaps and gap candidates beyond the core budget;
4. equal-mass groups at coverage and budget boundaries;
5. negative, missing, nonfinite, all-zero, one-positive, and two-positive
   mass vectors with exact typed statuses;
6. one finite source value per graph vertex, with partial/nonfinite fields
   rejected rather than converted to induced subgraphs;
7. multiple components, per-component roots, and direction/component changes;
8. deep, disjoint sentinel ancestor paths that exceed the final view budget;
9. round-trip serialization and stale rejection of the proposal record;
10. public `gflow` filtered-layout equivalence to the same branches/events in
    the complete canonical tree;
11. a clean-checkout Subject 15 fixture with verified provenance; and
12. desktop and narrow-viewport usability checks for both ordinary and
    overflow states.

## Conditions for Re-audit

A revised specification is ready for re-audit when it:

1. chooses exact measure ownership and mapping semantics;
2. replaces the unbounded largest-eligible-gap rule with a bounded tie-safe
   rule;
3. defines the required public `gflow` filtered-layout contract;
4. scopes proposals to direction and component;
5. specifies invalid/zero/singleton mass statuses and preserves the
   full-field construction precondition;
6. defines sentinel/closure overflow behavior;
7. provides a versioned proposal identity and settings scope; and
8. makes the Subject 15 regression portable and provenance-pinned.

Until those corrections are incorporated, implementation would necessarily
invent scientific/display semantics that the specification is supposed to
settle.
