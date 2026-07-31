# Response to the Adaptive Initial Filtering Specification Audit

## Disposition

All eight blocking findings and both nonblocking findings were incorporated
into the specification. Revision 3 also incorporates the residual findings
from the first re-audit:

`dev/basin_merge_tree_adaptive_initial_filtering_spec_2026-07-31.md`

The first re-audit response is:

`dev/audits/basin_merge_tree_adaptive_initial_filtering_spec_audit_response_reaudit_response_2026-07-31.md`

This response records changes only. It does not claim audit acceptance or
implementation completion.

## Finding Responses

### A-01: Ambiguous basin mass

Resolved by the new **Version 1 Measure Contract** and **Required Mapping**
sections.

- The mass core uses trajectory-flow `primary.support.mass`.
- The support sentinel uses trajectory-flow `primary.support.size`.
- Peak value comes from the selected field and is descending for maxima.
- Prominence comes from the canonical superlevel merge tree.
- Tree-native support mass must be labeled distinctly.
- Mapping by `(direction, extremum.vertex)` must be one-to-one and is
  translated to canonical branch IDs.
- Missing, duplicate, direction-mismatched, or component-mismatched mappings
  block with `mapping_invalid`.
- Measure names and construction identities are proposal-record fields.

### A-02: Unbounded late-tail gap and tie splitting

Resolved by **Tie Groups** and **Automatic Mass-Core Algorithm**.

- Exact equal-mass groups are indivisible.
- `j.coverage` is a complete tie-group endpoint.
- Auto chooses the earliest qualifying strong gap at or after the required
  coverage/minimum boundary.
- Gap search stops at the core budget.
- Coverage beyond budget returns `coverage_capped` without further tail
  search.
- A budget-straddling tie is included whole and returns `tie_overflow`.
- The required adversarial distributions are explicit test cases.

### A-03: No public filtered canonical layout

Resolved normatively by **Required Public `gflow` Layout Contract**.

The specification assigns selection validation, ancestor closure, event
selection, and crossing-free layout to a new public pure `gflow` accessor. It
defines inputs, returned data, rejection behavior, optional closure, and the
requirement that `plot.basin.merge.tree()` consume the same result. Adaptive
policy remains in `gflowui`.

The API is a prerequisite and has not been implemented in this specification
pass.

### A-04: Undefined forest behavior

Resolved by **Direction and Component Scope**.

- Version 1 operates on one maximum/superlevel graph component.
- The component survivor is mandatory.
- Whole-direction and selected-component counts are reported.
- Multi-component inputs expose a selector.
- The deterministic initial component has greatest validated positive mass,
  with stable component-ID tie breaking.
- Direction or component changes invalidate the proposal.

### A-05: Invalid, zero, singleton, and unavailable masses

Resolved by **Source and Mass Validation**, the bounded algorithm, and the
typed-status summary.

- Partial/nonfinite source fields block as `source_invalid`.
- Missing, negative, or nonfinite derived mass blocks mass filters as
  `mass_invalid`.
- Exact zero remains valid, is excluded from logs, and remains available to
  Show All, sentinels, and ancestry.
- No positive mass returns `mass_unavailable`.
- One positive branch returns `single_positive` without a fabricated gap.
- Normalization follows validation.
- Core and final coverage share one recorded positive-mass denominator.

### A-06: Sentinel and closure overload

Resolved by **Mandatory Sentinels** and **Final Rendering Budget and
Overflow**.

- Core, each sentinel-only category, ancestor-only additions, and final union
  have non-overlapping counts.
- All inclusion reasons are retained even though one primary reason is used
  for counts.
- The core and final rendering budgets are distinct.
- Mandatory IDs are never dropped.
- `sentinel_overflow`, `closure_overflow`, and `tie_overflow` have specified
  diagnostic-first presentation and a complete zoomable/scrollable view.
- Deep/disjoint closure and narrow-viewport cases are required tests.

### A-07: Direction, settings scope, and proposal identity

Resolved by **Status**, **Direction and Component Scope**, and **Versioned
Proposal Record**.

- Version 1 is maxima/superlevel only.
- Settings persist only in the active session and construction context.
- The proposal schema records fingerprints, measure identities, tie groups,
  parameters, all branch sets and reasons, coverages, typed statuses, and
  creation time.
- Every coordinated panel rejects a stale proposal.

### A-08: Nonportable Subject 15 provenance

Resolved by adding:

- `tests/testthat/fixtures/basin_merge_tree_subject15_maxima.csv`;
- `tests/testthat/fixtures/basin_merge_tree_subject15_maxima_provenance.csv`;
- `dev/fixtures/derive_subject15_basin_merge_tree_adaptive_fixture.R`; and
- `tests/testthat/test-basin-merge-tree-adaptive-filtering-fixture.R`.

The fixture contains all 352 maximum branches, trajectory/canonical IDs,
canonical parents, mass, support, peak, prominence, direction, component, and
survivor status. Provenance pins upstream revisions, source digests,
construction fingerprints, and measure names. The regression no longer
depends on the local ZIP.

### A-09: Overstated independence

Corrected. The specification now calls the peak-scale agreement “a
corroborating numerical-floor check on the same selected field” and expressly
rejects its use as independent scientific validation or universal tuning.

### A-10: Prescriptive auditor handoff

Corrected. The replacement handoff contains only change summary, repository
facts, evidence, reproduction commands, and known limitations. It does not
prescribe questions, acceptance criteria, or verdicts.

## Verification Performed in This Revision

- Source ZIP and topology RDS SHA-256 values were checked.
- The portable fixture was regenerated from those pinned inputs.
- The one-to-one 352-branch trajectory/canonical mapping and canonical
  parentage were validated during derivation.
- Fixture-integrity assertions reproduce the raw Subject 15 mass and gap
  values and distinguish raw retained mass from denominator-normalized
  coverage.
- A separate clean-checkout reference test executes the bounded rule and
  asserts exact tie groups, `j.coverage`, `j.minimum`, eligible boundaries,
  the first qualifying boundary, canonical core/final IDs, and
  `strong_gap`.

Implementation-level synthetic, public-API, serialization, Shiny, and viewport
tests remain future work because the feature itself has not been implemented.
