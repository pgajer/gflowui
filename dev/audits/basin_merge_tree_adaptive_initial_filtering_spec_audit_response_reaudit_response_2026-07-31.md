# Response to the Adaptive Initial Filtering First Re-audit

## Disposition

Revision 3 of
`dev/basin_merge_tree_adaptive_initial_filtering_spec_2026-07-31.md`
incorporates all four blocking findings and the nonblocking test-evidence
finding from the first re-audit.

This response records changes. It does not claim re-audit acceptance or
implementation completion.

## Finding Responses

### R-01: Generic core overflow

Resolved in **Other filter modes**, **Final Rendering Budget and Overflow**,
**Typed Status Summary**, and **Required Validation**.

- Core status and final render status are independent.
- `core_overflow` applies whenever any complete core exceeds
  `final.render.budget`.
- Cause precedence is core, pre-closure sentinel union, ancestor closure, then
  renderable.
- A core can retain `tie_overflow` while final rendering reports
  `core_overflow`.
- Auto, Cumulative Mass, Minimum Mass, Top K, and None/Show All all pass
  through the same final-budget decision.
- None/Show All opens the complete interactive view rather than attempting a
  compressed static tree when over budget.
- Non-tied and tied core-overflow tests are explicit requirements.

### R-02: Component selection with invalid or unavailable mass

Resolved in **Direction and Component Scope** and **Source and Mass
Validation**.

- Mass validation now occurs across the whole maximum direction before
  automatic component selection.
- Valid positive totals select the greatest component total with stable-ID
  tie breaking.
- All-zero totals select the smallest stable component ID and expose
  `mass_unavailable`.
- Invalid mass disables mass-based component selection, selects the smallest
  stable component ID for deterministic error presentation, and exposes
  `mass_invalid`.
- The proposal records component totals or their unavailability, selection
  rule, tie-break, and fallback reason.

### R-03: Canonical values versus filtered coordinates

Resolved in **Required Public `gflow` Layout Contract** and **Required
Validation**.

- Canonical IDs, parents, events, births, deaths, merge levels, persistence,
  and survivor identity remain exact.
- Filtered leaf order is the complete canonical crossing-free order restricted
  to selected IDs.
- Filtered x positions may be deterministically compressed and reindexed.
- Static and interactive renderers of the same selection must share filtered
  coordinates.
- No test compares filtered x positions with complete-layout x positions.

### R-04: Parameter domains and invalid settings

Resolved by the new **Parameter Validation** section.

- Every scalar parameter now has a type and range.
- Whole-number parameters reject fractional values.
- `core.branch.budget` must not be smaller than
  `minimum.core.branches`.
- `final.render.budget` may be smaller than the core budget; this valid
  configuration can produce `core_overflow`.
- Minimum Mass with no matches returns `threshold_empty`.
- Invalid settings return `settings_invalid` with field-specific messages.
- Pure helpers never clamp or coerce.
- The UI retains the last valid proposal, marks it retained rather than
  current, and does not recompute until settings validate.
- Pure-helper and UI boundary tests are required.

### R-05: Obsolete largest-gap regression claim

Resolved in
`tests/testthat/test-basin-merge-tree-adaptive-filtering-fixture.R`.

The raw `which.max(gap)` assertion remains only in the fixture-integrity test.
A separate reference test now:

- builds ordered exact tie groups;
- computes the deterministic positive-mass denominator;
- asserts `j.coverage = 17` and `j.minimum = 3`;
- asserts eligible boundaries 17 through 50;
- chooses the first qualifying strong gap at rank 17;
- verifies the exact 17 canonical core IDs;
- computes the peak, prominence, support, and survivor sentinels;
- applies canonical parent closure;
- verifies zero sentinel-only and ancestor-only additions;
- verifies the exact final canonical IDs and normalized coverage; and
- returns core status `strong_gap` and final status `renderable`.

The audit response wording now distinguishes raw fixture-integrity evidence
from the bounded reference-algorithm regression.

## Verification

The focused clean-checkout fixture test is the executable verification for this
specification-only revision. The future implementation must still add the
synthetic setting, component, overflow, layout, Shiny, and viewport tests
listed in the specification.
