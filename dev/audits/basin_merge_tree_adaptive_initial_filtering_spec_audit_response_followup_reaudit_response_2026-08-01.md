# Response to the Adaptive Initial Filtering Follow-up Re-audit

## Disposition

Revision 4 of
`dev/basin_merge_tree_adaptive_initial_filtering_spec_2026-07-31.md`
addresses FR-01 through FR-04 from the 2026-08-01 follow-up re-audit and the
corresponding S2-01 through S2-03 findings from the related second re-audit.

This response records specification and reference-test changes. It does not
claim auditor acceptance or implementation completion.

## Finding Responses

### FR-01: Complete manual-mode contract

Addressed in **Parameter Validation**, **Tie Groups**, **Configurable
defaults**, **Other filter modes**, **User Interface**, **Proposal State
Model**, and **Required Validation**.

- Top K is initialized on first activation to
  `min(10, selected component branch count)`.
- Minimum Mass is initialized on first activation to zero.
- Top K and Minimum Mass controls are conditional on their active modes.
- Inactive mode-specific values are retained but not validated and cannot
  block another active mode. Reactivation restores and validates the retained
  value before recomputation.
- Minimum Mass is explicitly measured in raw validated trajectory-flow
  `primary.support.mass`, not selected-component normalized share.
- Positive-mass groups are reserved for Auto, Cumulative Mass, logarithms,
  gaps, and coverage. All-mass ranking groups drive Top K and Minimum Mass and
  include one complete zero-mass group.
- Minimum Mass zero therefore includes every branch in the selected component.
- A Top K boundary entering a tie, including the zero-mass group, includes the
  complete tie and records additive warning `tie_overflow`.
- Ordinary successful core outcomes are `minimum_mass`, `top_k`, and
  `complete`; `threshold_empty` remains the valid empty Minimum Mass outcome.

The focused reference test now checks the exact requested examples:

- masses `0.6, 0.4, 0, 0` with Top K three return all four IDs with outcome
  `top_k` and warning `tie_overflow`;
- the same vector with Minimum Mass zero returns all four IDs with outcome
  `minimum_mass`; and
- raw masses `0.4, 0.3` with Minimum Mass `0.5` return
  `threshold_empty`, rather than selecting `0.4` after normalization.

It also exercises first activation, initialization, cross-mode retention,
reactivation, inactive-invalid input tolerance, and active-invalid rejection.

### FR-02: Orthogonal proposal state

Addressed in **Versioned Proposal Record**, **Proposal State Model**, and
**Required Validation**.

The serialized proposal schema is now
`gflowui_basin_merge_tree_display_proposal/2`. It records independent fields
for:

- identity validation;
- source validation;
- mapping validation;
- mass validation;
- settings validation;
- proposal availability;
- core selection outcome;
- additive core warnings; and
- final render outcome.

Blocking-state precedence is explicit:

1. stale identity returns render outcome `stale` and cached IDs are not
   rendered;
2. invalid source, mapping, or active settings returns `unavailable`;
3. invalid or unavailable mass returns `unavailable` for an active mass-based
   mode; and
4. otherwise the current core proceeds through the rendering budgets.

None/Show All remains a canonical-only fallback. With valid identity, source,
mapping, and settings, it can return core outcome `complete` and a current
render outcome while preserving a separate `mass_invalid` or
`mass_unavailable` validation state and disclosure. Mass-derived annotations
and coverage remain
unavailable in that state.

The focused reference test covers every mass-based mode and None/Show All
under valid, mass-invalid, mass-unavailable, source-invalid, mapping-invalid,
settings-invalid, and stale conditions.

### FR-03: Nominal Top-N boundaries

Addressed in **Parameter Validation**, **Mandatory Sentinels**, **Label
Policy**, and **Required Validation**.

Sentinel and label N values are now nominal rank boundaries rather than hard
upper counts. Fewer than N eligible branches returns all available branches.
A boundary tie is included completely and may expand the result beyond N.
Every affected record exposes both requested N and the tie-expanded count.
The validation contract requires a straddling-tie test for every sentinel and
Important-label measure.

### FR-04: Public layout accessor naming

Addressed in **Required Public `gflow` Layout Contract**.

The proposed accessor is now `get.basin.merge.tree.layout()`. This ordinary
function name exposes the required pure layout operation without suggesting
that `graphics::layout()` provides S3 dispatch.

## Verification

From `/Users/pgajer/current_projects/gflowui`:

```sh
Rscript -e \
  'testthat::test_file("tests/testthat/test-basin-merge-tree-adaptive-filtering-fixture.R")'
```

Result:

```text
PASS 141
FAIL 0
WARN 0
SKIP 0
```

`git diff --check` also passes.

The application feature and required public `gflow` layout accessor remain
future implementation work.
