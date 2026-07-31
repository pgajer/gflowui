# Adaptive Initial Filtering: Re-audit Handoff

## Change Summary

Revision 3 of the adaptive initial filtering specification responds to the
2026-07-31 audit and first re-audit. No `gflow` or `gflowui` application
behavior has been implemented in this specification pass.

The revised specification now defines:

- explicit trajectory-flow versus canonical merge-tree measure ownership;
- one-to-one extrema mapping to canonical branch IDs;
- a bounded, tie-group-safe mass-core algorithm;
- one-direction, one-component proposal scope;
- exact invalid, zero, singleton, and unavailable-mass behavior;
- separate core and final rendering budgets;
- typed sentinel and ancestor-closure overflow presentation;
- a required public pure filtered-layout contract in `gflow`;
- a versioned proposal record and construction-scoped settings;
- a portable, provenance-pinned Subject 15 fixture;
- expanded algorithm, topology, serialization, and viewport tests;
- a generic `core_overflow` final-render state for every filter mode;
- deterministic component fallback for invalid or unavailable mass;
- separate canonical vertical values and compressed filtered x-layout; and
- strict parameter domains with noncoercing invalid-setting behavior.

The handoff no longer prescribes audit questions, acceptance criteria, or
verdict wording.

## Repositories and Baselines

### gflowui

Repository:

`/Users/pgajer/current_projects/gflowui`

Baseline before revision 3:

`51998e3776c2ee0e74a747f9b14089d0e36a9da9`

Revised assets:

- `dev/basin_merge_tree_adaptive_initial_filtering_spec_2026-07-31.md`
- `dev/audits/basin_merge_tree_adaptive_initial_filtering_spec_audit_response_2026-07-31.md`
- `dev/audits/basin_merge_tree_adaptive_initial_filtering_spec_audit_response_reaudit_2026-07-31.md`
- `dev/audits/basin_merge_tree_adaptive_initial_filtering_spec_audit_response_reaudit_response_2026-07-31.md`
- `dev/fixtures/derive_subject15_basin_merge_tree_adaptive_fixture.R`
- `tests/testthat/fixtures/basin_merge_tree_subject15_maxima.csv`
- `tests/testthat/fixtures/basin_merge_tree_subject15_maxima_provenance.csv`
- `tests/testthat/test-basin-merge-tree-adaptive-filtering-fixture.R`

### gflow

Repository:

`/Users/pgajer/current_projects/gflow`

Audited baseline:

`24a671c4927df6ab6e5ac10361aecfd87cfaa0cb`

The revised specification requires a new public filtered-layout accessor and
requires `plot.basin.merge.tree()` to consume the same result. This API has not
yet been implemented.

### Scientific repository

Repository:

`/Users/pgajer/current_projects/vaginal_community_trajectory_types`

Pinned evidence revision:

`4615555547f3f406e79436c308d28fd78985b64e`

The local scientific figure builder and unfiltered validation output remain
uncommitted in that repository. They are supporting visual evidence, not
clean-checkout dependencies of the revised core regression.

## Primary Documents

Revised specification:

`/Users/pgajer/current_projects/gflowui/dev/basin_merge_tree_adaptive_initial_filtering_spec_2026-07-31.md`

Original audit:

`/Users/pgajer/current_projects/gflowui/dev/audits/basin_merge_tree_adaptive_initial_filtering_spec_audit_2026-07-31.md`

Audit response:

`/Users/pgajer/current_projects/gflowui/dev/audits/basin_merge_tree_adaptive_initial_filtering_spec_audit_response_2026-07-31.md`

First re-audit:

`/Users/pgajer/current_projects/gflowui/dev/audits/basin_merge_tree_adaptive_initial_filtering_spec_audit_response_reaudit_2026-07-31.md`

First re-audit response:

`/Users/pgajer/current_projects/gflowui/dev/audits/basin_merge_tree_adaptive_initial_filtering_spec_audit_response_reaudit_response_2026-07-31.md`

## Portable Fixture

The clean-checkout fixture contains all 352 Subject 15 maximum branches and
the fields needed by the display proposal:

`/Users/pgajer/current_projects/gflowui/tests/testthat/fixtures/basin_merge_tree_subject15_maxima.csv`

Its provenance record is:

`/Users/pgajer/current_projects/gflowui/tests/testthat/fixtures/basin_merge_tree_subject15_maxima_provenance.csv`

The derivation script is:

`/Users/pgajer/current_projects/gflowui/dev/fixtures/derive_subject15_basin_merge_tree_adaptive_fixture.R`

Pinned source digests:

```text
Export ZIP:
15d575fea00267de49b12192060aeecdd373df6edfdea52cd250d68d2202c275

Topology RDS:
afb7863d761932e31f4f1816f95b496db16fc58028663f26cb036ec6aa1af000
```

The fixture records trajectory-flow mass/support separately from canonical
tree parentage/prominence. Its tests reproduce the 352-branch mapping and raw
rank-17 evidence, then execute the revision-3 bounded reference rule through
tie groups, eligible boundaries, sentinels, closure, and final IDs without the
upstream ZIP.

## Supporting Visual Evidence

Filtered figure:

`/Users/pgajer/current_projects/vaginal_community_trajectory_types/docs/reports/hmp_subject15_superlevel_merge_tree_specification/figures/figure_01_crossing_free_merge_tree_and_barcode.pdf`

Unfiltered figure:

`/Users/pgajer/current_projects/vaginal_community_trajectory_types/docs/reports/hmp_subject15_superlevel_merge_tree_specification/figures/figure_01_crossing_free_merge_tree_and_barcode_unfiltered.pdf`

Validation records:

- `/Users/pgajer/current_projects/vaginal_community_trajectory_types/docs/reports/hmp_subject15_superlevel_merge_tree_specification/figure_validation.csv`
- `/Users/pgajer/current_projects/vaginal_community_trajectory_types/docs/reports/hmp_subject15_superlevel_merge_tree_specification/figure_validation_unfiltered.csv`

These figures demonstrate the readability problem. They do not validate a
universal filtering threshold.

## Reproduction Commands

From `/Users/pgajer/current_projects/gflowui`:

```sh
Rscript dev/fixtures/derive_subject15_basin_merge_tree_adaptive_fixture.R
Rscript -e 'testthat::test_file("tests/testthat/test-basin-merge-tree-adaptive-filtering-fixture.R")'
```

The first command intentionally requires the pinned upstream source assets and
fails on a digest mismatch. The second command uses only committed fixture
assets.

## Known Limits

- The adaptive proposal and UI are not implemented.
- The required public `gflow` filtered-layout API is specified but not
  implemented.
- The Subject 15 fixture validates one empirical component; adversarial
  synthetic tests remain an implementation requirement.
- Minima/sublevel defaults are outside version 1.
- No scientific acceptance of adaptive filtering or EOD interpretation is
  claimed.
