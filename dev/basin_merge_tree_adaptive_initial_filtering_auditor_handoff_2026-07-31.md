# Adaptive Initial Filtering: Re-audit Handoff

## Change Summary

Revision 7 of the adaptive initial filtering specification responds to the
2026-07-31 audit series through the Revision 6 re-audit dated 2026-08-01. No
`gflow` or `gflowui` application behavior has been implemented in this
specification pass.

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
- separate canonical vertical values and compressed filtered x-layout;
- strict parameter domains with noncoercing invalid-setting behavior;
- executable Minimum Mass, Top K, and Filter None contracts;
- distinct positive-mass and all-mass tie groups, including complete zero ties;
- raw trajectory-flow units for Minimum Mass;
- mode-aware initialization, validation, retention, and switching;
- orthogonal identity, source, mapping, mass, settings, core-selection,
  warning, and render state;
- nominal Top-N boundaries with disclosed tie-expanded counts;
- a public-accessor recommendation that does not imply nonexistent S3
  dispatch;
- exact whole-direction validity domains for mass, support, peak, and
  canonical prominence;
- blocking behavior for invalid mandatory ranking vectors;
- a finite component-floor prominence convention for elder-rule survivors;
- immutable successful proposals separated from active-attempt and display
  state;
- deterministic current, retained-last-valid, absent, recovery, and stale
  transitions; and
- distinct persistent-filter, shortcut, and nonmutating-viewer semantics for
  complete-tree controls;
- first-use filtration-value and density-value elder-rule terminology;
- a field-level typed mass-derived contract for canonical-only Filter None
  proposals under invalid or unavailable mass;
- preservation of valid non-mass Important-label contributions in those
  proposals;
- explicit context, proposal, and active-attempt SHA-256 fingerprint
  contracts;
- independently validated proposal/view deserialization with no fingerprint
  repair; and
- exact separation of serialized filter state `none` from core outcome
  `complete`;
- closed context/1, proposal/3, and view-state/1 wire schemas;
- exact field, type, cardinality, ordering, nullability, and schema-evolution
  rules;
- schema-complete valid, mass-invalid, and mass-unavailable reference
  proposals;
- rejection of missing, additional, mistyped, and wrong-version records;
- a view-state fingerprint covering every deterministic envelope field; and
- semantic state-matrix validation after fingerprint validation.

The handoff no longer prescribes audit questions, acceptance criteria, or
verdict wording.

## Repositories and Baselines

### gflowui

Repository:

`/Users/pgajer/current_projects/gflowui`

Baseline before revision 7:

`f9a879027dd359d29b1894d4cf23ea0b850f7784`

Revised assets:

- `dev/basin_merge_tree_adaptive_initial_filtering_spec_2026-07-31.md`
- `dev/audits/basin_merge_tree_adaptive_initial_filtering_spec_audit_response_2026-07-31.md`
- `dev/audits/basin_merge_tree_adaptive_initial_filtering_spec_audit_response_reaudit_2026-07-31.md`
- `dev/audits/basin_merge_tree_adaptive_initial_filtering_spec_audit_response_reaudit_response_2026-07-31.md`
- `dev/audits/basin_merge_tree_adaptive_initial_filtering_spec_second_reaudit_2026-07-31.md`
- `dev/audits/basin_merge_tree_adaptive_initial_filtering_spec_audit_response_followup_reaudit_2026-08-01.md`
- `dev/audits/basin_merge_tree_adaptive_initial_filtering_spec_audit_response_followup_reaudit_response_2026-08-01.md`
- `dev/audits/basin_merge_tree_adaptive_initial_filtering_revision4_reaudit_2026-08-01.md`
- `dev/audits/basin_merge_tree_adaptive_initial_filtering_revision4_reaudit_response_2026-08-01.md`
- `dev/audits/basin_merge_tree_adaptive_initial_filtering_revision5_reaudit_2026-08-01.md`
- `dev/audits/basin_merge_tree_adaptive_initial_filtering_revision5_reaudit_response_2026-08-01.md`
- `dev/audits/basin_merge_tree_adaptive_initial_filtering_revision6_reaudit_2026-08-01.md`
- `dev/audits/basin_merge_tree_adaptive_initial_filtering_revision6_reaudit_response_2026-08-01.md`
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

Related second re-audit:

`/Users/pgajer/current_projects/gflowui/dev/audits/basin_merge_tree_adaptive_initial_filtering_spec_second_reaudit_2026-07-31.md`

Follow-up re-audit:

`/Users/pgajer/current_projects/gflowui/dev/audits/basin_merge_tree_adaptive_initial_filtering_spec_audit_response_followup_reaudit_2026-08-01.md`

Revision 4 response:

`/Users/pgajer/current_projects/gflowui/dev/audits/basin_merge_tree_adaptive_initial_filtering_spec_audit_response_followup_reaudit_response_2026-08-01.md`

Revision 4 re-audit:

`/Users/pgajer/current_projects/gflowui/dev/audits/basin_merge_tree_adaptive_initial_filtering_revision4_reaudit_2026-08-01.md`

Revision 5 response:

`/Users/pgajer/current_projects/gflowui/dev/audits/basin_merge_tree_adaptive_initial_filtering_revision4_reaudit_response_2026-08-01.md`

Revision 5 re-audit:

`/Users/pgajer/current_projects/gflowui/dev/audits/basin_merge_tree_adaptive_initial_filtering_revision5_reaudit_2026-08-01.md`

Revision 6 response:

`/Users/pgajer/current_projects/gflowui/dev/audits/basin_merge_tree_adaptive_initial_filtering_revision5_reaudit_response_2026-08-01.md`

Revision 6 re-audit:

`/Users/pgajer/current_projects/gflowui/dev/audits/basin_merge_tree_adaptive_initial_filtering_revision6_reaudit_2026-08-01.md`

Revision 7 response:

`/Users/pgajer/current_projects/gflowui/dev/audits/basin_merge_tree_adaptive_initial_filtering_revision6_reaudit_response_2026-08-01.md`

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
rank-17 evidence, then execute the revision-6 bounded reference rule through
tie groups, eligible boundaries, sentinels, closure, and final IDs without the
upstream ZIP. Revision 5 retained the exact manual-mode cases and added
whole-direction ranking-domain checks, invalid-ranking blocking, immutable
proposal/view-state transitions, recovery and context invalidation, and
complete-tree control semantics. Revision 6 adds full canonical-only
mass-failure proposal/view round-trips, typed mass fields, non-mass label
retention, deterministic SHA-256 recomputation, tamper rejection, and exact
None/complete control-state separation. Revision 7 replaces the abbreviated
reference record with the complete closed proposal/3 shape, adds strict
context/proposal/view structural validation, fingerprints the complete view
envelope, and rejects both isolated and consistently re-fingerprinted
state-matrix corruption.

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
assets. At Revision 7 it reports:

```text
PASS 424
FAIL 0
WARN 0
SKIP 0
```

## Known Limits

- The adaptive proposal and UI are not implemented.
- The required public `gflow` filtered-layout API is specified but not
  implemented.
- The Subject 15 fixture validates one empirical component; adversarial
  synthetic tests remain an implementation requirement.
- Minima/sublevel defaults are outside version 1.
- No scientific acceptance of adaptive filtering or EOD interpretation is
  claimed.
