# Canonical Basin Complex Implementation Audit Response

Date: 2026-07-28

Responds to:
`dev/audits/basins_canonical_complex_implementation_audit_2026-07-28.md`

Status: all four blockers and all three nonblocking findings addressed

Final repository provenance:

- `gflow`: `af1fc4e53365e421440c0d0ac71a01ad0f91fb52`
- `vaginal_community_trajectory_types` registration:
  `b6eb27d31e1ad88579a7df92eb0bb4345a6b18fb`
- `gflowui`: the commit containing this response; resolve with
  `git log -1 --format=%H -- dev/audits/basins_canonical_complex_implementation_audit_response_2026-07-28.md`

## Finding Dispositions

### I-01 — addressed

External source alignment is now a required, validated input to basin
construction rather than an inferred declaration.

- Registered sources carry graph ID, `k`, graph fingerprint, source ordered
  vertex IDs/fingerprint, displayed ordered-ID fingerprint, contract version,
  and algorithm.
- The selected graph identity is recomputed from the actual adjacency and edge
  lengths. It is not accepted from display metadata alone.
- `gflowui_validate_basin_source_alignment()` performs the comparisons before
  cache lookup or construction and emits typed comparison evidence.
- Missing or mismatched contracts are rejected. Conditional-expectation
  estimates remain displayable without a contract, but cannot be used for a
  canonical basin computation until their upstream producer supplies one.
- `gflow` now stores source identity in attributed upstream attestations.
  Constructor-validated declarations no longer imply that the constructor
  checked external scientific ownership.

The canonical Subject15 registration command is:

```sh
Rscript /Users/pgajer/current_projects/vaginal_community_trajectory_types/analysis/291_register_hmp_subject15_k03_gflowui_project.R
```

Tests cover wrong graph ID, wrong `k`, wrong graph fingerprint, source-ID
permutation, wrong source vertex fingerprint, wrong displayed-ID fingerprint,
wrong field fingerprint, missing conditional-expectation contract, and wrong
conditional-expectation contract.

### I-02 — addressed

There is now one construction-input identity containing project, graph set,
actual graph identity, ordered vertex identity, source and field identity,
typed mass provenance and alignment evidence, fixed reconstruction parameters,
gflow build ID, and runtime ID.

An observer recomputes that identity when the active source field or displayed
graph changes. A mismatch clears the basin result, removes basin rendering,
closes the inspector, and requires recomputation or a matching validated cache
lookup. A real Subject15 `shiny::testServer()` test covers:

- path 4 computation;
- same source-key transition to path 5 and immediate invalidation;
- recomputation with a changed construction identity;
- actual graph-identity change and immediate invalidation; and
- recovery through a matching cache hit.

### I-03 — addressed

The cache key now includes the canonicalized full typed mass-provenance record
and all external alignment evidence. Authority, contract version, algorithm,
evidence, source graph identity, or validation-status changes produce different
keys. Only `status == "ok"` canonical objects are cached, and a failed object
already present in the cache is discarded rather than restored.

Targeted tests cover every provenance component named by the finding and failed
cache-object rejection.

### I-04 — addressed

The gflow ownership ledger now includes `get.gflow.build.identity`, including
its downstream callers. The protected canonical-basin changes were explicitly
adjudicated in:

`/Users/pgajer/current_projects/gflow/split_audit/cleanup/canonical-basin-provenance-authorized-change-2026-07-28.md`

The protected-surface fingerprints were regenerated through the repository
ledger builder, and the maintained public-surface test records the earlier
`compute.tube.lens.corridor` export.

Validation:

- complete gflow source `testthat::test_dir()` suite: passed, with 10 expected
  skips;
- `make audit-cleanup-boundary`: passed, covering 108 exports, 96 S3 methods,
  44 dependency declarations, and 81 native registrations;
- `make check`: tests, examples, vignettes, and manuals passed; one expected
  CRAN incoming-feasibility warning remains because `dgraphs` and `grip` are
  not in mainstream repositories;
- final checked tarball installed successfully under R 4.7;
- installed `summary.basin_complex` S3 dispatch and
  `get.gflow.build.identity()` were verified.

### I-05 — addressed

The benchmark now preserves the source vector, rereads the source asset after
construction, and compares:

- source before versus source after;
- source versus UI-normalized field;
- normalized field versus `without$field$input.values`; and
- normalized field versus `without$field$vertex.mass.input`.

The source asset is exactly unchanged. UI normalization differs by at most
`5.20417042793042e-18`; both constructed vectors are exactly identical to the
normalized input. The corrected report is:

`dev/basins_subject15_benchmark_2026-07-28.md`

### I-06 — addressed

Tests now construct actual Plotly traces and assert the selected maximum-fill
and minimum-halo trace counts, names, vertices, and colors. Actual RGL
null-device rendering is asserted when RGL is installed, with an explicit
skip otherwise.

Production Plotly and RGL renderers consume one shared basin-layer
specification, which prevents selection/color drift between renderer-specific
code paths. Reproducible final-state evidence is saved in:

`dev/basins_renderer_final_state_qa_2026-07-28.md`

For the registered Subject15 project it records:

- source, selected-field, alignment-evidence, build, and runtime identities;
- 6 selected maximum and 6 selected minimum basin keys;
- Plotly: 13 total traces, including 6 maximum fills and 6 minimum halos;
- actual RGL null device: 6 minimum marker layers; and
- no construction, alignment, Plotly-build, or RGL-layer errors.

The evidence generator is `dev/qa_basins_renderer_reference.R`, and its report
contains the exact registration, QA, and source-app start commands.

### I-07 — addressed

The original implementation commit is correctly recorded as
`5567e11f4904c50fb5829ae04f322a408ce571f3`. The remediation commit is
`af1fc4e53365e421440c0d0ac71a01ad0f91fb52`.

## Final Validation Summary

`gflowui`:

- focused occupation-density/alignment/cache tests: passed;
- focused full-server lifecycle tests: passed;
- complete source suite: passed with one pre-existing fixture-dependent
  conditional-expectation skip;
- `R CMD build`: passed;
- `R CMD check --as-cran`: tests passed, no errors, status
  `1 WARNING, 4 NOTEs`;
- renderer evidence script: passed for Plotly and actual RGL;
- corrected reference benchmark: passed.

The package-check warning and notes are existing packaging/readiness items:
non-mainstream dependencies and title/version metadata, non-standard top-level
files, dynamic Shiny static-analysis globals, one long Rd example line, and an
old HTML Tidy installation. No CRAN-readiness claim is made.

Final check logs:

- gflow:
  `/Users/pgajer/current_projects/gflow/gflow.Rcheck/00check.log`
- gflowui:
  `/private/tmp/gflowui-final-check.uIgOJl/gflowui.Rcheck/00check.log`

The source-loaded application is started from the final gflowui source at:
`http://127.0.0.1:3867/`.
