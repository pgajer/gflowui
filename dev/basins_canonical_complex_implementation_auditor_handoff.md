# Canonical Basin Complex and Basin Inspector Implementation Handoff

Status: Implementation and implementation-audit remediation complete; final
validation and repository provenance are recorded below

Role: Implementation owner

Repositories:

- `/Users/pgajer/current_projects/gflow`
- `/Users/pgajer/current_projects/gflowui`

Implementation date: 2026-07-28

Repository provenance:

- `gflow` branch: `main`
- `gflow` implementation and remediation commit:
  `af1fc4e53365e421440c0d0ac71a01ad0f91fb52`
- `gflowui` branch: `main`
- `gflowui` implementation commit: the commit containing this handoff; resolve
  it without a recursive self-reference using
  `git log -1 --format=%H -- dev/basins_canonical_complex_implementation_auditor_handoff.md`
- Canonical Subject15 registration commit:
  `b6eb27d95f6782d511acbcb662ff39b6a102dbc3`
- Pre-implementation baselines: `gflow`
  `92a61c086f2fa1fa77223edfb02b74a1be3f1a28`; `gflowui`
  `a74da1f6eb38d74b23c374a677c42190dc86c91b`

Plan baseline:
`/Users/pgajer/current_projects/gflowui/dev/basins_canonical_complex_modification_plan.md`

Final plan re-audit:
`/Users/pgajer/current_projects/gflowui/dev/audits/basins_canonical_complex_plan_final_reaudit_2026-07-28.md`

Final re-audit response:
`/Users/pgajer/current_projects/gflowui/dev/audits/basins_canonical_complex_plan_final_reaudit_response_2026-07-28.md`

Implementation audit response:
`/Users/pgajer/current_projects/gflowui/dev/audits/basins_canonical_complex_implementation_audit_response_2026-07-28.md`

## Goal

Replace the direction-specific UI reconstruction with one canonical
`gflow::create.basin.complex()` object constructed for both directions under
fixed CLOSEST/exact-plateau settings. Add a generic, provenance-aware summary
API and a bottom Basin Inspector that independently filters, selects, colors,
and renders maximum and minimum basins.

## Work Completed

### `gflow`

- Appended `vertex.id` and `vertex.mass.provenance` after the complete legacy
  constructor signature. The implementation retains internal integer indices
  and adds external-ID companion fields.
- Added layered mass/source provenance with constructor-computed,
  constructor-validated, and upstream-attested scopes.
- Added a complete code-input manifest and exported build/runtime identity.
- Extended `summary.basin_complex()` with independent maximum/minimum Top-K
  filters, direction-specific ranking availability, explicit ranking errors,
  zero-Top-K support, and column definitions.
- Implemented the `auto` hierarchy as primary support mass, current
  membership-allocated mass, retained coverage mass, raw coverage mass, then
  the corresponding support-size measures.
- Preserved legacy positional constructor compatibility and added regression
  coverage for the full unnamed call.
- Regenerated roxygen-derived `NAMESPACE` and Rd files. Rd files remain
  generated build artifacts because this repository ignores `man/`; their
  canonical sources are the roxygen blocks in `R/`.

### `gflowui`

- Removed `Flow direction` and the precomputed-maxima shortcut.
- Added the simplified Basins sidebar: estimate source, independent maximum and
  minimum Top-K values, ranking measure, compute action, inspector action, and
  construction details.
- Requires a finite full-domain field plus a source-side graph/vertex contract.
  It compares graph ID, `k`, actual graph fingerprint, ordered source vertex
  IDs/fingerprint, displayed ordered-ID fingerprint, and selected-field
  fingerprint before canonical construction or cache access.
- Uses one both-direction `gflow::create.basin.complex()` result with fixed
  CLOSEST, connected exact plateaus, all admissible edges, and no stored
  trajectories.
- Keys the cache by the complete construction-input identity, including actual
  graph/field/source identity, typed provenance, external alignment evidence,
  fixed reconstruction parameters, and exact gflow build/runtime identities.
- Invalidates the result, rendering, and open inspector immediately when a
  same-key field/path change or graph change alters construction identity.
- Added the resizable viewer-bottom Basin Inspector with row filters,
  compact/full columns, frozen identifying columns, per-basin checkboxes and
  colors, bulk selection, independent display modes, opacity controls, column
  definitions, and construction/provenance details.
- Top-K, row filtering, selection, colors, inspector layout, and renderer
  redraws do not reconstruct the canonical object.
- Plotly displays maximum fills and minimum outline halos. RGL displays the
  same maximum fills and uses enlarged translucent secondary markers for
  minimum basins; its legend and construction details identify this renderer
  difference.
- Density low/mid/high colors now have independent alpha controls. Density
  display state is retained when the subject overlay is toggled.
- Arms initialize with Endpoint A and Endpoint B both set to `NONE`.

## Files Changed Or Created

### `gflow`

- `.gitignore`
- `Makefile`
- `NAMESPACE`
- `R/basin_complex.R`
- `R/basin_complex_refinement.R`
- `R/basin_identity.R`
- `R/basin_summary.R`
- `inst/extdata/gflow-code-manifest.tsv`
- `tests/testthat/test-basin-complex-summary-identity.R`
- `tools/update_gflow_build_manifest.R`

Generated but ignored:

- `man/get.gflow.build.identity.Rd`
- `man/summary.basin_complex.Rd`

### `gflowui`

- `.Rbuildignore`
- `DESCRIPTION`
- `R/app_server.R`
- `R/app_server_renderer_helpers.R`
- `R/app_ui.R`
- `R/basin_display_helpers.R`
- `R/occupation_density_helpers.R`
- `inst/app/www/basin-inspector-state.js`
- `inst/app/www/styles.css`
- `tests/testthat/test-app-constructs.R`
- `tests/testthat/test-occupation-density.R`
- `dev/benchmark_basins_reference.R`
- `dev/basins_subject15_benchmark_2026-07-28.md`
- `dev/qa_basins_renderer_reference.R`
- `dev/basins_renderer_final_state_qa_2026-07-28.md`
- `dev/basins_canonical_complex_modification_plan.md`
- `dev/basins_canonical_complex_plan_auditor_handoff.md`
- the basin-plan audit and response records under `dev/audits/`

The pre-existing untracked
`dev/eod_gflowui_agent_handoff_prompt.md` was read as the EOD integration
contract and was not modified.

The pre-existing `gflow/AGENTS.md` modification and untracked
`gflow/split_audit/` files are unrelated user work. They were preserved and
excluded from the `gflow` implementation commit.

## Generated Artifacts

- Subject15 benchmark report:
  `/Users/pgajer/current_projects/gflowui/dev/basins_subject15_benchmark_2026-07-28.md`
- gflow code manifest:
  `/Users/pgajer/current_projects/gflow/inst/extdata/gflow-code-manifest.tsv`
- gflow source tarball:
  `/Users/pgajer/current_projects/gflow/gflow_0.2.0.tar.gz`
- gflowui source tarball:
  `/Users/pgajer/current_projects/gflowui/gflowui_0.0.0.9000.tar.gz`
- Source-loaded application:
  `http://127.0.0.1:3867/`
- Canonical project registration:
  `/Users/pgajer/current_projects/vaginal_community_trajectory_types/analysis/291_register_hmp_subject15_k03_gflowui_project.R`

The benchmark script is the source of truth for benchmark reproduction. The
gflow manifest is regenerated by `make manifest`. Rd files and `NAMESPACE` are
regenerated by `make document`.

## Commands Run

From `/Users/pgajer/current_projects/gflow`:

```sh
Rscript -e 'pkgload::load_all(".", quiet=TRUE); testthat::test_dir("tests/testthat", filter="basin", reporter="summary", stop_on_failure=TRUE)'
Rscript -e 'pkgload::load_all(".", quiet=TRUE); testthat::test_dir("tests/testthat", reporter="summary", stop_on_failure=TRUE)'
make audit-cleanup-boundary
make check
R CMD INSTALL gflow_0.2.0.tar.gz
Rscript -e 'library(gflow); print(get.gflow.build.identity())'
```

From `/Users/pgajer/current_projects/gflowui`:

```sh
Rscript -e 'pkgload::load_all(".", quiet=TRUE); testthat::test_file("tests/testthat/test-occupation-density.R", reporter="summary", stop_on_failure=TRUE)'
Rscript -e 'pkgload::load_all(".", quiet=TRUE); testthat::test_file("tests/testthat/test-app-constructs.R", reporter="summary", stop_on_failure=TRUE)'
Rscript -e 'pkgload::load_all(".", quiet=TRUE); testthat::test_dir("tests/testthat", reporter="summary", stop_on_failure=TRUE)'
/usr/bin/time -l Rscript dev/benchmark_basins_reference.R
Rscript dev/qa_basins_renderer_reference.R
R CMD build .
R CMD check gflowui_0.0.0.9000.tar.gz --as-cran
Rscript -e 'pkgload::load_all(".", quiet=TRUE); gflowui::run_gflowui(host="127.0.0.1", port=3867, launch.browser=FALSE)'
```

R 4.7 package installation used for the effective checker library:

```sh
Rscript -e 'install.packages(c("lifecycle", "testthat", "devtools"), lib="/Library/Frameworks/R.framework/Versions/4.7/Resources/library", repos="https://cloud.r-project.org", dependencies=NA)'
Rscript -e 'install.packages("desc", lib="/Library/Frameworks/R.framework/Versions/4.7/Resources/library", repos="https://cloud.r-project.org", dependencies=NA)'
```

## Validation

### `gflow`

- Focused basin-complex, plateau, identity, provenance, ranking, manifest, and
  compatibility tests passed.
- The complete source suite passed with 10 expected skips.
- `make audit-cleanup-boundary` passed with 108 exports, 96 S3 methods,
  44 dependency declarations, and 81 native registrations.
- Full `make check` completed all tests, examples, vignettes, and manual with
  one CRAN incoming-feasibility warning for non-mainstream `dgraphs`/`grip`.
- The checked tarball was installed, and
  `gflow::get.gflow.build.identity()` loaded successfully from the installed
  package.

### `gflowui`

- The complete test suite passed with one existing fixture-dependent skip:
  the conditional-expectation adapter test skips when both fit/refit fixture
  functions are unavailable.
- The final source package built successfully.
- `R CMD check --as-cran` completed with no errors. Remaining status was one
  incoming-feasibility warning and four notes: development version/non-mainstream
  dependencies/title casing, non-standard top-level files, existing dynamic
  server static-analysis globals and one long Rd example line, and unavailable
  recent HTML Tidy.
- Live Subject15 QA exercised the 6,529-vertex weighted-GRIP graph, Brier path
  index 4, both-direction construction, table rendering, checkbox persistence,
  per-basin color persistence, maximum fills, Plotly minimum halos, and actual
  RGL minimum secondary markers. Browser diagnostics contained no warning or
  error entries.
- Automated final-state evidence records 13 Plotly traces (6 maximum fills and
  6 minimum halos) and 6 actual RGL null-device minimum-marker layers.

### Subject15 Measurements

- 6,529 finite aligned field values.
- 13,058 assignment rows: 6,529 maximum and 6,529 minimum.
- 352 maximum basins and 841 minimum basins.
- `auto` resolved to `primary.support.mass` in both directions.
- No-trajectory uncached elapsed time: 10.680 seconds.
- Cache-hit elapsed time: 0.127 seconds.
- No-trajectory object size: 56,406,104 bytes.
- Stored-trajectory object size: 61,895,056 bytes.
- Whole benchmark-process maximum resident set: 798,720,000 bytes.
- Whole benchmark-process peak memory footprint: 604,177,536 bytes.

The implementation fixes trajectory storage off because no inspector or
renderer consumes path objects, and retaining them increased canonical object
size by approximately 9.7%.

## Canonical/Generated File Notes

- Roxygen comments in `gflow/R/` are canonical for generated Rd and
  `NAMESPACE`; regeneration command: `make document`.
- `gflow/tools/update_gflow_build_manifest.R` is canonical for the versioned
  code-input manifest; regeneration command: `make manifest`.
- `gflowui/dev/benchmark_basins_reference.R` is canonical for the benchmark
  report measurements.
- The HMP graph, layout, occupation-density, and metadata assets were treated
  as upstream fingerprinted inputs and were not edited. Their registration
  script now records the explicit graph/source/display vertex alignment
  contract.
- Source code was modified after the first package-check run to fix a live
  inspector redraw defect and to label RGL minimum markers. Focused/full tests,
  package build/check, and live renderer QA were repeated on the final source.

## Limitations And Unverified Claims

- The stored-trajectory benchmark ran second in the same R process, so its
  elapsed time is a warm-order observation rather than a controlled speed
  comparison.
- `Rprofmem` cumulative allocation bytes are not peak live memory.
  `/usr/bin/time -l` peak evidence covers the combined benchmark process, not
  each configuration independently.
- The benchmark does not claim basin-ID equivalence with legacy precomputed
  `trajectory_flow` assets. It verifies the canonical reconstruction, an
  unchanged source asset, the negligible UI normalization delta, and exact
  identity of the constructed field/mass with the normalized adapter input.
- RGL cannot reproduce Plotly marker outlines exactly. The implemented and
  live-tested fallback is an enlarged translucent secondary marker layer with
  explicit legend entries and construction text.
- The full conditional-expectation adapter fixture was unavailable in the
  current environment; generic conditional-expectation source discovery and
  support-size ranking are covered by the remaining tests.
- The package-check warning and notes described above remain; no CRAN
  submission-readiness claim is made.
- The source-loaded application is a local process and will cease to be
  available if that process or host is stopped.

## Reusable Workflow Capture

Classification: script/template candidate

Rationale: `dev/benchmark_basins_reference.R` provides a reproducible template
for measuring full-domain canonical basin construction, cache latency, retained
object size, and trajectory-storage cost on future reference projects. No new
global skill or shared note was created.

## Next Actor

Ready for: independent implementation audit or human review

Requested decision: none
