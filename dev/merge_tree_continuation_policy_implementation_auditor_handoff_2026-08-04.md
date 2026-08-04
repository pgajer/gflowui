# Merge-Tree Continuation Policy Implementation Correction Handoff

Status: ready for independent re-audit

Role: implementation worker addressing audit findings MTCP-I01 and MTCP-I02

Audit report: `dev/audits/merge_tree_continuation_policy_implementation_audit_2026-08-04.md` on `codex/merge-continuation-policy-audit` at `d1435ce8e71d395ad5247d573811fa2c39bf8c51`

gflow branch: `codex/merge-continuation-policy`

gflow correction base: `5bcdba6f95a35d58d693de4dc668077f4bd48366`

gflow final: `a7b5ea72ff48f6a602a3d1660e7a0f87a44086d2`

gflowui branch: `codex/merge-continuation-policy`

gflowui continuation-policy code final: `61d81e31a52a109a542334e2b9fd605526e1e4cf`

Final tracked status at validation: clean in both fresh validation worktrees

## Goal

Correct the nonreproducible committed gflow code manifest identified as
MTCP-I01, install the corrected package, and revalidate the continuation-policy
gflowui tests against that exact installed build. The nonblocking MTCP-I02
coverage recommendation was also implemented.

## Work Completed

The code-identity input collector now uses Git's canonical package-file view
when the source tree is a Git checkout. It includes tracked files and
nonignored untracked package inputs, but excludes ignored developer-local and
toolchain-generated files. For source archives, installed packages, and
non-Git test fixtures, it retains a physical-file fallback.

The committed manifest was regenerated. It no longer contains these ignored
local paths from the audit finding:

- `src/Makevars.win.local`;
- `src/tests/main.cpp`;
- `src/tests/Makefile`; and
- `src/tests/test_helpers.hpp`.

It also no longer contains the ignored configure-generated
`src/Makevars.local`. A clean build can generate that file without changing
the manifest.

A temporary Git-repository fixture now verifies that tracked and nonignored
untracked package inputs are included while an ignored developer-local input
is excluded.

Durable public-API tests were added for the MTCP-I02 recommendation:

- minimum-tree external continuation through layout, exact cut,
  dendrogram conversion, tree plotting, and barcode plotting; and
- a disconnected equal-height forest/plateau case with independent external
  owners in both components.

The generated protected-symbol ledger was refreshed for the intentional
change to `.gflow.code.input.files`.

No continuation-policy algorithm or gflowui application source changed while
addressing the audit findings.

## Files Changed Or Created

gflow:

- `R/basin_identity.R`
- `inst/extdata/gflow-code-manifest.tsv`
- `tests/testthat/test-basin-complex-summary-identity.R`
- `tests/testthat/test-basin-merge-tree-public.R`
- `split_audit/cleanup/protected-basin-surface.txt`

gflowui:

- `dev/merge_tree_continuation_policy_implementation_auditor_handoff_2026-08-04.md`

## Commits And Remote State

gflow correction commits:

- `f18a19208dc0e88a8dd7c5f217de7359c84fd2ae` — `Make gflow build manifest reproducible`
- `a7b5ea72ff48f6a602a3d1660e7a0f87a44086d2` — `Refresh manifest ownership guardrail`

Both commits were pushed to `origin/codex/merge-continuation-policy`.

The gflowui validation used the exact existing continuation-policy source
commit `61d81e31a52a109a542334e2b9fd605526e1e4cf`.

## Commands Run

The gflow correction was generated and validated from isolated worktrees under
`/Users/pgajer/current_projects`.

```sh
make document
make manifest
Rscript tools/build_cleanup_ledger.R
git diff --check
make audit-cleanup-boundary
make check-fast
git status --short
git diff --exit-code -- inst/extdata/gflow-code-manifest.tsv
make install
env -u R_HOME -u R_LIBS -u R_LIBS_USER -u R_LIBS_SITE \
  R CMD INSTALL \
  --library=/Library/Frameworks/R.framework/Versions/4.7/Resources/library \
  gflow_0.2.0.tar.gz
```

The full gflow source suite was run after `make document` with:

```sh
Rscript -e 'pkgload::load_all(".", quiet=TRUE); testthat::test_dir("tests/testthat", reporter="summary", stop_on_failure=TRUE)'
```

The four gflowui files were run from a fresh detached worktree at
`61d81e31a52a109a542334e2b9fd605526e1e4cf` after loading the default-installed
gflow package:

```sh
Rscript -e 'library(gflow); pkgload::load_all(".", quiet=TRUE); files <- c(
  "tests/testthat/test-basin-merge-tree-panel.R",
  "tests/testthat/test-basin-merge-tree-adaptive-filtering-fixture.R",
  "tests/testthat/test-occupation-density.R",
  "tests/testthat/test-app-constructs.R"
); for (f in files) testthat::test_file(f, reporter="summary", stop_on_failure=TRUE)'
```

## Validation

### gflow

- Full `tests/testthat` source suite: **956 passed, 0 failed, 0 warnings,
  10 skipped**.
- `make audit-cleanup-boundary`: passed for 112 exports, 99 S3 methods,
  44 dependency declarations, and 81 native registrations.
- `make check-fast`: completed with no errors and one CRAN incoming-feasibility
  warning because `dgraphs` and `grip` are not in the mainstream CRAN/BioC
  repositories.
- Fresh-checkout reproducibility gate: `make check-fast` regenerated the
  manifest and `git status --short`, `git diff --exit-code`, and
  `git diff --exit-code -- inst/extdata/gflow-code-manifest.tsv` all remained
  empty/successful afterward.
- Direct inspection found no manifest rows matching
  `src/Makevars.local`, `src/Makevars.win.local`, or `src/tests/`.
- `make install` succeeded. The same built tarball was also installed into the
  first/default R 4.7 library so ordinary `library(gflow)` resolves the
  corrected build at
  `/Library/Frameworks/R.framework/Versions/4.7/Resources/library/gflow`.
- Default-installed build identity used by gflowui validation:
  - manifest digest: `514c4dce12b71a2e1e22f0f66879971f`;
  - native artifact digest: `cec210f96cc44997a9db3a475c9c4bf2`;
  - build ID: `8b2500f44b6cac82c708749c66b9044d`.

### gflowui

All four audited test files passed against the default-installed corrected
gflow build:

- `test-basin-merge-tree-panel.R`;
- `test-basin-merge-tree-adaptive-filtering-fixture.R`;
- `test-occupation-density.R`; and
- `test-app-constructs.R`.

`test-app-constructs.R` retained the previously disclosed 74
`non-empty data for zero-extent matrix` warnings from mocked rendering paths
and one optional conditional-expectation adapter skip. It had no failure or
error.

## Canonical And Generated File Notes

`R/basin_identity.R` is the source of truth for code-manifest membership.
`inst/extdata/gflow-code-manifest.tsv` is regenerated by:

```sh
make manifest
```

The package's normal `make build`, `make check-fast`, and `make install` paths
also regenerate the manifest before building.

`split_audit/cleanup/protected-basin-surface.txt` is generated by
`Rscript tools/build_cleanup_ledger.R`. That generator also refreshes an
environment-dependent cross-repository usage column in
`split_audit/cleanup/api-ownership.csv`; those unrelated usage-discovery
changes were not committed. Only the protected symbol hash required by the
source change was retained.

No source file was modified after the validation described above. This handoff
was created after validation and does not affect package or application code.

## Limitations And Unverified Claims

- The audit-only 1,036-state randomized oracle was not rerun because it is not
  part of the repository. The unchanged continuation algorithm was instead
  covered by the full gflow suite, the new durable minimum/forest/dendrogram/
  plotting tests, and all four audited gflowui files.
- Full `make check` was not rerun. The narrower audit acceptance gate requested
  `make check-fast`, and the complete source test suite was run separately.
- The known mocked-rendering warnings and optional adapter skip in
  `test-app-constructs.R` remain; this correction did not address them.
- Installation emitted existing compiler pragma warnings when C sources saw a
  C++-specific diagnostic option. Installation completed successfully, and
  this correction did not modify compiler configuration.
- The gflowui branch contains no application-code change after
  `61d81e31a52a109a542334e2b9fd605526e1e4cf`; the only new gflowui artifact in
  this correction is this factual handoff.

## Reusable Workflow Capture

Classification: no new reusable artifact needed.

Rationale: the repository already provides manifest, package-check, ownership,
and handoff workflows. The reproducibility behavior is now enforced directly
by a package test and the standard clean-checkout build gate.

## Current State

Ready for independent re-audit. The blocking provenance defect is corrected,
the nonblocking public-API coverage recommendation is implemented, the
corrected gflow build is installed, and the audited gflowui tests pass against
that installed build.
