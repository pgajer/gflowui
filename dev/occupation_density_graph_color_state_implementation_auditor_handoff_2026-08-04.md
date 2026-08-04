# Occupation-Density Graph Color-State Implementation Handoff

Status: Ready for independent audit

Role: Implementation worker

Repository: `/Users/pgajer/current_projects/gflowui`

Implementation worktree:
`/Users/pgajer/current_projects/gflowui-color-state`

Branch: `codex/occupation-density-color-state`

Base commit:
`61d81e31a52a109a542334e2b9fd605526e1e4cf`

Implementation source commit:
`75523b693a919a9d41399761eefbff3ebddbaa97`

Related documentation commit:
`ff0eef73c6d5b910fc0d14a00b54c05a583a9b7d`

The branch is based on the pushed merge-continuation implementation so the
resulting app contains both the continuation-policy work and this color-state
fix.

## Goal

Correct the intermittent Subject 15 graph-color feedback loop described in:

```text
/Users/pgajer/current_projects/gflowui/dev/
  occupation_density_graph_color_state_implementer_handoff_2026-08-04.md
```

The required state contract makes `graph_layout_state$color_by` the
authoritative in-session color selection for rendering and selector
presentation while retaining genuine user changes, programmatic density and
basin selections, and deterministic fallbacks.

## Work Completed

The graph color source now has an explicit one-way state path:

```text
user or programmatic action
        |
        v
graph_layout_state$color_by
        |
        +--> graph renderer
        |
        +--> graph color selector
```

The broad graph-layout observer no longer rereads
`input$graph_layout_color_by` whenever unrelated layout inputs invalidate.
Graph color has a dedicated input observer that runs only when the color input
itself changes.

Programmatic density and basin actions use one helper that:

- writes the canonical graph color state;
- records the server-selected value while the dynamic selector is rebinding;
- updates the browser selector; and
- rejects stale browser values until the requested selection is acknowledged.

The renderer and rebuilt Graph Layout selector now resolve color selection from
canonical state first. Browser input is treated as an input event rather than
an independent rendering authority.

Color choices are validated against the currently available sources. A valid
canonical source survives dynamic choice changes. If the source disappears,
the existing preset/project default precedence chooses one deterministic valid
fallback, and a reconciliation observer writes that fallback to both canonical
state and the selector.

The graph-asset layout preset writer also prefers canonical color state.

No density computation, normalization, Subject 15 numerical asset,
diffusion-time observer, or client-side density palette restoration code was
changed.

## Files Changed Or Created

Implementation source:

- `R/app_server.R`
- `tests/testthat/test-app-constructs.R`

Factual handoffs:

- `dev/merge_tree_continuation_policy_implementation_auditor_handoff_2026-08-04.md`
- `dev/occupation_density_graph_color_state_implementation_auditor_handoff_2026-08-04.md`

The first handoff documents the immediately preceding continuation-policy
implementation. It is unrelated to the color-state source diff.

## Regression Coverage

The new Subject 15 regression establishes:

- the project still starts with `cst_norm`;
- displaying density sets canonical and effective selection to
  `occupation_density_active`;
- the rebuilt selector emits the density option as selected;
- a stale CST rebinding event cannot overwrite the pending programmatic density
  selection;
- a transient `NULL` browser input preserves density;
- a subsequent acknowledged genuine CST selection updates both canonical and
  effective state;
- changing diffusion time while density is active retains the density source;
  and
- removing the active density source selects and reconciles the deterministic
  CST fallback.

The existing basin display regression now also verifies that programmatic
`basin_active` selection is the effective graph selection, not only the stored
state.

## Generated Artifacts

Exact-commit package build/check directory:

```text
/Users/pgajer/.codex/tmp/gflowui-color-state-check.fdGXu7
```

Package tarball:

```text
/Users/pgajer/.codex/tmp/gflowui-color-state-check.fdGXu7/
  gflowui_0.0.0.9000.tar.gz
```

Tarball SHA-256:

```text
fb4c406ead59ecea6d3aa7df8dac820f816f2c0296b69cf5c504f101e51fdc3c
```

Check log:

```text
/Users/pgajer/.codex/tmp/gflowui-color-state-check.fdGXu7/
  gflowui.Rcheck/00check.log
```

Check-log SHA-256:

```text
ba3815b227a22d8de4b85b4e5662d0043d0b779e9506adfa8dff0e9c733f3cdf
```

Exact source-loaded archive used for live validation:

```text
/Users/pgajer/.codex/tmp/gflowui-source-loaded-75523b6.Gp5PBl
```

These are task-specific temporary local artifacts and are not committed.

## Commands Run

All implementation and package-validation commands were run from:

```text
/Users/pgajer/current_projects/gflowui-color-state
```

The diagnostic split-state probe from the implementer handoff was rerun before
editing. It reproduced:

```text
input=cst_norm
state=occupation_density_active
effective=cst_norm
state_after_null=occupation_density_active
effective_after_null=cst_norm
```

Focused validation:

```sh
Rscript -e 'pkgload::load_all(".", quiet=TRUE); testthat::test_file("tests/testthat/test-app-constructs.R", reporter="summary", stop_on_failure=TRUE)'
```

```sh
Rscript -e 'pkgload::load_all(".", quiet=TRUE); testthat::test_file("tests/testthat/test-app-constructs.R", desc="canonical graph color state survives density source rebinding", reporter="summary", stop_on_failure=TRUE)'
```

```sh
Rscript -e 'pkgload::load_all(".", quiet=TRUE); testthat::test_file("tests/testthat/test-occupation-density.R", reporter="summary", stop_on_failure=TRUE)'
```

Full package tests:

```sh
Rscript -e 'pkgload::load_all(".", quiet=TRUE); testthat::test_dir("tests/testthat", reporter="summary", stop_on_failure=TRUE)'
```

Package build/check, run from the task-specific check directory:

```sh
R CMD build /Users/pgajer/current_projects/gflowui-color-state
R CMD check gflowui_0.0.0.9000.tar.gz --no-manual --no-tests
```

Repository checks and commits:

```sh
git diff --check
git commit -m 'Document merge continuation implementation audit handoff'
git commit -m 'Stabilize canonical graph color selection'
```

All R commands used a library path beginning with:

```text
/Users/pgajer/.codex/tmp/gflowui-basin-analysis-phase2-r-library
```

That task-specific library contains gflow commit
`5bcdba6f95a35d58d693de4dc668077f4bd48366`, which supplies the continuation
API required by the branch base.

The source-loaded app was launched from the exact source archive with:

```r
options(
  gflowui.projects_data_dir =
    "/Users/pgajer/Library/Application Support/org.R-project.R/R/gflowui/projects"
)
.libPaths(c(
  "/Users/pgajer/.codex/tmp/gflowui-basin-analysis-phase2-r-library",
  .libPaths()
))
pkgload::load_all(".", quiet = TRUE, export_all = FALSE)
gflowui::run_gflowui(
  host = "127.0.0.1",
  port = 3868,
  launch.browser = FALSE
)
```

## Validation

### Automated

- The focused graph-color regression passed with 15 expectations.
- `tests/testthat/test-app-constructs.R` passed.
- `tests/testthat/test-occupation-density.R` passed.
- The complete gflowui test suite passed.
- The complete suite reported the existing 74
  `non-empty data for zero-extent matrix` warnings from mocked rendering paths
  and one optional conditional-expectation adapter skip.
- `R CMD build` succeeded from clean commit `75523b6`.
- `R CMD check --no-manual --no-tests` completed with no errors or warnings and
  one existing NOTE about dynamically sourced/global helpers in
  `app_server.R`.
- `git diff --check` passed before commit.

### Live browser

Two fresh browser sessions opened the registered:

```text
HMP Subject 15 | k=3 Heat and Basin Path
```

The first session established:

- initial graph color source `VALENCIA CST`;
- three consecutive **Show Density on Graph** actions all retained
  `EOD: subject 15, Graph heat kernel, time index 4`;
- manual selection of `VALENCIA CST` succeeded;
- another density action restored the density source;
- dragging diffusion time from index 4 to index 5 updated the selector and
  status to the index-5 density; and
- the selector remained on the index-5 density after an additional six-second
  wait.

The second fresh session independently started at CST and selected the index-4
density after **Show Density on Graph**.

The Plotly graph visibly used the density palette while the density source was
selected.

No implementation source was modified after the exact-commit full suite,
package build/check, and browser validation.

## Canonical/Generated File Notes

`R/app_server.R` and `tests/testthat/test-app-constructs.R` are canonical source
files.

No public API or roxygen documentation changed. `NAMESPACE`, `man/*.Rd`,
generated vignettes, package tarballs, and `.Rcheck` output were not edited or
committed.

The diagnostic implementer handoff in the shared main checkout was read as
reference material and was not copied, changed, or committed by this worker.
The shared dirty main checkout and unrelated sibling files were not modified.

## Limitations And Unverified Claims

- An automated browser click issued approximately 400 ms after first expanding
  the dynamic Occupation Densities controls encountered
  `Density evaluation failed: argument is of length zero`. Retrying after the
  controls had settled succeeded, and all subsequent density actions did so.
  This input-initialization race occurs before graph-color selection and was
  outside the implemented state-ownership fix.
- The live browser validation observed selector text, density status, and
  visible Plotly density coloring. It did not capture a timestamped trace of
  every Shiny client message.
- The browser sessions exercised Subject 15's precomputed graph-heat path. They
  did not manually exercise every occupation-density estimator or every
  project.
- Dynamic-source removal and `basin_active` behavior were covered by server
  regression tests; source removal was not manually triggered in the browser.
- Package check used `--no-tests` because the complete test suite had already
  passed separately on the same clean commit.
- The source-loaded server and build/check artifacts are local temporary
  resources rather than durable deployments.
- This branch has not been merged into `main`.

## Reusable Workflow Capture

Classification: No reusable artifact needed.

Rationale: The fix is specific to gflowui's dynamic graph selector. The
canonical-state contract and rebinding behavior are now encoded in source and
regression tests, while existing package QA and worker/auditor workflows cover
the process.

## Next Actor

Ready for: Independent pre-merge audit of the implementation source commit and
its factual validation record.

Requested decision: None.
