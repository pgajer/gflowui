# Merge-Tree Continuation Policy Implementation Handoff

Status: Ready for independent audit

Role: Implementation worker

Repositories:

- `/Users/pgajer/current_projects/gflow`
- `/Users/pgajer/current_projects/gflowui`

Branches:

- `gflow`: `codex/merge-continuation-policy`
- `gflowui`: `codex/merge-continuation-policy`

Base commits:

- `gflow`: `eaaa65ea0cba34bd804db2e1c92a4a21648be8d5`
- `gflowui`: `5760bb00e2ff138948536c93bffc85a37dcb0c01`

Final commits:

- `gflow`: `5bcdba6f95a35d58d693de4dc668077f4bd48366`
- `gflowui`: `61d81e31a52a109a542334e2b9fd605526e1e4cf`

Remote state:

- Both final commits were pushed to
  `origin/codex/merge-continuation-policy`.
- The isolated implementation worktrees were clean after commit and were
  removed with `git worktree remove`; their metadata was pruned.

## Goal

Add a scientifically explicit merge-tree branch-continuation policy selector
without conflating branch continuation with the existing basin-label ranking
rule.

The implementation was required to:

- retain the canonical field-value elder rule as the default;
- allow trajectory-flow basin mass or support to determine which branch
  continues at a merge;
- retain canonical elder-rule prominence as a stable characteristic;
- add a separate policy-dependent `Continuation lifetime` characteristic; and
- adapt static and interactive figure titles to the active continuation rule.

## Work Completed

### gflow

The public merge-tree construction and rendering path now accepts an optional
external continuation priority and a continuation-measure label.

At each merge:

- the branch with the larger supplied priority continues;
- an exact priority tie falls back to the canonical field-value elder rule;
- an exact birth-value tie falls back to extremum-vertex index ordering.

The layout preserves canonical prominence in `persistence` and adds the
selected policy's parent, death level, and `continuation.lifetime`.

The policy arguments propagate through the public layout, cut, dendrogram, and
plot interfaces. Plot and barcode titles identify the active continuation
measure.

### gflowui

General Inspector now contains two independent controls:

- `Label basins by`, which controls the user-facing M/m labels; and
- `Branch continuation at merges`, which controls merge-tree parentage.

The continuation choices are:

- `Field-value elder rule (canonical)`;
- `Trajectory-flow basin mass priority`; and
- `Trajectory-flow basin support priority`.

The active continuation policy propagates to:

- the displayed and complete merge-tree layouts;
- cut labels and the linked 3D superlevel-set graph;
- selected-policy connector ancestors needed by filtered trees;
- merge-event and interactive-tree hover information;
- static, interactive, and barcode titles; and
- Basin Inspector and Basin Plot Workspace presentation data.

`Continuation lifetime` is present in the Basin characteristics table and the
plot-characteristic selector. Canonical `Prominence` remains unchanged when
the continuation policy changes.

Policy-specific lifetime calculation is memoized using the basin construction
fingerprint, gflow build identifier, and continuation rule. The canonical rule
uses prominence directly.

The display proposal's scientific initial subset remains based on the existing
trajectory-flow mass filtering. When a noncanonical continuation policy needs
additional ancestors to connect the requested branches, those branches are
added as connector-only ancestors. Rendering pauses rather than silently
dropping required branches if this closure exceeds the render budget.

## Files Changed Or Created

### gflow

- `R/basin_complex_merge_tree.R`
- `R/basin_merge_tree_public.R`
- `tests/testthat/test-basin-merge-tree-public.R`
- `inst/extdata/gflow-code-manifest.tsv`
- `split_audit/cleanup/protected-basin-surface.txt`

### gflowui

- `R/app_server.R`
- `R/basin_display_policy.R`
- `R/basin_inspector_helpers.R`
- `R/basin_merge_tree_panel.R`
- `R/basin_plot_helpers.R`
- `tests/testthat/test-app-constructs.R`
- `tests/testthat/test-basin-merge-tree-adaptive-filtering-fixture.R`
- `tests/testthat/test-basin-merge-tree-panel.R`
- `tests/testthat/test-occupation-density.R`

This handoff was created after those implementation commits and does not alter
either audit target commit.

## Generated Artifacts

No generated package artifact was committed.

For live validation, an exact `git archive` of gflowui commit `61d81e3` was
expanded under:

```text
/Users/pgajer/.codex/tmp/gflowui-source-loaded-61d81e3.aLYU3l
```

The matching gflow commit was installed into the task-specific library:

```text
/Users/pgajer/.codex/tmp/gflowui-basin-analysis-phase2-r-library
```

The gflowui package-check directory was:

```text
/Users/pgajer/.codex/tmp/gflowui-merge-policy-check.vd0SRB
```

These are temporary local artifacts and are not durable repository outputs.

## Commands Run

### gflow

Commands were run from the isolated gflow implementation worktree, which was
removed after the final clean-state check:

```sh
Rscript -e 'pkgload::load_all(".", quiet=TRUE); testthat::test_file("tests/testthat/test-basin-merge-tree-public.R", reporter="summary", stop_on_failure=TRUE)'
Rscript -e 'pkgload::load_all(".", quiet=TRUE); testthat::test_dir("tests/testthat", reporter="summary", stop_on_failure=TRUE)'
make check-fast
make audit-cleanup-boundary
R CMD INSTALL --library=/Users/pgajer/.codex/tmp/gflowui-basin-analysis-phase2-r-library .
git diff --check
git commit -m 'Add configurable merge-tree continuation policies'
git push -u origin codex/merge-continuation-policy
```

### gflowui

Commands were run from the isolated gflowui implementation worktree, which was
removed after the final clean-state check:

```sh
Rscript -e 'pkgload::load_all(".", quiet=TRUE); testthat::test_file("tests/testthat/test-basin-merge-tree-adaptive-filtering-fixture.R", reporter="summary", stop_on_failure=TRUE); testthat::test_file("tests/testthat/test-basin-merge-tree-panel.R", reporter="summary", stop_on_failure=TRUE); testthat::test_file("tests/testthat/test-occupation-density.R", reporter="summary", stop_on_failure=TRUE); testthat::test_file("tests/testthat/test-app-constructs.R", reporter="summary", stop_on_failure=TRUE)'
Rscript -e 'pkgload::load_all(".", quiet=TRUE); testthat::test_dir("tests/testthat", reporter="summary", stop_on_failure=TRUE)'
R CMD build /Users/pgajer/current_projects/gflowui-merge-continuation
R CMD check /Users/pgajer/current_projects/gflowui-merge-continuation/gflowui_0.0.0.9000.tar.gz --no-manual --no-tests
git diff --check
git commit -m 'Add merge-tree continuation policy selector'
git push -u origin codex/merge-continuation-policy
git archive HEAD
```

The R commands used a library path beginning with:

```text
/Users/pgajer/.codex/tmp/gflowui-basin-analysis-phase2-r-library
```

The source-loaded app was launched from the exact gflowui archive with:

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

### gflow

- The focused public merge-tree tests passed.
- The complete gflow `testthat` suite passed.
- `make check-fast` completed. Its CRAN incoming-feasibility warning reported
  that the strong dependency `dgraphs` and suggested/enhanced package `grip`
  were unavailable from the mainstream CRAN/Bioconductor repositories used by
  that check.
- `make audit-cleanup-boundary` passed after the protected-surface hash was
  regenerated for the intentional public merge-tree changes.
- The committed gflow revision was installed into the task-specific library
  used by gflowui validation.

The tests include a synthetic case in which canonical and externally weighted
continuation produce different surviving roots, plus exact-priority tie
fallback and cut-label behavior.

### gflowui

- The focused merge-tree, occupation-density, and Subject 15 server tests
  passed.
- The complete gflowui `testthat` suite passed.
- The full suite reported 74 existing
  `non-empty data for zero-extent matrix` warnings from mocked server-rendering
  paths and one conditional-expectation adapter skip because optional
  fit/refit functions were unavailable.
- The package built successfully.
- `R CMD check --no-manual --no-tests` completed with no errors or warnings and
  one NOTE listing longstanding dynamically sourced/global helper names in
  `app_server.R`.
- `git diff --check` passed before commit.
- The source-loaded app returned the expected `gflowui` page at
  `http://127.0.0.1:3868/`.

Live browser validation used the registered
`HMP Subject 15 | k=3 Heat and Basin Path` project and its graph-heat density at
time index 4. It established:

- the continuation selector is rendered at the top of General Inspector;
- the canonical field-value rule is the default;
- the Basin characteristics table includes `Continuation lifetime`;
- the mass rule renders the title
  `Filtered crossing-free trajectory-flow mass-priority continuation tree`;
- the support rule renders the corresponding support-priority title; and
- the Inspector's lifetime explanation updates to name the active rule.

No R or JavaScript source was modified after the complete test suite and
package check. The final commit metadata was created after those checks. The
exact committed archive was subsequently used for live browser validation.

## Canonical/Generated File Notes

The R and test files listed above are canonical source files.

`inst/extdata/gflow-code-manifest.tsv` and
`split_audit/cleanup/protected-basin-surface.txt` are repository guardrail
metadata regenerated for the intentional protected-surface changes. No
`man/*.Rd` or `NAMESPACE` change was required because the public documentation
is generated from the edited roxygen source and the affected functions were
already exported.

Package tarballs and `.Rcheck` output were kept outside the commits.

## Limitations And Unverified Claims

- No pull request was created and neither branch was merged into `main`.
- The gflowui package build/check ran against the final source content before
  commit creation rather than rebuilding a tarball from the post-commit
  archive. The post-commit archive was live-tested, and no source changed
  between package checking and commit.
- The gflowui package check intentionally used `--no-tests` because the full
  test suite had already been run separately against the same source content.
- Live browser validation exercised the maximum-basin Subject 15 density
  workflow. It did not manually exercise every minimum-basin view or every
  conditional-expectation project.
- The live browser check verified adaptive titles and policy descriptions. It
  did not manually compare every displayed policy-specific lifetime value
  against an independently calculated reference table.
- Temporary installed libraries, archive directories, and check logs are local
  and may be removed by later cleanup.
- The source-loaded server is a local process and is not a durable deployed
  service.

## Reusable Workflow Capture

Classification: No reusable artifact needed.

Rationale: The implementation uses the existing R package QA, isolated
worktree, and worker/auditor handoff workflows. The continuation-policy
semantics are project-specific and are represented in package source,
documentation text, and regression tests.

## Next Actor

Ready for: Independent pre-merge audit of the two pushed commits and their
factual validation record.

Requested decision: None.
