# Basin Analysis Developer Reference

## Architecture boundary

The implementation follows the Revision 9 adaptive maximum-tree contract.
Display filtering is presentation policy and cannot alter the complete
canonical basin complex, density-value elder-rule topology, canonical IDs,
merge events, parentage, prominence, trajectory-flow assignments, mass,
support, or graph/source alignment.

`gflow` owns canonical tree construction and validation, elder survival,
branch and event identity, ancestor closure, restricted leaf order, and
filtered/complete layout coordinates.

`gflowui` owns defensive replacement-only scientific bundles, complete
trajectory-to-canonical mapping validation, component selection, display
policy, typed overflow, the pure attempt/display reducer, Shiny orchestration,
linked presentation, settings-only recipes, and render telemetry. Scientific
and selection algorithms belong in pure helpers rather than observers.

Version 1 presents a maximum-basin tree one component at a time. The complete
both-direction result remains available to the Inspector, Plot Workspace,
graph overlay, and export.

## State and result installation

Every proposal-affecting event allocates an attempt synchronously before
validation. A completion installs only when all three keys match:

```text
bundle.id
context.generation
attempt.id
```

`current.proposal` and `retained.last.valid.proposal` are separately owned and
must never alias. Same-context proposal edits may retain the prior valid
display. Scientific bundle or context invalidation clears current and
retained proposals, pins, transient selection, caches, and pending work.
Presentation events such as label mode and diagnostic visibility do not
allocate proposal attempts.

The launcher uses `later`. Result installation is asynchronous relative to the
initiating observer, but execution remains on the R session event loop. It is
not an off-process architecture.

## Cross-panel identity and recipes

Canonical maximum-basin ID links the merge tree, complete tree, Plot
Workspace, Inspector, and reference graph. Transient selection is
presentation-only. Pin and unpin reconstruct proposal membership, closure,
layout, counts, and overflow.

Recipes are validated settings, not serialized state. They exclude bundle,
context, component, canonical IDs, pins, selection, proposal, result, and
layout. Browser storage uses:

```text
gflowui-basin-analysis-recipe-v1
```

Restore validates against the active runtime bundle, reruns automatic
component selection, and performs an ordinary proposal recomputation.

## Performance telemetry

`basin_analysis_panel_metrics()` is construction-scoped session telemetry and
is cleared when scientific context is invalidated. Current fields include:

```text
filtered:
  layout.elapsed.ms
  render.elapsed.ms
  branch.count

complete:
  prepare.elapsed.ms
  branch.count
  event.count
```

Measure proposal construction around `gflowui_basin_execute_pending()`.
Report proposal, filtered layout, static render, and complete-view preparation
separately, with component size, final displayed count, filter/render
outcomes, platform, package commit, and pinned `gflow` commit. Also report
perceptible UI latency because short CPU timings alone do not prove an
event-loop-based app is responsive.

## Accessibility and responsive invariants

- General Inspector is an accessible `aside`.
- Tree, Plot Workspace, and Inspector are named sibling regions.
- Linked status changes use polite live regions.
- Row Show, Pin, and Color controls have basin-specific accessible names.
- The basin characteristics table has a screen-reader caption.
- The desktop separator supports pointer/touch and keyboard operation.
- At the responsive breakpoint, the Inspector stacks below the graph and the
  horizontal separator is hidden.
- Tree plots are dimensionally stable and horizontally scrollable.
- Overflow, empty, retained, all-label, long-label, and one-branch states
  expose explicit content rather than a blank frame.

## Release QA

Use the reviewed `gflow` installation first on `R_LIBS`. Run focused helper
and integrated-server tests before the full source suite:

```sh
Rscript -e '
  pkgload::load_all(".", quiet = TRUE)
  testthat::test_file(
    "tests/testthat/test-basin-merge-tree-panel.R",
    reporter = "summary",
    stop_on_failure = TRUE
  )
'

Rscript -e '
  pkgload::load_all(".", quiet = TRUE)
  testthat::test_file(
    "tests/testthat/test-app-constructs.R",
    reporter = "summary",
    stop_on_failure = TRUE
  )
'

Rscript -e '
  pkgload::load_all(".", quiet = TRUE)
  testthat::test_dir(
    "tests/testthat",
    reporter = "summary",
    stop_on_failure = TRUE
  )
'
```

After the final source/test edit, commit the exact source and extract it with
`git archive`. Build and check only that extracted commit:

```sh
R CMD build source
R CMD check --no-manual gflowui_0.0.0.9000.tar.gz
```

Record commit and dependency identities, commands, test totals, artifact
timestamps, and SHA-256 values for the tarball, `00check.log`, and
`testthat.Rout`. Compare worker files, Git blobs, and extracted source-package
files.

## Generated-file hygiene

Commit source inputs only. Keep these outside the repository:

- source tarballs and `.Rcheck` directories;
- screenshots and browser downloads;
- project registries, scientific caches, and exported basin bundles;
- temporary benchmark output; and
- rendered site output unless the repository explicitly declares it
  canonical.

Use a unique external QA directory for exact-commit artifacts and record its
path and checksums in the implementation handoff.
