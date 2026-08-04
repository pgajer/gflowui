# Basin-Tree Event Navigator Implementation Auditor Handoff

Status: Ready for independent audit

Role: Implementation worker

Date: 2026-08-04 (America/New_York)

Implementation worktree:
`/Users/pgajer/current_projects/gflowui-event-navigator`

Implementation branch:
`codex/basin-tree-event-navigator`

Base commit:
`f233112332ea64770ba86a094bf3a5f16e85f40d`

Final implementation commit:
`7ff2a4f647b7bb7c42bf3d2e22bc0bda33d727d3`

Primary implementation commit:
`fc358a2dff4489e42c72f28c25bd41e2034a3c26`

Portable-status follow-up commit:
`7ff2a4f647b7bb7c42bf3d2e22bc0bda33d727d3`

Remote branch:
`origin/codex/basin-tree-event-navigator`

Paired `gflow` source commit used for implementation, tests, live use, build,
and the authoritative package check:
`5bcdba6f95a35d58d693de4dc668077f4bd48366`

Pinned R package library:
`/Users/pgajer/.codex/tmp/gflowui-basin-analysis-phase2-r-library`

Final implementation-worktree status before the handoff-only commit:
clean; `git status --short` returned no entries.

## Goal

Replace the all-distinct-field-value threshold control in the interactive
maximum-basin merge-tree viewer with a discrete topology-event navigator while
preserving exact canonical `field >= h` graph cuts. The requested interaction
uses a horizontal slider, Previous and Next controls, browser-only threshold
preview during movement, and server computation only after a committed event.

## Work Completed

The implementation separates the interactive viewer into:

- a static resolved tree structure;
- a canonical topology-event table;
- an exact committed cut; and
- browser payload/commit coordination.

The event table is the strictly decreasing exact union of:

- one finite above-maximum sentinel;
- resolved-layout branch births;
- resolved-layout merge plateaus; and
- the selected graph component floor.

Equal canonical heights are grouped without a floating-point tolerance.
Events carry zero-based and one-based indices, exact heights, birth and merge
ID list-columns, combined event kinds, human summaries, and accessible value
text.

Every committed event delegates membership and component ownership to
`gflow::cut.basin.merge.tree()`. The navigator-facing membership is the full
canonical superlevel-set membership, including when the displayed proposal
contains only a filtered branch subset. Displayed maxima and merge-plateau
annotations remain scoped to the displayed tree.

The Shiny server now maintains an authoritative context token, event index,
exact height, and client nonce. Context identity includes the scientific
bundle/field, component, scope, resolved branch domain, continuation policy,
labels, colors, and selection/pin identities that affect the rendered layout.
Stale, malformed, repeated, and out-of-range commits do not cause a new cut.

Scope/proposal/continuation changes preserve an exact old height when present,
otherwise remap downward in filtration order. Scientific bundle or component
reset clears the old height and returns to the above-maximum event.

Cuts are computed lazily and retained in a bounded 32-entry in-session cache.
Static layout work is outside the committed-event reactive. Diagnostic counters
measure static builds, canonical cut calls, accepted/ignored commits, tree
renders, and linked graph-overlay evaluations.

The browser controller:

- changes only the Plotly threshold shape, annotation, and preview status on
  the range input event;
- does not call `Shiny.setInputValue()` from that input handler;
- sends a structured token/index/nonce commit on native change or a step
  button;
- retains at most one in-flight commit and one latest pending target;
- discards pending input when the scientific context changes;
- clears preview state on committed payload/plot replacement; and
- restores the authoritative committed state after a rejected/stale commit.

The UI now places Previous event, the horizontal event slider, and Next event
in one row, followed by preview and committed status. Boundary and single-event
disabled states, integer slider semantics, ARIA range metadata, accessible
names, and a polite committed-status region are present.

## Files Changed Or Created

Changed:

- `R/app_server.R`
- `R/basin_merge_tree_panel.R`
- `inst/app/www/basin-tree-interaction.js`
- `inst/app/www/styles.css`
- `tests/testthat/test-app-constructs.R`
- `tests/testthat/test-basin-merge-tree-panel.R`

Created:

- `tests/testthat/test-basin-tree-event-navigator-assets.R`
- `dev/basin_tree_event_navigator_performance_2026-08-04.md`
- `dev/basin_tree_event_navigator_implementation_auditor_handoff_2026-08-04.md`

No `gflow` source, numerical project asset, density estimate, merge-tree
construction rule, basin ranking/filtering rule, `NAMESPACE`, or Rd file was
changed.

## Generated Artifacts

Subject 15 browser timing/counter evidence is committed at:

`dev/basin_tree_event_navigator_performance_2026-08-04.md`

The exact implementation commit was exported and built outside the source
worktree under:

`/Users/pgajer/.codex/tmp/gflowui-event-check-7ff2a4f.BokGNc`

Built tarball:

`/Users/pgajer/.codex/tmp/gflowui-event-check-7ff2a4f.BokGNc/gflowui_0.0.0.9000.tar.gz`

SHA-256:

`a3b752677c2c591eb21ce338650067c1e07f0ba061da246d2060ba99738ec18f`

Package-check directory:

`/Users/pgajer/.codex/tmp/gflowui-event-check-7ff2a4f.BokGNc/gflowui.Rcheck`

Package-check log:

`/Users/pgajer/.codex/tmp/gflowui-event-check-7ff2a4f.BokGNc/gflowui.Rcheck/00check.log`

## Commands Run

Focused pure-helper and browser-asset regressions, from the implementation
worktree:

```sh
Rscript -e '.libPaths(c("/Users/pgajer/.codex/tmp/gflowui-basin-analysis-phase2-r-library", .libPaths())); pkgload::load_all(".", quiet=TRUE); testthat::test_file("tests/testthat/test-basin-merge-tree-panel.R", reporter="summary", stop_on_failure=TRUE); testthat::test_file("tests/testthat/test-basin-tree-event-navigator-assets.R", reporter="summary", stop_on_failure=TRUE)'
```

Subject 15 server regression, from the implementation worktree:

```sh
Rscript -e '.libPaths(c("/Users/pgajer/.codex/tmp/gflowui-basin-analysis-phase2-r-library", .libPaths())); pkgload::load_all(".", quiet=TRUE); testthat::test_file("tests/testthat/test-app-constructs.R", reporter="summary", stop_on_failure=TRUE)'
```

Exact-final-commit complete source test suite, from the implementation
worktree:

```sh
Rscript -e '.libPaths(c("/Users/pgajer/.codex/tmp/gflowui-basin-analysis-phase2-r-library", .libPaths())); pkgload::load_all(".", quiet=TRUE); testthat::test_dir("tests/testthat", reporter="summary", stop_on_failure=TRUE)'
```

Static validation, from the implementation worktree:

```sh
git diff --check
node --check inst/app/www/basin-tree-interaction.js
Rscript -e 'parse(file="R/app_server.R"); parse(file="R/basin_merge_tree_panel.R")'
Rscript -e 'tools::showNonASCIIfile("R/app_server.R")'
```

Exact source export and external build:

```sh
check_root=$(mktemp -d /Users/pgajer/.codex/tmp/gflowui-event-check-7ff2a4f.XXXXXX)
source_dir="$check_root/source"
mkdir "$source_dir"
git archive 7ff2a4f | tar -x -C "$source_dir"

R_PROFILE_USER="$check_root/pinned-library.R" R CMD build source
```

The temporary profile prepended the pinned package library:

```r
.libPaths(c(
  "/Users/pgajer/.codex/tmp/gflowui-basin-analysis-phase2-r-library",
  .libPaths()
))
```

Authoritative package check, from the external build directory:

```sh
R_LIBS="/Users/pgajer/.codex/tmp/gflowui-basin-analysis-phase2-r-library" \
R_LIBS_USER="/Users/pgajer/Library/R/arm64/4.7/library" \
R CMD check --no-manual gflowui_0.0.0.9000.tar.gz
```

The explicit `R_LIBS` assignment is material on this machine because its
default `R_LIBS` points to the framework library before `R_LIBS_USER`.

The source-loaded live app was started from the implementation worktree with:

```sh
Rscript -e '.libPaths(c("/Users/pgajer/.codex/tmp/gflowui-basin-analysis-phase2-r-library", .libPaths())); pkgload::load_all(".", quiet=TRUE); run_gflowui(host="127.0.0.1", port=3868, launch.browser=FALSE)'
```

## Validation

Focused merge-tree helper tests passed.

Browser-asset regression tests passed. The regression isolates the JavaScript
`input` handler and establishes that it invokes client preview but contains
no `Shiny.setInputValue()` call. It also checks structured commit fields,
coalescing state, context-change pending-state clearing, accessible metadata,
and horizontal responsive CSS.

The Subject 15 `test-app-constructs.R` run passed. It included:

- initial above-maximum state with zero active vertices;
- valid structured commit;
- stale-token rejection;
- repeated-index no-op;
- no static layout rebuild during event navigation;
- one new exact cut for one fresh committed event;
- bounded-cache backtracking reuse;
- no graph-overlay evaluation while linking is disabled;
- applying the committed event after relinking;
- exact-height preservation when changing scope; and
- all 6,529 Subject 15 component vertices active at the complete-domain floor.

That file reported one intentional skip because optional conditional-
expectation fit/refit functions were unavailable. It reported 74 existing
warnings with message `non-empty data for zero-extent matrix`.

The exact-final-commit complete `tests/testthat` directory passed. It reported
the same one intentional skip and the same 74 warnings. There were no test
failures.

The exact-commit tarball built successfully.

The authoritative `R CMD check --no-manual` completed with:

```text
Status: 1 NOTE
```

There were zero errors and zero warnings. The note is the repository's existing
large set of `no visible global function definition` and
`no visible binding for global variable` findings in `app_server` and
`mod_data_server`. The final check reported all three used `gflow`
merge-tree APIs as exported and reported the R source as ASCII-clean.

Live Subject 15 browser evidence used the graph-heat occupation density at
diffusion-time index 4 and the Plotly renderer. It observed:

- 6,385 former all-field positions;
- 35 current-proposal event positions;
- 677 complete-component event positions;
- 6,529 active vertices in one component at the complete floor;
- one initial static build and one lazy initial exact cut;
- no static rebuild for event commits;
- one accepted Next action causing one new cut, one tree render, and one linked
  graph-overlay evaluation;
- zero new cuts on cached backtracking; and
- no graph-overlay evaluation for navigation while graph linking was disabled.

Recorded single-run server elapsed times were:

- proposal static structure: 277 ms;
- proposal initial cut: 2 ms;
- proposal fresh early cut: 3 ms;
- proposal cached backtrack: 0 ms;
- complete static structure: 305 ms;
- complete initial cut: 2 ms;
- complete middle cut: 49 ms; and
- complete floor cut: 33 ms.

The live browser showed the accepted Previous-slider-Next order, exact event
status, boundary-disabled Previous button at the initial event, and a one-step
Next commit. Browser console inspection during the performance run returned no
warnings or errors.

## Interrupted Or Non-Authoritative Runs

An initial external check allowed the machine's default framework `gflow`
installation to precede the pinned library. That check reported the three
merge-tree functions as unexported and was interrupted. It is not used as
release evidence.

A second check used `R_PROFILE_USER`, but `R CMD check` launches vanilla
child processes and again allowed the framework `gflow` to take precedence.
It was also interrupted and is not used as release evidence.

The first check also identified two non-ASCII em dashes newly added to
`R/app_server.R`. Commit `7ff2a4f` replaced them with portable ASCII text.
All final source tests, build input, and the authoritative check use that commit.

## Canonical And Generated File Notes

The R, JavaScript, CSS, test, performance Markdown, and handoff Markdown files
are source files.

The tarball and `.Rcheck` tree are generated from the exact
`7ff2a4f647b7bb7c42bf3d2e22bc0bda33d727d3` source archive. They are retained
outside the worktree and are not committed.

No roxygen-generated files were changed or regenerated because the redesign
adds no exported R API.

The shared main checkout had pre-existing modified and untracked developer
documents. Implementation source was not written there. After validation, an
exact human-facing copy of this requested handoff was added at:

`/Users/pgajer/current_projects/gflowui/dev/basin_tree_event_navigator_implementation_auditor_handoff_2026-08-04.md`

The branch copy is the committed provenance record.

No implementation source was modified after the exact-final-commit full suite
or authoritative package check. Only this handoff document was added
afterward.

## Limitations And Unverified Claims

- The browser automation surface did not provide a reliable way to pause a
  native range drag before pointer release. A human-held drag was not recorded.
  Zero server work before release is established by the isolated JavaScript
  input-handler regression, source inspection, and the absence of a Shiny
  publication call in that handler, rather than by a paused-pointer trace.
- A native keyboard Arrow action could not be made to change the range value
  through the browser automation surface. Keyboard behavior relies on the
  native HTML range control plus its `input`/`change` listeners and was not
  independently demonstrated in the live automation session.
- RGL-specific frame timing was not collected. Plotly was exercised live.
  Plotly and RGL consume the same committed server graph-overlay reactive, and
  server tests cover renderer switching, but this handoff does not claim a
  separate RGL browser timing measurement.
- The server timings are single-run elapsed measurements, not a formal
  repeated-sample benchmark.
- The Subject 15 event counts are tied to the current scientific bundle,
  displayed proposal, component, continuation policy, and floor-deduplication
  behavior; they are not asserted as universal constants.
- Diagnostic performance counters remain exposed as `data-*` attributes on
  the event range input. They do not contain scientific membership tables or
  project secrets, but they are runtime instrumentation rather than a
  user-facing feature.
- The package-check NOTE remains unresolved. It predates this task and concerns
  broad namespace/static-analysis hygiene outside the event-navigator change.
- No hosted pull request was created.

## Reusable Workflow Capture

Classification: No additional reusable artifact needed.

Rationale: The topology-event model and client/server commit protocol are
specific to the gflowui basin-tree viewer. Existing isolated-worktree,
worker-auditor handoff, and R-package QA workflows already cover the reusable
process and release gates.

## Next Actor

Ready for: Independent implementation audit and pre-merge review.

Requested decision: None.

