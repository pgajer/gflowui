# Canonical Basin Complex and Basin Inspector Plan Handoff

Status: Revised after independent re-audit; ready for final independent
re-audit
Role: Planning and source-inspection worker
Branch/worktree: `main` in `/Users/pgajer/current_projects/gflowui`
Base commit: `a74da1f6eb38d74b23c374a677c42190dc86c91b`
Final commit: No commit was created during this planning task
Audited gflow commit: `92a61c086f2fa1fa77223edfb02b74a1be3f1a28`
Audited gflow Git-describe output: `v0.2.0-1-g92a61c08-dirty`
Final git status: Plan, handoff, two audit records, and two audit responses are
untracked; the pre-existing `dev/eod_gflowui_agent_handoff_prompt.md` remains
untracked and was not modified

## Goal

Produce a detailed, auditable modification plan for replacing the current
direction-specific Basins workflow with a canonical both-direction
`gflow::create.basin.complex()` workflow, a strengthened
`summary.basin_complex()` API, a selectable wide basin summary table, and basin
color controls.

After the first independent audit, revise the plan to address all recorded
blockers and nonblocking major findings without beginning implementation.

After the independent re-audit, correct the remaining provenance trust-boundary
blocker and the three secondary findings without beginning implementation.

The planning task also required resolving:

- whether generic conditional expectations should be min-max transformed for
  mass ranking;
- how `rank.by = "auto"` should behave;
- whether CLOSEST ignores the edge-length quantile and long-edge fallback; and
- the precise meaning of the proposed Basins sidebar controls.

## Work Completed

The worker:

- read the complete agent handoff requirements;
- inspected current `gflowui` basin reconstruction, server state, sidebar
  controls, and reduced summary table;
- inspected canonical `gflow` constructor, trajectory adapters,
  connected-plateau CLOSEST implementation, C++ next-edge selection,
  canonical table schema, public accessors, and summary methods;
- distinguished canonical `summary.basin_complex()` from the comprehensive
  summary attached to the retired `basin_cx` class;
- ran a direct both-direction canonical construction on a nine-vertex graph;
- recorded decisions about conditional-expectation mass, auto ranking,
  fixed CLOSEST parameters, sidebar semantics, inspector layout, selection
  state, and color rendering; and
- created the implementation plan;
- read the complete independent audit;
- accepted and incorporated all four blocking and two nonblocking findings;
- added immutable gflow build identity, typed constructor-owned mass
  provenance, direction-specific measure-usability ranking, and strict
  full-domain validation to the plan;
- removed the proposed automatic induced-subgraph fallback;
- added complete zero-Top-K UI coverage and reference performance/memory
  evidence requirements; and
- created a finding-by-finding audit response;
- read the complete independent re-audit;
- replaced ambiguous provenance validation with explicit
  constructor-computed, constructor-validated, and upstream-attested layers;
- added ordered `vertex.id` as a proposed constructor input and assigned
  external-alignment rejection to gflowui;
- added `raw.allocated.mass` as a deliberate ranking option with
  allocation-current semantics;
- replaced the curated build-digest input set with a versioned complete
  package-code manifest and conservative runtime-compatibility identity;
- corrected the handoff's Git-describe label and Markdown document counts; and
- created a finding-by-finding re-audit response.

No R, C++, JavaScript, CSS, package metadata, tests, generated documentation,
or project assets were modified.

## Files Changed Or Created

Created:

- `/Users/pgajer/current_projects/gflowui/dev/basins_canonical_complex_modification_plan.md`
- `/Users/pgajer/current_projects/gflowui/dev/basins_canonical_complex_plan_auditor_handoff.md`
- `/Users/pgajer/current_projects/gflowui/dev/audits/basins_canonical_complex_plan_audit_response_2026-07-28.md`
- `/Users/pgajer/current_projects/gflowui/dev/audits/basins_canonical_complex_plan_reaudit_response_2026-07-28.md`

Pre-existing and not modified:

- `/Users/pgajer/current_projects/gflowui/dev/eod_gflowui_agent_handoff_prompt.md`
- `/Users/pgajer/current_projects/gflowui/dev/audits/basins_canonical_complex_plan_audit_2026-07-28.md`
- `/Users/pgajer/current_projects/gflowui/dev/audits/basins_canonical_complex_plan_reaudit_2026-07-28.md`

Inspected but not modified:

- `/Users/pgajer/current_projects/gflowui/R/basin_display_helpers.R`
- `/Users/pgajer/current_projects/gflowui/R/app_server.R`
- `/Users/pgajer/current_projects/gflowui/DESCRIPTION`
- `/Users/pgajer/current_projects/gflow/R/basin_complex.R`
- `/Users/pgajer/current_projects/gflow/R/basin_complex_adapters.R`
- `/Users/pgajer/current_projects/gflow/R/basin_complex_plateau_flow.R`
- `/Users/pgajer/current_projects/gflow/R/gfc_flow.R`
- `/Users/pgajer/current_projects/gflow/R/basin_cx.R`
- `/Users/pgajer/current_projects/gflow/R/compute_basins_of_attraction.R`
- `/Users/pgajer/current_projects/gflow/src/gfc_flow.cpp`
- `/Users/pgajer/current_projects/gflow/NAMESPACE`
- focused `gflow` basin-complex tests

## Generated Artifacts

No generated artifacts or reports were produced.

The plan, handoff, two audit records, and two audit-response Markdown files are
canonical source documents, not generated outputs. The two audit records were
authored independently and were not modified by the planning worker.

## Commands Run

Working directory for the commands below:

```text
/Users/pgajer/current_projects/gflowui
```

Handoff requirements and repository state:

```sh
sed -n '1,260p' /Users/pgajer/.codex/notes/workflows/agent_handoff_requirements.md
wc -l /Users/pgajer/.codex/notes/workflows/agent_handoff_requirements.md
sed -n '1,180p' dev/eod_gflowui_agent_handoff_prompt.md
git status --short
find dev -maxdepth 2 -type f -print | sort
git log -5 --oneline --decorate
git rev-parse HEAD
git branch --show-current
git remote -v
sed -n '1,460p' dev/audits/basins_canonical_complex_plan_audit_2026-07-28.md
sed -n '1,420p' dev/audits/basins_canonical_complex_plan_reaudit_2026-07-28.md
```

Source discovery:

```sh
rg -n "gflowui_estimate_basin_overlay|trajectory_flow|trajectory.flow|create\\.basin\\.complex|Flow direction|basin_direction|basin_summary|summary\\.basin" R tests /Users/pgajer/current_projects/gflow/R /Users/pgajer/current_projects/gflow/tests /Users/pgajer/current_projects/gflow/man
rg -n "\\.basin.success.object|basin.table <-|primary.support|exclusive.support|overlap" /Users/pgajer/current_projects/gflow/R
rg -n "\\.basin.requested.directions|\\.basin.assignment.from.membership|\\.trajectory.plateau.flow.direction" /Users/pgajer/current_projects/gflow/R/basin_complex_adapters.R /Users/pgajer/current_projects/gflow/R/basin_complex_plateau_flow.R
rg -n "edge.length.thld|edge.length.quantile.thld|long.edge.fallback|modulation == \\\"CLOSEST\\\"|CLOSEST" /Users/pgajer/current_projects/gflow/R /Users/pgajer/current_projects/gflow/src
rg -n "export\\((create\\.basin\\.complex|get\\.basin\\.table|get\\.basin\\.assignment|get\\.basin\\.membership)\\)|S3method\\(summary,(basin_complex|basin_cx)\\)" /Users/pgajer/current_projects/gflow/NAMESPACE
```

Relevant source ranges were printed with `sed`, including:

```sh
sed -n '1,360p' R/basin_display_helpers.R
sed -n '8360,8535p' R/app_server.R
sed -n '12745,12870p' R/app_server.R
sed -n '380,535p' /Users/pgajer/current_projects/gflow/R/basin_complex.R
sed -n '1430,1735p' /Users/pgajer/current_projects/gflow/R/basin_complex.R
sed -n '1880,1925p' /Users/pgajer/current_projects/gflow/R/basin_complex.R
sed -n '300,545p' /Users/pgajer/current_projects/gflow/R/basin_complex_adapters.R
sed -n '1,475p' /Users/pgajer/current_projects/gflow/R/basin_complex_plateau_flow.R
sed -n '38,100p' /Users/pgajer/current_projects/gflow/R/gfc_flow.R
sed -n '660,730p' /Users/pgajer/current_projects/gflow/src/gfc_flow.cpp
sed -n '920,995p' /Users/pgajer/current_projects/gflow/src/gfc_flow.cpp
sed -n '292,505p' /Users/pgajer/current_projects/gflow/R/basin_cx.R
```

Direct runtime probe:

```sh
Rscript - <<'RS'
pkgload::load_all('/Users/pgajer/current_projects/gflow', quiet = TRUE)
edges <- matrix(
  c(
    1,2,1, 1,4,1, 2,3,1, 2,5,1, 3,6,1, 4,5,1,
    4,7,1, 5,6,1, 5,8,1, 6,9,1, 7,8,1, 8,9,1
  ),
  ncol = 3,
  byrow = TRUE
)
adj <- lens <- vector('list', 9)
for (i in seq_len(nrow(edges))) {
  a <- edges[i,1]
  b <- edges[i,2]
  w <- edges[i,3]
  adj[[a]] <- c(adj[[a]], b)
  adj[[b]] <- c(adj[[b]], a)
  lens[[a]] <- c(lens[[a]], w)
  lens[[b]] <- c(lens[[b]], w)
}
field <- c(0,1,0,1,3,1,0,1,2)
x <- create.basin.complex(
  adj,
  lens,
  field,
  method = 'trajectory_flow',
  direction = 'both',
  method.params = list(
    modulation = 'CLOSEST',
    plateau.policy = 'connected_exact',
    edge.length.quantile.thld = 1,
    long.edge.fallback = 'allow_and_flag',
    store.trajectories = TRUE,
    symmetric.seeding = FALSE,
    tie.breaking = FALSE,
    primary.assignment.policy = 'backend_primary'
  )
)
print(x$status)
print(table(x$assignment$direction))
print(table(x$basin.table$type))
print(names(summary(x)))
print(names(get.basin.table(x)))
RS
```

Audit-response provenance and consistency checks:

```sh
git -C /Users/pgajer/current_projects/gflow rev-parse HEAD
git -C /Users/pgajer/current_projects/gflow describe --always --tags --dirty
git -C /Users/pgajer/current_projects/gflow status --short
git -C /Users/pgajer/current_projects/gflow diff -- \
  DESCRIPTION R/basin_complex.R R/basin_complex_adapters.R \
  R/basin_complex_plateau_flow.R R/gfc_flow.R src/gfc_flow.cpp
shasum -a 256 \
  /Users/pgajer/current_projects/gflow/DESCRIPTION \
  /Users/pgajer/current_projects/gflow/R/basin_complex.R \
  /Users/pgajer/current_projects/gflow/R/basin_complex_adapters.R \
  /Users/pgajer/current_projects/gflow/R/basin_complex_plateau_flow.R \
  /Users/pgajer/current_projects/gflow/R/gfc_flow.R \
  /Users/pgajer/current_projects/gflow/src/gfc_flow.cpp
rg -n "mass\\.source|finite-induced|induced-subgraph|gflow package version|primary support size because|always available|mass is present|mass-present|mass-absent|rank\\.resolved|store\\.trajectories|zero|retained rows|retention filtering|build identity" \
  dev/basins_canonical_complex_modification_plan.md
python3 <inline document-consistency assertions>
git diff --check
git status --short --branch
curl -sS -o /dev/null -w 'source_app_http=%{http_code}\n' \
  http://127.0.0.1:3867/
```

The inline assertions checked final newlines and trailing whitespace in all
four worker-authored planning/response documents, one-to-one R-01 through R-04
response coverage, required trust/build/ranking markers, sequential acceptance
gates 1–22, and removal of the two stale handoff labels.

## Validation

Performed:

- verified that the handoff requirements file contains 161 lines and was read
  through its end;
- verified that ordinary `gflowui` reconstruction calls the exported canonical
  `gflow::create.basin.complex()` function;
- verified that current `gflowui` invokes the constructor for only one selected
  direction;
- verified that the precomputed occupation-density maximum path can bypass a
  fresh canonical construction;
- verified that canonical `create.basin.complex(direction = "both")` constructs
  both directions in one object;
- verified that the canonical detailed basin table is available through the
  exported `get.basin.table()` accessor;
- verified that current `summary.basin_complex()` contains aggregate counts but
  not the comprehensive per-basin table;
- verified that the comprehensive `summary.basin_cx()` belongs to the retired
  object class;
- verified from R and C++ source that CLOSEST uses the edge-length threshold and
  fallback policy as documented; and
- verified Markdown plan content against the technical findings above;
- verified the exact audited gflow commit and dirty worktree description;
- verified that the inspected constructor-related gflow files have no Git diff
  from the audited commit;
- recorded SHA-256 evidence for those source files;
- mapped each audit finding to revised plan decisions, requirements, tests,
  implementation phases, and acceptance gates;
- checked the revised plan for obsolete package-version cache identity,
  inferred mass semantics, and induced-subgraph fallback language;
- mapped re-audit R-01 through R-04 to revised trust boundaries, allocated-mass
  semantics, complete build inputs, tests, and acceptance gates;
- checked that upstream claims retain validation authority, contract version,
  algorithm, evidence fingerprint, and status;
- checked that the plan assigns recomputable mismatches to gflow and external
  source/alignment mismatches to gflowui; and
- corrected the handoff's provenance label and source-document count;
- verified all 22 acceptance-gate numbers are sequential; and
- verified the source-loaded app returned HTTP 200 on port 3867.

Runtime probe output:

```text
status: ok
direction: both
assignment rows: max = 9, min = 9
basin rows: max = 2, min = 3
summary.basin_complex fields: 10 aggregate fields
canonical basin-table columns: 23
```

No implementation tests or package checks were run because no package source
code was changed.

## Main Findings

1. Current ordinary basin reconstruction is canonical gflow reconstruction,
   not an internal gflowui trajectory algorithm.
2. The current `Flow direction` control selects one direction passed to the
   canonical constructor.
3. A single both-direction constructor call provides one maximum and one
   minimum assignment per finite vertex.
4. Generic conditional-expectation fields have no scientifically justified
   mass by default.
5. Min-max transforming a conditional expectation would create arbitrary,
   directionally problematic pseudo-mass and was rejected in the plan.
6. `rank.by = "auto"` now resolves independently by direction from usable
   primary mass, current membership-allocated mass, coverage mass, and support
   size measures; mass presence alone is insufficient.
7. Under CLOSEST, the threshold is not absent from the implementation. With a
   permissive fallback it normally affects telemetry rather than the chosen
   shortest improving edge. With `forbid` it can change assignments.
8. Fixing the threshold at `1` makes all graph edges admissible and preserves
   unrestricted CLOSEST flow to graph-local extrema.
9. The plan replaces interactive advanced reconstruction settings with
   read-only construction details.
10. Semantic gflow version is insufficient for cache identity; the revised
    plan requires an immutable build ID tied to source and build artifacts.
11. Semantic vertex-mass provenance is an upstream attestation that gflow
    schema-validates and preserves without claiming to have verified the
    scientific meaning; the summary must not infer or relabel it.
12. Incomplete or non-finite fields are blocking in the default workflow;
    automatic induced-subgraph construction is removed.
13. Top-K zero is a supported post-ranking display state, not evidence that a
    canonical direction is empty.
14. gflow can verify only facts derivable from supplied mass, vertex IDs,
    adjacency, and edge lengths; external semantics and alignment remain
    explicitly attributed upstream attestations.
15. Membership-allocated mass is conserved when its complete raw allocation is
    current, whereas raw and retained support mass are overlapping coverage
    measures.
16. The build ID is based on a versioned complete package-code manifest, while
    runtime compatibility is a separate conservative cache identity.

## Canonical/Generated File Notes

- The plan and this handoff are canonical Markdown files under the `gflowui`
  repository. They have no generated HTML counterpart.
- No Markdown file under `/Users/pgajer/.codex/notes` was edited, so the shared
  notes HTML build was not required.
- `gflow` roxygen source is canonical for generated `man/*.Rd` and potentially
  `NAMESPACE`. No generated gflow files were edited or regenerated.
- Existing HMP project assets are derived scientific artifacts and were not
  modified.
- No package source file was modified. Final Markdown validation was rerun
  after the document edits.

## Limitations And Unverified Claims

- The strengthened `summary.basin_complex()` API is a proposal and has not been
  implemented.
- No package tests, package builds, R CMD checks, performance benchmarks, or
  installed-package checks were run for the proposed changes.
- The direct runtime probe used a nine-vertex graph, not the 6,529-vertex
  Subject 15 reference graph.
- The computational cost and memory use of both-direction reconstruction on the
  reference graph were not measured.
- Existing precomputed Subject 15 basin assignments were not numerically
  compared with a fresh canonical both-direction reconstruction.
- The proposed maximum-fill/minimum-halo encoding was not prototyped in Plotly
  or RGL.
- The bottom inspector was not rendered, measured, or tested at different
  viewport sizes.
- The proposed column-definition contract was not checked against all future
  basin-construction methods. The revised ranking contract explicitly covers
  current canonical availability patterns, including RTCB and overlap-cell
  objects, but remains unimplemented.
- The plan assumes occupation-density probability mass is the intended
  `vertex.mass` for basin-mass ranking. That assumption follows current
  gflowui behavior but was not independently validated against an external
  scientific specification during this task.
- The exact public argument and field names for the strengthened summary method
  remain provisional until implemented and reviewed in gflow.
- The independent audit reported focused gflow and gflowui validation, but the
  planning worker did not rerun the auditor's full command set while revising
  the documents.
- The independent re-audit reported focused gflow and gflowui validation, but
  the planning worker did not rerun that full command set while revising the
  documents.
- The twice-revised plan and re-audit response have not yet received final
  independent re-audit.

## Reusable Workflow Capture

Classification: No additional reusable artifact needed.

Rationale: The project-specific plan and factual handoff capture the relevant
decisions. The existing agent handoff requirements already provide the reusable
workflow standard.

Recommended artifact, if any: None beyond the plan, handoff, and two audit
responses created during this planning chain.

## Next Actor

Ready for: Final independent plan re-audit.

Requested decision: Human authorization to begin the cross-repository
implementation after the revised plan is accepted.
