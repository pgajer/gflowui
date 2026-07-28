# Canonical Basin Complex and Basin Inspector Plan Final Re-Audit

Date: 2026-07-28

Auditor role: Independent final plan re-auditor

Re-audited handoff:
`/Users/pgajer/current_projects/gflowui/dev/basins_canonical_complex_plan_auditor_handoff.md`

Re-audited plan:
`/Users/pgajer/current_projects/gflowui/dev/basins_canonical_complex_modification_plan.md`

Worker response to the preceding re-audit:
`/Users/pgajer/current_projects/gflowui/dev/audits/basins_canonical_complex_plan_reaudit_response_2026-07-28.md`

Preceding re-audit:
`/Users/pgajer/current_projects/gflowui/dev/audits/basins_canonical_complex_plan_reaudit_2026-07-28.md`

## Verdict

**Accepted with nonblocking comments.**

The revised plan resolves the remaining provenance blocker and all three
secondary findings from the preceding re-audit. The trust model now limits
constructor verification to supplied values, preserves scientific and external
alignment claims as attributed upstream attestations, and assigns mismatch
rejection to the component that has the evidence needed to perform it.

The mass-ranking hierarchy, complete code-input manifest, conservative runtime
identity, full-domain field requirement, cache boundary, zero-Top-K states, and
implementation gates are mutually consistent. No remaining plan-level blocker
was found.

This verdict accepts the implementation plan, not an implementation. All 22
implementation acceptance gates remain open.

## Preceding-Finding Disposition

| Finding | Final disposition | Evidence |
|---|---|---|
| R-01: truthful provenance trust boundary | Resolved | D-08, Sections 5.5, 6.1–6.4, 10.3, 11, and gates 7–10 distinguish computed facts, validated declarations, and upstream attestations |
| R-02: membership-allocated mass omitted from `auto` | Resolved | D-02 deliberately places current `raw.allocated.mass` ahead of overlapping coverage mass and requires stale-allocation rejection |
| R-03: manually curated build-input digest | Resolved | D-07 and Section 10.4 require a versioned, build-generated complete package-code manifest plus runtime-compatibility identity |
| R-04: inaccurate handoff labels and counts | Resolved | The handoff identifies Git-describe output correctly and accounts for the plan, handoff, two audit records, and two responses |

## Findings

### Estimator And Implementation Correctness

#### F-01 — MAJOR, NONBLOCKING — Preserve positional constructor compatibility when adding identity and provenance arguments

The plan provisionally adds:

```r
vertex.id = seq_along(field)
vertex.mass.provenance = NULL
```

at `dev/basins_canonical_complex_modification_plan.md:237-246` and
`dev/basins_canonical_complex_modification_plan.md:539-550`.

The current public constructor has eleven formal arguments, ending with
`verbose`, at
`/Users/pgajer/current_projects/gflow/R/basin_complex.R:1559-1578`.
The default `vertex.id` preserves behavior when the new argument is omitted,
but it does not by itself preserve positional compatibility if either new
argument is inserted before an existing formal.

Implementation recommendation:

- append the new formals after all existing formal positions, or explicitly
  version and document a breaking migration;
- add a regression test using the complete legacy unnamed positional call;
- define the accepted canonical types and encoding for `vertex.id`, including
  factor handling and missing values, so its digest is stable; and
- state whether canonical assignment and support tables retain internal integer
  indices, expose external IDs in an additional field, or both.

This is nonblocking because the plan explicitly calls the argument names
provisional and the compatibility-preserving implementation is
straightforward. It must be settled before the public constructor change is
released.

### Handoff And Plan Accuracy

#### F-02 — MINOR — Correct one remaining Git-describe label in the plan

The handoff correctly labels `v0.2.0-1-g92a61c08-dirty` as Git-describe output.
The plan still calls the same value the “Audited gflow description” at
`dev/basins_canonical_complex_modification_plan.md:9`. Change that phrase to
“Audited gflow Git-describe output” during the next plan touch. This does not
affect the technical contract.

## Audit-Charter Layer Results

### Data-Generating Process

No data-generating process or new scientific dataset is proposed in this
planning phase. The reference workflow uses an existing occupation-density
asset. The plan does not claim that the reference asset was regenerated.

The default source contract now requires exactly one finite field value and one
reviewed external ID per graph vertex. It rejects missing, duplicate,
non-finite, or misordered inputs before cache lookup. Automatic
finite-induced-subgraph construction is removed and partial-field semantics are
deferred.

### Measurement

No new performance measurements are reported as results. The implementation
plan requires uncached elapsed time, process-memory evidence, object size,
cache-hit latency, and trajectory-storage comparisons on the 6,529-vertex
reference project before implementation acceptance.

### Estimation And Selection Fairness

The plan continues to reject conditional-expectation values as pseudo-mass.
`rank.by = "auto"` evaluates actual post-retention values independently by
direction and rejects partial, non-finite, negative, and all-zero candidates.

The revised hierarchy deliberately distinguishes:

- unique primary-assignment mass;
- conserved membership-allocated raw mass while its allocation is current;
- overlapping support-coverage mass; and
- support size.

Explicit ranking uses the same availability tests and cannot silently fall
back. Top-K is applied only after ranking and does not change construction.
These rules remove the earlier method-label and mass-presence confounding.

### Statistical Inference

No statistical comparison, uncertainty interval, or inferential claim is
proposed in this planning phase.

### Artifacts And Provenance

The recorded repository baselines were reproduced:

```text
gflowui HEAD: a74da1f6eb38d74b23c374a677c42190dc86c91b
gflow HEAD:   92a61c086f2fa1fa77223edfb02b74a1be3f1a28
```

The three final worker inputs had these SHA-256 values:

```text
cec5b45f19a35427913014791b6a96e7c6db4c3f87841bb4168fc99b1c163212  handoff
e5865cc7cfb899aaeae4c0533c3589530ac0e7ce5a41a61fb941ec8202496266  plan
a7acfa44d62ba6d2be966de06e9c0a916e05a3b2a43a79a82d2333538878ad43  re-audit response
```

The inspected `gflow` constructor-related files have no Git diff from the
recorded `gflow` commit. The existing `gflow/AGENTS.md` modification and
untracked `split_audit` artifacts are outside this plan's package-source
evidence and were not modified by this audit.

The revised provenance model is implementable from the proposed inputs:

- `gflow` computes mass, ordered-vertex, internal-graph, build, and runtime
  facts;
- `gflow` validates declarations whose underlying values it receives;
- `gflowui` validates external source, graph-ID, and vertex-order alignment;
  and
- `gflow` schema-validates and preserves those upstream attestations without
  upgrading their authority.

### Estimator And Implementation Correctness

The plan is internally coherent across decisions, API contract, source
contract, state model, cache identity, tests, implementation phases, and 22
acceptance gates. The plan explicitly removes induced-subgraph construction
from the default workflow.

Focused tests validate only the current packages; none of the proposed API,
provenance, cache, UI, or renderer changes exists yet. F-01 should be converted
into a constructor regression test during Phase A.

### Rendering Fidelity

No new renderer is claimed as validated. The plan separately requires Plotly
and actual RGL evidence and treats the proposed minimum-basin halo as
unverified until implementation QA.

## Reproduced Evidence And Falsification Checks

### Ranking-availability counterexample

An independent nine-vertex probe with normalized vertex mass reproduced:

```text
method                 assignment rows   finite primary masses
trajectory_flow        18                5
rtcb                   18                0
overlap_cell_complex   18                0
```

This falsifies any design that equates supplied mass with available primary
mass. The revised direction-specific candidate checks handle this case.

### Allocated-versus-coverage mass

On the same overlap-cell object:

```text
direction   sum(raw.support.mass)   sum(raw.allocated.mass)
max         1.4444444               1
min         1.5555556               1
```

The probe confirms that support mass is overlapping coverage while allocated
mass is conserved. The revised definitions, ordering, stale-allocation rule,
and requested overlap tests address the semantic difference.

### Trust-boundary falsification

The preceding blocker was retested by tracing every fingerprint and claim to
the component that receives its underlying evidence. No remaining statement
requires `gflow` to verify external content it does not receive. The plan also
forbids a global `validation.status = "validated"` label and requires the
summary to preserve layer-specific authority.

### Cache and partial-field falsification

The plan was searched for paths that could:

- reuse a same-version but different-build cache entry;
- access cache before full-domain validation;
- silently substitute basin IDs for an unavailable rank;
- reconstruct on Top-K changes; or
- fall back to an induced subgraph.

The build/runtime identities, pre-cache source rejection, unranked/error state,
state separation, and D-09/out-of-scope clauses close those paths at plan
level.

## Commands And Validation

Repository and document checks:

```sh
git status --short
git rev-parse HEAD
git -C /Users/pgajer/current_projects/gflow status --short
git -C /Users/pgajer/current_projects/gflow rev-parse HEAD
git -C /Users/pgajer/current_projects/gflow diff -- \
  DESCRIPTION R/basin_complex.R R/basin_complex_adapters.R \
  R/basin_complex_plateau_flow.R R/gfc_flow.R src/gfc_flow.cpp
shasum -a 256 \
  dev/basins_canonical_complex_plan_auditor_handoff.md \
  dev/basins_canonical_complex_modification_plan.md \
  dev/audits/basins_canonical_complex_plan_reaudit_response_2026-07-28.md
git diff --check
```

Focused tests:

```sh
Rscript - <<'RS'
pkgload::load_all('/Users/pgajer/current_projects/gflow', quiet = TRUE)
testthat::test_dir(
  '/Users/pgajer/current_projects/gflow/tests/testthat',
  filter = 'basin-complex',
  reporter = 'summary',
  stop_on_failure = TRUE
)
pkgload::load_all('/Users/pgajer/current_projects/gflowui', quiet = TRUE)
testthat::test_file(
  '/Users/pgajer/current_projects/gflowui/tests/testthat/test-occupation-density.R',
  reporter = 'summary',
  stop_on_failure = TRUE
)
RS
```

Result: all focused current `gflow` basin-complex tests and current `gflowui`
occupation-density tests passed.

Independent R probes:

- rebuilt the nine-vertex graph from its raw edge matrix;
- constructed trajectory-flow, RTCB, and overlap-cell objects with normalized
  mass;
- counted assignment rows and finite primary-mass aggregates;
- recomputed direction-level support and allocated mass totals; and
- inspected the current constructor's actual formal argument order.

## Remaining Implementation-Phase Work

No R, C++, JavaScript, CSS, package metadata, tests, generated documentation,
cache records, or scientific assets were modified by this audit.

Before feature acceptance, implementation still must complete and validate the
proposed constructor and summary contracts, provenance structures, build
manifest, runtime identity, cache behavior, UI state, inspector, renderers,
documentation, installed-package behavior, full package checks, reference
benchmark, and live reference-project QA.
