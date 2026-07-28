# Canonical Basin Complex and Basin Inspector Plan Re-Audit

Date: 2026-07-28

Auditor role: Independent plan re-auditor

Re-audited handoff:
`/Users/pgajer/current_projects/gflowui/dev/basins_canonical_complex_plan_auditor_handoff.md`

Re-audited plan:
`/Users/pgajer/current_projects/gflowui/dev/basins_canonical_complex_modification_plan.md`

Audit response:
`/Users/pgajer/current_projects/gflowui/dev/audits/basins_canonical_complex_plan_audit_response_2026-07-28.md`

Prior audit:
`/Users/pgajer/current_projects/gflowui/dev/audits/basins_canonical_complex_plan_audit_2026-07-28.md`

## Verdict

**Revise before acceptance.**

The revision materially resolves five of the six prior findings and most of the
sixth. One provenance trust-boundary blocker remains: the response says the
`gflow` constructor validates external graph, vertex-order, and source
fingerprints, but the proposed constructor contract does not provide the
external identifiers or source content needed to perform those validations.

The plan should not describe an upstream attestation as constructor-verified
provenance. Once verification ownership and inputs are made explicit, no other
plan-level blocker identified in this re-audit prevents acceptance.

## Prior-Finding Disposition

| Prior finding | Re-audit disposition | Evidence |
|---|---|---|
| A-01: build/cache identity | Resolved, with a nonblocking completeness recommendation | D-07, Sections 10.3–10.4, distinct-build test and acceptance gate |
| A-02: `auto` selected unavailable primary mass | Resolved, with a nonblocking measure-choice recommendation | D-02, direction-specific usability rules, RTCB/overlap/partial tests |
| A-03: semantic mass provenance unavailable | Partially resolved; one blocker remains | Typed provenance is stored, but constructor/upstream verification ownership is not implementable as claimed |
| A-04: automatic induced subgraph | Resolved | D-09, full-domain rejection before cache access, induced mode deferred |
| A-05: zero-Top-K UI states | Resolved | All three zero combinations and recovery states are tests and gates |
| A-06: reference performance evidence | Resolved | Elapsed time, process memory, object size, cache latency, trajectory comparison, and usability disposition required |

## Findings

### Artifacts And Provenance

#### R-01 — BLOCKER — The mass-provenance contract does not define a truthful verification boundary

Evidence:

- The audit response states that the constructor validates “the supplied mass,
  graph, vertex-order, and source fingerprints” at
  `dev/audits/basins_canonical_complex_plan_audit_response_2026-07-28.md:82-96`.
- The handoff repeats that semantic mass provenance is validated by the
  constructor at
  `dev/basins_canonical_complex_plan_auditor_handoff.md:284-288`.
- The plan proposes only
  `vertex.mass.provenance = NULL` as the new constructor argument at
  `dev/basins_canonical_complex_modification_plan.md:458-465`.
- The plan requires the provenance record to contain a source-content,
  graph, vertex-order, and mass fingerprint at
  `dev/basins_canonical_complex_modification_plan.md:200-218`, but explicitly
  requires the constructor to recompute and verify only the mass-vector
  fingerprint.
- The source adapter—not `gflow`—has access to the asset ID, external vertex
  identifiers, source content, and alignment evidence at
  `dev/basins_canonical_complex_modification_plan.md:562-579`.
- The current `create.basin.complex()` interface accepts adjacency lists,
  parallel edge lengths, numeric field and mass vectors, method parameters,
  and simplification parameters. It does not receive external vertex IDs or
  source content:
  `/Users/pgajer/current_projects/gflow/R/basin_complex.R:1559-1578`.

Impact:

From its proposed inputs, `gflow` can independently:

- validate the mass vector;
- recompute its mass fingerprint and normalization facts;
- compute a fingerprint of the ordered internal adjacency/edge-length inputs;
  and
- schema-check controlled provenance fields.

It cannot independently verify:

- that an external source-content fingerprint identifies the claimed asset;
- that a claimed external vertex-order fingerprint matches source IDs that
  were never supplied; or
- that the controlled semantic kind (for example,
  `occupation_probability`) is scientifically correct.

Accepting `validation.status = "validated"` from the caller and preserving it
does not turn those facts into constructor verification. The current wording
would allow the UI to display “Mass provenance validation: validated” without
identifying who validated which claims from what evidence.

Required revision:

Define a field-level trust model in the plan and response:

1. **Constructor-computed facts:** mass fingerprint, input total,
   normalization facts, internal ordered-graph fingerprint, and build
   identity.
2. **Constructor-validated declarations:** controlled enum/schema values and
   any declared digest for which the constructor receives the underlying value
   and recomputes the digest.
3. **Upstream attestations:** scientific mass kind, source/asset identity,
   external vertex ordering, and source alignment validated by `gflowui` or a
   manifest authority.

For every attested field, store the validation authority, algorithm/version,
evidence fingerprint, and status. Do not label it constructor-verified.

If the design requires `gflow` itself to verify external vertex order, add an
explicit ordered `vertex.id` input (or an equivalent canonical identity
object) so the constructor can recompute the fingerprint. If the design keeps
vertex identity in `gflowui`, say so plainly and test that the adapter's
attestation round-trips without being upgraded to constructor validation.

Update the mismatch tests to state which layer rejects each mismatch:

- `gflow` rejects recomputable mass/internal-graph mismatches;
- `gflowui` rejects external source, graph-ID, and vertex-order mismatches
  before construction.

Construction details should distinguish, for example:

```text
Mass vector: constructor verified
Mass semantics: occupation_probability (attested by manifest <id>)
Source/vertex alignment: validated by gflowui contract <version>
```

### Estimation And Selection

#### R-02 — MAJOR, NONBLOCKING — Explain the omission of membership-allocated mass from `auto`

The revised hierarchy at
`dev/basins_canonical_complex_modification_plan.md:71-97` falls from primary
mass to retained and raw support mass, while the canonical table also contains
`raw.allocated.mass`. The latter is explicitly defined as membership-weighted
mass.

Independent reproduction on the nine-vertex overlap-cell object showed:

```text
direction  sum(raw.support.mass)  sum(raw.allocated.mass)
max        1.533333               1.000000
min        1.555556               1.000000
```

This is not evidence that raw support mass is wrong: it measures full mass
coverage of overlapping supports, so totals above one are expected. It is,
however, a different scientific quantity from allocated mass. The public
`auto` hierarchy should not silently encode that preference without rationale.

Recommended revision:

- document why coverage mass is preferred to membership-allocated mass for
  canonical methods without primary assignment; or
- add `raw.allocated.mass` as an explicit ranking option and place it
  deliberately in or outside the `auto` hierarchy.

Add a test using overlapping memberships where support and allocated mass
produce different totals or ordering. Ensure the resolved measure and column
definition state whether mass is overlapping coverage or conserved
membership allocation.

### Build Identity

#### R-03 — MAJOR, NONBLOCKING — Make the build-digest input set complete by construction

The plan requires a digest of “constructor-relevant” R and native sources at
`dev/basins_canonical_complex_modification_plan.md:181-198` and
`dev/basins_canonical_complex_modification_plan.md:899-919`. This resolves the
semantic-version collision, but a manually curated relevant-file list can
silently omit a helper that later begins affecting basin construction.

Recommended revision:

- Prefer hashing the complete installed R code/native artifact set or a
  build-generated manifest of all package code inputs.
- If a smaller dependency closure is used, version the manifest and test that
  changing any listed code input changes the combined build ID.
- Record R/platform and behavior-relevant dependency versions as descriptive
  provenance; include them in identity if testing shows they can change
  construction results.

This is not a current blocker because the plan already requires every relevant
source change to alter the build ID; the recommendation makes that requirement
less dependent on fallible manual classification.

### Handoff Accuracy

#### R-04 — MINOR — Correct two provenance labels/counts

- `dev/basins_canonical_complex_plan_auditor_handoff.md:9` labels
  `v0.2.0-1-g92a61c08-dirty` as the “gflow description”; it is Git-describe
  output, not the package description or semantic version.
- The Generated Artifacts section says “The two Markdown documents are source
  documents,” although the revised work created the plan, handoff, and audit
  response. This does not affect scientific validity but should be factual in
  the next handoff.

## Audit-Charter Layer Results

### Data-Generating Process

No new data-generating process or experimental run was introduced. The
reference occupation-density source remains an existing artifact. The plan now
requires full-domain finite alignment before cache access, which resolves the
prior partial-domain concern.

### Measurement

No new reported metric was generated. The revised plan now requires reference
elapsed time, process-memory evidence, object size, cache-hit latency, and a
trajectory-storage comparison. These are appropriately implementation-phase
measurements.

### Estimation And Selection Fairness

The conditional-expectation pseudo-mass rejection remains sound. Directional
`auto` availability is now tested from the values actually being ranked rather
than mass presence or method labels. R-02 is a request to make the chosen
overlap-mass semantics explicit, not a finding that the hierarchy is
necessarily invalid.

### Statistical Inference

No inferential comparison is proposed in this planning phase.

### Artifacts And Provenance

The exact `gflow` commit, dirty state, and constructor-source hashes were
reproduced. The build-ID design fixes the prior same-version cache collision.
R-01 remains the only provenance blocker.

### Estimator And Implementation Correctness

The full-domain source rule, post-ranking Top-K behavior, direction-specific
availability, explicit empty/unranked statuses, cache invalidation, and
renderer-specific gates are internally coherent. Focused current tests pass,
but none of the proposed APIs is implemented yet.

### Rendering Fidelity

The plan continues to treat Plotly and RGL validation separately and does not
claim that the proposed halo has been tested. No new rendering blocker was
found.

## Reproduced Evidence And Falsification Checks

### Source provenance

The following planning-baseline claims were reproduced:

```text
gflow commit:   92a61c086f2fa1fa77223edfb02b74a1be3f1a28
Git describe:   v0.2.0-1-g92a61c08-dirty
packageVersion: 0.2.0
```

All six constructor-related SHA-256 values in the audit response matched the
current files, and those files had no Git diff from the recorded commit.

### Canonical availability counterexample

The earlier no-primary counterexample remains reproducible:

- trajectory flow: 18 assigned rows and five finite primary masses;
- RTCB: 18 `not_applicable` rows and no finite primary mass;
- overlap cell: 18 `not_applicable` rows and no finite primary mass.

The revised direction-specific hierarchy detects these unavailable primary
measures and therefore resolves the original A-02 defect.

### Full-domain behavior

The current helper still demonstrates why the planned change is necessary:
removing an interior non-finite vertex renumbers the induced graph and leaks
induced IDs through the returned canonical object. The revised plan now blocks
that field before cache lookup and removes automatic induced construction, so
the original A-04 defect is resolved at plan level.

## Commands And Validation

Read-only repository and provenance checks included:

```sh
git status --short
git rev-parse HEAD
git -C /Users/pgajer/current_projects/gflow status --short
git -C /Users/pgajer/current_projects/gflow rev-parse HEAD
shasum -a 256 <six recorded constructor-related gflow files>
git -C /Users/pgajer/current_projects/gflow diff -- \
  DESCRIPTION R/basin_complex.R R/basin_complex_adapters.R \
  R/basin_complex_plateau_flow.R R/gfc_flow.R src/gfc_flow.cpp
```

Independent R probes:

- reproduced trajectory-flow, RTCB, and overlap-cell availability;
- compared overlap support mass with membership-allocated mass; and
- inspected the constructor's actual formal inputs.

Focused tests rerun:

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

Result: all focused current `gflow` basin-complex and `gflowui`
occupation-density tests passed.

No implementation, documentation regeneration, package build/check,
installed-package check, reference benchmark, or renderer QA was performed.
Those remain implementation-phase gates.

## Required Revision Before Acceptance

The plan and audit response must resolve R-01 by defining:

1. which provenance facts are recomputed by `gflow`;
2. which declarations are schema-validated by `gflow`;
3. which scientific/source/alignment claims are attested upstream;
4. the evidence and validation authority stored for each attestation; and
5. whether external vertex IDs become constructor inputs or remain exclusively
   owned by the `gflowui` source contract.

R-02 and R-03 may remain recorded implementation recommendations if the human
investigator accepts the stated tradeoffs. R-04 is editorial.
