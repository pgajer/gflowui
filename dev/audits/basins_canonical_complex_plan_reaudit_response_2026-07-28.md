# Response to Canonical Basin Complex Plan Re-Audit

Date: 2026-07-28

Plan:
`/Users/pgajer/current_projects/gflowui/dev/basins_canonical_complex_modification_plan.md`

Re-audit:
`/Users/pgajer/current_projects/gflowui/dev/audits/basins_canonical_complex_plan_reaudit_2026-07-28.md`

Prior response:
`/Users/pgajer/current_projects/gflowui/dev/audits/basins_canonical_complex_plan_audit_response_2026-07-28.md`

Disposition: R-01 through R-04 accepted and incorporated; revised plan is ready
for final independent re-audit

## Summary

The re-audit correctly found that the prior provenance design crossed an
untruthful verification boundary. `gflow` cannot verify external source
identity, scientific meaning, or source-to-graph alignment from values it
never receives.

The revised plan now separates:

1. facts computed by the constructor;
2. declarations the constructor can validate from supplied underlying values;
   and
3. scientific, source, and alignment claims attested by a named upstream
   authority.

The revision also resolves the three nonblocking comments by defining the role
of membership-allocated mass, replacing a curated build-input list with a
complete versioned code manifest, and correcting handoff labels and document
counts.

No implementation was performed.

## Finding Dispositions

### R-01 — Accepted

The constructor contract now includes:

```r
vertex.id = seq_along(field)
vertex.mass.provenance = NULL
```

`vertex.id` is unique, nonmissing, and length-aligned. gflowui passes reviewed
external IDs in graph order; the default integer sequence preserves backward
compatibility for callers without external identity.

The provenance model has three layers.

**Constructor-computed facts**

- mass-vector fingerprint;
- input total and normalization facts;
- ordered internal-graph fingerprint computed from supplied `vertex.id`,
  adjacency, and edge lengths; and
- gflow build and runtime-compatibility identities.

**Constructor-validated declarations**

- controlled enum and schema values;
- declared mass digest checked against `vertex.mass`;
- declared ordered-vertex digest checked against `vertex.id`; and
- declared internal-graph digest checked against supplied graph inputs.

**Upstream attestations**

- scientific mass kind;
- source/asset identity and source-content fingerprint;
- external graph identity; and
- source/mass/vertex/graph alignment.

Every attestation stores its claim, validation authority, validator or contract
name and version, algorithm, evidence fingerprint, and status. gflow
schema-validates and preserves the attestation but does not describe it as
constructor verification.

Mismatch ownership is explicit:

- gflow rejects recomputable mass, supplied-vertex-ID, and internal-graph
  mismatches;
- gflowui rejects external source, graph-ID, and vertex-order mismatches before
  construction.

Construction details distinguish constructor verification from upstream
attestation and no longer show a global “validated” provenance label.

Plan locations: D-08, R-16, R-22, Sections 5.5, 6.1–6.4, 7.1–7.3,
10.1–10.3, 11–14.

The prior audit response's overstatement was also corrected.

### R-02 — Accepted

`raw.allocated.mass` is now:

- an explicit public `rank.by` option;
- included deliberately in `auto` after primary support mass and before
  overlapping coverage-mass measures; and
- usable by `auto` only while canonical provenance says its raw membership
  allocation remains current for the retained rows.

The rationale is that allocated mass uses membership weights and is conserved
within a direction when all raw memberships are represented. Raw and retained
support mass are coverage measures; overlapping basins may each receive the
same vertex's full mass, so directional totals can exceed one.

Refinements that change basin membership or support without recomputing
allocation make `raw.allocated.mass` unavailable to both `auto` and explicit
ranking. Its column definition retains the pre-refinement meaning.

Tests now require an overlapping-membership fixture where coverage and
allocated mass differ in totals or ordering, plus resolved-measure and stale
allocation behavior.

Plan locations: D-02, R-21, Sections 5.2–5.7, 8.3, 12–14.

### R-03 — Accepted

The plan no longer relies on a manually curated list of
“constructor-relevant” files. The build identity uses a versioned,
build-generated manifest covering the complete package code input set,
including:

- all `R/` and `src/` files;
- `DESCRIPTION` and `NAMESPACE`;
- `configure*`, `Makevars*`, and other build configuration; and
- any other file loaded or compiled into package behavior.

Adding, removing, or changing any manifested code input changes the build ID.
Tests mutate representative R, native, namespace/configuration, and newly added
files.

R version, platform/architecture, native ABI, and imported/linked dependency
versions form a conservative runtime-compatibility identity used by the cache.
Narrowing that identity later requires evidence that an omitted difference
cannot alter construction.

Plan locations: D-07, R-18, R-23, Sections 10.3–10.4, 12–15.

### R-04 — Accepted

The handoff now labels `v0.2.0-1-g92a61c08-dirty` as Git-describe output rather
than a package description.

The Generated Artifacts and reusable-workflow sections now state the actual
number and role of Markdown source documents, including both audit responses.

## Remaining Scope

These are plan-level corrections. Implementation still must:

- define the final R data structures and argument names;
- implement provenance, vertex identity, allocated-mass availability, build
  manifest, runtime identity, cache behavior, UI, inspector, and renderers;
- regenerate documentation;
- run focused and full package QA;
- install the validated gflow build; and
- benchmark and inspect the 6,529-vertex reference project.
