# Response to Canonical Basin Complex Plan Audit

Date: 2026-07-28

Plan:
`/Users/pgajer/current_projects/gflowui/dev/basins_canonical_complex_modification_plan.md`

Audit:
`/Users/pgajer/current_projects/gflowui/dev/audits/basins_canonical_complex_plan_audit_2026-07-28.md`

Disposition: All six initial findings accepted and incorporated; provenance
wording and design were further corrected after the independent re-audit

## Summary

The audit correctly identified four design blockers and two missing acceptance
areas. The plan has been revised rather than treating any of these as
implementation details to resolve later.

The revised design now:

- uses an immutable gflow build identity rather than semantic package version
  as cache identity;
- resolves automatic ranking from direction-specific measure usability rather
  than the presence of `vertex.mass`;
- stores typed vertex-mass provenance in the canonical `basin_complex` object
  with constructor facts, validated declarations, and upstream attestations
  kept distinct;
- rejects incomplete or non-finite default-workflow fields before cache access
  and removes automatic induced-subgraph construction;
- specifies all supported zero-Top-K UI states; and
- requires elapsed-time, memory, object-size, cache-latency, and
  trajectory-storage evidence on the reference graph.

No package implementation was performed as part of this response.

## Finding Dispositions

### A-01 — Accepted

The audit showed that semantic version `0.2.0` does not uniquely identify the
gflow constructor code. The plan now requires a combined immutable build ID
covering:

- semantic version as display metadata;
- source revision and dirty state when available;
- a versioned, build-generated manifest digest of the complete package code
  input set;
- installed native or package-build artifact digest; and
- an embedded source/build digest for installed packages without Git metadata.

A separate conservative runtime-compatibility identity records R,
platform/architecture, native ABI, and imported/linked dependency versions.
Both identities participate in cache retrieval.

Cache retrieval requires an exact combined-build-ID match. The validation
matrix includes a same-version, distinct-build collision test.

Plan locations: D-07, Section 3.4, R-18, R-23, Sections 10.3–10.4, Phase A,
Sections 13.1–14.

### A-02 — Accepted

The plan no longer treats `vertex.mass` presence as proof that a ranking
measure is usable. For each direction after retention filtering and before
Top-K filtering, a candidate is usable only if the direction is nonempty, all
candidate values are finite and nonnegative, and at least one value is
positive.

`auto` uses this hierarchy independently for maxima and minima:

1. primary support mass;
2. membership-allocated raw mass when its allocation is current;
3. retained support coverage mass;
4. raw support coverage mass;
5. primary support size;
6. retained support size; and
7. raw support size.

An empty direction has an explicit empty status. A nonempty direction with no
usable measure is explicitly unranked or errors; it is never ranked by basin
ID as a substitute. Explicit ranking does not fall back. Tests now cover
RTCB, overlap-cell, partial, all-zero, empty, mass-bearing/no-primary, and
direction-specific availability cases. Overlap fixtures distinguish conserved
membership allocation from support coverage that may sum above one.

Plan locations: D-02, R-05, R-15, Sections 5.2–5.4, Section 5.7, Phase A,
Sections 13.1 and 14.

### A-03 — Accepted

The summary method cannot reconstruct semantic mass provenance from the
current vectors and normalization data. The plan now extends the canonical
constructor/object contract with typed `vertex.mass` provenance and an
explicit ordered `vertex.id` input.

The corrected trust boundary is:

- gflow computes the actual mass fingerprint, normalization facts, ordered
  internal-graph fingerprint, and build/runtime identities;
- gflow validates controlled schemas and declared digests only when it receives
  the underlying mass, vertex IDs, or graph inputs needed to recompute them;
  and
- scientific mass meaning, source identity/content, external graph identity,
  and source alignment remain upstream attestations owned by a named,
  versioned gflowui or manifest authority.

Every attestation records authority, validator/contract version, algorithm,
evidence fingerprint, and status. `summary.basin_complex()` preserves these
layers without accepting a free-form semantic relabeling argument or
describing upstream claims as constructor-verified. Omitted provenance on
explicit mass is recorded as `unspecified_explicit` for backward compatibility,
not inferred as occupation probability.

Round-trip and layer-specific mismatch tests cover construction, refinement,
supported conversion, serialization, caching, and summary. gflow rejects
recomputable mass, supplied-vertex-ID, and internal-graph mismatches; gflowui
rejects external source, graph-ID, and vertex-order mismatches before
construction.

Plan locations: D-08, R-16, Sections 5.4–5.5, Sections 6.2–6.3, Sections
10.1–10.4, Phase A, Sections 13.1 and 14.

### A-04 — Accepted

The automatic finite-induced-subgraph fallback has been removed from the
planned default workflow. Every eligible source must contain exactly one
finite, graph-aligned scalar value for every vertex.

Length, ID, order, graph-fingerprint, and finiteness failures block before
cache lookup or construction. They cannot populate or reuse a cache and cannot
leave an older result displayed as current. Partial-domain support is deferred
until a separate scientific specification defines graph-domain semantics and
complete ID remapping.

Plan locations: D-09, R-17, Sections 6.1 and 6.4, Sections 10.3 and 11,
Phase B, Sections 13.2, 14, and 16.

### A-05 — Accepted

The UI validation matrix and acceptance gates now cover:

- maximum Top-K zero with minimum Top-K positive;
- minimum Top-K zero with maximum Top-K positive;
- both Top-K values zero;
- empty table and empty/disabled bulk-selection behavior;
- neutral graph rendering; and
- restoration after Top-K is increased.

The ranking contract also states that Top-K is applied after availability
resolution and ranking, so zero is a supported display filter rather than an
empty-source condition.

Plan locations: D-06, Section 5.3, R-19, Sections 13.2 and 14.

### A-06 — Accepted

Reference acceptance now requires:

- uncached elapsed time;
- peak resident-memory or equivalent process-memory evidence;
- canonical object size;
- cache-hit latency;
- a trajectory-storage-on/off comparison;
- the final fixed trajectory-storage choice; and
- a human-facing usability disposition.

The first measured reference run establishes the baseline. A hard numerical
threshold is intentionally deferred until evidence exists, but implementation
cannot be accepted without recording the measurements and deciding whether
optimization is needed.

Plan locations: D-03, R-20, Phase E, Sections 13.3 and 14.

## Audited Dependency Provenance

The plan's gflow source conclusions and this response use:

```text
Repository: /Users/pgajer/current_projects/gflow
Commit: 92a61c086f2fa1fa77223edfb02b74a1be3f1a28
Describe: v0.2.0-1-g92a61c08-dirty
Semantic package version: 0.2.0
```

The gflow worktree dirtiness consists of a modified `AGENTS.md` and untracked
synthetic-data planning/audit artifacts. The constructor-related source files
inspected for this plan match the recorded commit. Their SHA-256 values are:

```text
DESCRIPTION                           28d2eb0d505e87c2ec7cdc85701d67351b2a6cfa075e1e9c9b850f6f75f16d35
R/basin_complex.R                     e9548a42c319600a2e7f096fc6a2ec43a24fedf549bf21737ac293e98268492d
R/basin_complex_adapters.R            b08175a4330f0ec87ddaa65a097aaa25baa883182d61e64d9db44a6188288733
R/basin_complex_plateau_flow.R        3e63414d2468e30b7860118aac480007a6618ae7d1bfab0a78165fd32521f5e7
R/gfc_flow.R                          2597de10efc24c8bae1a533ab9ef76557cee1551ab83a037d549cd7a54cdce81
src/gfc_flow.cpp                      b7cfc326f26ae1eddfabf7d1ae4b4a771ce3c84ec78314abbc2edb28a5db1daa
```

These recorded hashes are audit evidence for the planning baseline. They are
not the proposed runtime build-ID algorithm.

## Remaining Scope

The revisions resolve plan-level omissions only. The following remain future
implementation and validation work:

- implement and test the gflow summary, provenance, and build-identity APIs;
- implement and test the gflowui full-domain adapter, cache, sidebar,
  inspector, selection, color, and renderer behavior;
- benchmark the 6,529-vertex reference graph;
- compare canonical and precomputed reference results; and
- complete focused and full package QA before installation or release.
