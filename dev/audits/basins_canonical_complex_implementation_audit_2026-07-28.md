# Canonical Basin Complex and Basin Inspector Implementation Audit

Date: 2026-07-28

Auditor role: Independent implementation auditor

Audited handoff:
`/Users/pgajer/current_projects/gflowui/dev/basins_canonical_complex_implementation_auditor_handoff.md`

Accepted implementation plan:
`/Users/pgajer/current_projects/gflowui/dev/basins_canonical_complex_modification_plan.md`

Repositories:

- `/Users/pgajer/current_projects/gflow`
- `/Users/pgajer/current_projects/gflowui`

## Verdict

**Revise before acceptance.**

The canonical constructor, ranked summary, complete code manifest, reference
benchmark, and most inspector state machinery are substantial and generally
coherent. The reference construction counts, ranking choices, object sizes,
and approximate timings are independently reproducible.

Four blockers prevent implementation acceptance:

1. `gflowui` attests source/graph/vertex alignment without comparing the
   source asset's graph and vertex identity with the displayed graph.
2. Changing an occupation-density field while retaining the same source key
   leaves the old basin object and old success status active.
3. Typed provenance is absent from the cache key, so a request with new
   attestation evidence can return an older attestation.
4. The full source-tree `gflow` suite and the repository ownership audit fail
   on the committed implementation.

These are provenance and stale-artifact failures, not cosmetic defects. They
must be fixed before the implementation phase is accepted.

## Findings

### Data And Source Contract

#### I-01 — BLOCKER — External graph and vertex alignment is asserted but not validated

The accepted plan says equal vector length is insufficient and assigns
source-ID, graph-ID, ordered external vertex-ID, and asset-content validation
to `gflowui`:

- `dev/basins_canonical_complex_modification_plan.md:689-706`
- `dev/basins_canonical_complex_modification_plan.md:717-737`

The implementation does not perform that comparison:

- `R/occupation_density_helpers.R:281-307` validates the density matrix's
  dimensions, finiteness, nonnegativity, and column totals, but does not require
  or compare source graph identity or ordered vertex identity.
- `R/app_server.R:8381-8408` combines values from the selected source with
  vertex IDs from the currently displayed graph.
- `R/app_server.R:8955-8964` hashes the current graph metadata, current graph
  vertex IDs, and source values together, but hashing supplied values together
  is not evidence that their upstream identities agree.
- `R/app_server.R:8969-8988` then creates an attestation with status
  `"validated"` and an algorithm claiming exact ordered vertex-ID and content
  comparison.

The actual Subject15 density asset contains:

```text
settings$graph.id:           symmetric_knn_k03
settings$graph.k:            3
selected$graph.fingerprint:  c0872d45a94b66aeb58689f49c1e59e61ad0dfd82de984c4b30c453cbab5d052
selected$vertex.fingerprint: c7eda107027b34105c94fa51ad35766a486fbeeef1654f4e294a166ae1977104
```

None of those identity fields is read by the basin source adapter. A
same-length permuted field or a field tied to another graph can therefore be
combined with the displayed graph and labeled externally validated.

The `gflow` provenance schema compounds the ambiguity by storing
`source.id` and `source.fingerprint` under `validated.declarations` at
`/Users/pgajer/current_projects/gflow/R/basin_identity.R:323-330`, although the
constructor only validates that they are nonempty strings. The scientific
source identity belongs in an attributed upstream attestation.

Required revision:

- require a source-side graph ID/fingerprint and ordered vertex-ID fingerprint
  or explicit ordered source vertex IDs;
- compare them with the selected graph's actual ID, `k`, fingerprint, and
  ordered IDs using the source contract's declared algorithm;
- reject the source before cache access when any comparison is unavailable or
  mismatched;
- include the compared values and algorithm version in attestation evidence;
- keep source identity in the upstream-attestation layer rather than implying
  constructor fact validation; and
- add permutation, wrong-graph-ID, wrong-`k`, and wrong-vertex-fingerprint
  rejection tests for occupation-density and conditional-expectation sources.

### Artifacts, State, And Provenance

#### I-02 — BLOCKER — A changed field can leave an older basin result displayed as current

The server invalidates a basin result only when `input$basin_source` changes to
a different source key:

- `R/app_server.R:8875-8883`

Occupation-density time changes update `occupation_density_result` without
changing the source key `occupation_density_active`:

- `R/app_server.R:7956-7965`
- `R/app_server.R:8198-8233`

No observer compares the active basin construction identity with the current
field fingerprint, graph fingerprint, graph set, or `k`.

An independent Subject15 `shiny::testServer()` probe reproduced the defect:

```text
computed path index:       4
stored field hash:         372d88291dc42c207cc0d1d6df42e106
new active path index:     5
new source field hash:     7cef2e4b01e2dd72c5dd4a288818ab52
basin_result cleared:      FALSE
stored field matches new:  FALSE
displayed status:          Computed ... time index 4 ... cache miss ...
```

The inspector and state still hold the path-4 object after path 5 becomes the
active estimate. The same missing identity observer can leave stale state after
graph changes.

Required revision:

- define one construction-input identity containing project, graph set,
  actual `k`, graph fingerprint, ordered vertex fingerprint, source/field
  fingerprint, mass fingerprint/provenance, construction parameters, build ID,
  and runtime ID;
- compare the active identity whenever source values or graph selection change;
- immediately mark the basin state stale or clear it, close/disable the
  inspector as appropriate, and prevent stale rendering;
- require explicit recomputation or a matching cache lookup before returning
  to ready state; and
- add server tests for same-key field changes, occupation-density path changes,
  graph changes, and recovery by recomputation.

#### I-03 — BLOCKER — Cache identity omits typed provenance and returns stale attestations

The accepted cache contract explicitly includes the attestation-evidence and
typed mass-provenance fingerprints:

- `dev/basins_canonical_complex_modification_plan.md:1054-1072`

`gflowui_basin_cache_key()` accepts no provenance argument and hashes no
attestation:

- `R/basin_display_helpers.R:160-194`

An independent probe made two otherwise identical construction requests. The
second request changed the attestation authority and evidence:

```text
same cache key:          TRUE
second request cache hit: TRUE
requested authority:    authority-B
returned authority:     authority-A
requested evidence:     evidence-B
returned evidence:      evidence-A
```

The cache therefore changes the truth of the returned provenance record.
Additionally, `R/basin_display_helpers.R:332-359` writes a constructor result
to cache before checking `status == "ok"`, allowing failed canonical objects to
become persistent entries.

Required revision:

- canonicalize and hash the full typed provenance record, including every
  upstream attestation and evidence fingerprint, in the cache key;
- store the external alignment validation evidence used by I-01 in that key;
- cache only validated `status == "ok"` objects; and
- add tests proving that changed authority, contract version, algorithm,
  evidence, source graph identity, or validation status cannot reuse an older
  entry.

### Estimator And Implementation Correctness

#### I-04 — BLOCKER — The committed gflow source tree fails its mandatory ownership tests

The full source-tree test suite fails:

```text
Namespace items missing from ledger: export:get.gflow.build.identity
Protected file changed: R/basin_complex.R
Protected symbol changed: .apply.basin.refinements
```

The same failures are reproduced by:

```sh
make audit-cleanup-boundary
```

Evidence:

- `/Users/pgajer/current_projects/gflow/tests/testthat/test-cleanup-ownership-guardrails.R:1-23`
- new export:
  `/Users/pgajer/current_projects/gflow/R/basin_identity.R:485-555`
- protected refinement change:
  `/Users/pgajer/current_projects/gflow/R/basin_complex_refinement.R:469-550`

The package-tarball check does not catch these defects because the tests
explicitly skip when the source-only cleanup ledger tools are absent from the
tarball. The latest retained `gflow.Rcheck/00check.log` is also from
`check-fast`, which skips tests.

The handoff's statement that the full package tests passed is therefore not
true for the committed source tree.

Required revision:

- add the export to the namespace ownership ledger;
- adjudicate and record the authorized protected-surface changes using the
  repository's cleanup ownership protocol rather than merely replacing
  expected hashes;
- rerun the complete source `testthat::test_dir()` suite;
- rerun `make audit-cleanup-boundary`;
- then rerun `make check` and retain evidence that tests, rather than only a
  no-tests fast check, ran on the final commit.

### Measurement

#### I-05 — MAJOR, NONBLOCKING — The benchmark's unchanged-mass check is tautological

The benchmark sets:

```r
field.before <- field
```

and later records:

```r
raw.mass.unchanged = identical(field, field.before)
```

at `dev/benchmark_basins_reference.R:25-27` and
`dev/benchmark_basins_reference.R:145-147`. No intervening operation can alter
either value, so this check does not validate the constructed object's stored
field or mass.

The production occupation-density path also calls
`gflowui_normalize_density()` before construction. On the reference vector the
sum is `0.99999999999999967`; normalization changes values by at most
`5.2041704279304213e-18`. That change is scientifically negligible and does not
alter this audit's basin findings, but it means “raw probability unchanged”
should be defined precisely.

Recommended revision:

- compare the original source vector with
  `without$field$input.values` and `without$field$vertex.mass.input`;
- record both exact identity and maximum absolute difference;
- distinguish an unchanged source asset from harmless numerical
  renormalization in the UI adapter.

### Rendering And Reproducibility

#### I-06 — MAJOR — Live Plotly/RGL acceptance lacks reproducible final-state evidence

The implementation contains distinct Plotly and RGL minimum-basin layers:

- Plotly outline markers: `R/app_server.R:9978-10044`
- RGL enlarged translucent markers: `R/app_server.R:10836-10914`

The new automated tests verify controls, helper state, and existing renderer
selection, but do not assert the basin traces/layers, selected vertex sets,
colors, or legend labels. The handoff reports live Plotly and actual RGL QA but
provides no saved script, screenshot, trace table, or renderer log from that
run.

The supplied running app at `http://127.0.0.1:3867/` loaded without browser
warnings or errors, but its project selector contained no registered projects,
so the reported Subject15 inspector and renderer state could not be reproduced
from that handoff artifact.

Required before implementation acceptance:

- add Plotly trace assertions for selected maximum fills and minimum halos;
- add RGL-layer assertions when RGL is installed, with an explicit skip reason
  otherwise;
- save a small final-state QA artifact identifying project, source fingerprint,
  build ID, renderer, selected basin keys, layer/trace counts, and diagnostic
  status; and
- make the handed-off app state capable of opening the referenced project or
  document the exact reproducible registration/start command.

### Handoff Accuracy

#### I-07 — MINOR — The recorded gflow implementation commit does not exist

The handoff records:

```text
5567e11f18e09140bd9fc11beca2400a3e868280
```

The actual implementation commit and current `gflow` HEAD are:

```text
5567e11f4904c50fb5829ae04f322a408ce571f3
```

The recorded object cannot be resolved by Git. Correct the handoff and any
downstream audit response to use the actual commit.

## Audit-Charter Layer Results

### Data-Generating Process

No new simulated data-generating process is introduced. The Subject15 graph
and density assets are existing upstream artifacts. Their file hashes are
stable during this audit, but I-01 means the implementation does not verify
their cross-asset alignment before attesting it.

### Measurement

Reference construction counts and object sizes reproduce exactly. Timing and
process-memory values are reasonably close and vary as expected between runs.
I-05 identifies one invalid diagnostic claim rather than a numerical basin
result discrepancy.

### Estimation And Selection Fairness

The implementation correctly avoids converting conditional expectations to
mass. Independent small-graph and reference probes confirm that `auto` chooses
primary support mass when usable and support size when mass is absent. The
allocated-versus-coverage distinction and direction-specific availability
logic are implemented and covered by focused tests.

### Statistical Inference

No inferential comparison or uncertainty claim is made.

### Artifacts And Provenance

The build manifest matches the current complete computed manifest. The
source-loaded build identity reports the actual clean source revision and
runtime identity. I-01 through I-03 are blocking because external provenance,
active state, and cache restoration can disagree with the actual construction
inputs.

### Estimator And Implementation Correctness

The canonical gflow object and summary behave correctly on the exercised
ranking and Top-K cases. Constructor positional compatibility is preserved.
The default workflow uses a full finite field and no induced subgraph.

I-04 prevents package-source acceptance. I-02 also makes the Shiny lifecycle
incorrect even though direct helper calls and the existing suite pass.

### Rendering Fidelity

The source contains the planned renderer-specific encodings, but durable
acceptance evidence and targeted layer tests are missing. Rendering remains
secondary to the source/provenance blockers.

## Independently Reproduced Reference Measurements

The benchmark was rerun from its source script on the audited commits:

| Measure | Handoff report | Audit rerun |
|---|---:|---:|
| Vertices | 6,529 | 6,529 |
| Assignment rows | 13,058 | 13,058 |
| Maximum basins | 352 | 352 |
| Minimum basins | 841 | 841 |
| Uncached elapsed | 9.667 s | 10.228 s |
| Cache-hit elapsed | 0.119 s | 0.125 s |
| No-trajectory object | 56,401,880 bytes | 56,401,880 bytes |
| Stored-trajectory object | 61,890,832 bytes | 61,890,832 bytes |
| Maximum resident set | 779,943,936 bytes | 813,596,672 bytes |
| Peak memory footprint | 613,827,616 bytes | 593,806,344 bytes |

Both runs resolved `primary.support.mass` for maximum and minimum basins. The
timing and process-memory variation does not challenge the handoff's usability
disposition.

The audit rerun's gflow identity was:

```text
source revision: 5567e11f4904c50fb5829ae04f322a408ce571f3
source dirty:    FALSE
manifest digest: 11c8630d55fa9b4456a25d42db761f1d
build ID:        e62dc69cb8e31ae451627e51ab20b510
runtime ID:      ad73a623d86e8825929530a2ae70e91a
```

## Commands And Validation

Repository and artifact checks included:

```sh
git status --short --branch
git rev-parse HEAD
git log -1 --format='%H %P %s' -- \
  dev/basins_canonical_complex_implementation_auditor_handoff.md
git diff --name-status \
  a74da1f6eb38d74b23c374a677c42190dc86c91b..HEAD
git -C /Users/pgajer/current_projects/gflow status --short --branch
git -C /Users/pgajer/current_projects/gflow rev-parse HEAD
git -C /Users/pgajer/current_projects/gflow diff --name-status \
  92a61c086f2fa1fa77223edfb02b74a1be3f1a28..HEAD
shasum -a 256 gflowui_0.0.0.9000.tar.gz \
  /Users/pgajer/current_projects/gflow/gflow_0.2.0.tar.gz
```

Independent validation included:

```sh
Rscript -e \
  'pkgload::load_all(".", quiet=TRUE);
   testthat::test_dir("tests/testthat",
                      reporter="summary",
                      stop_on_failure=TRUE)'
```

Results:

- `gflowui`: passed with one conditional-expectation fixture skip.
- `gflow`: failed the two cleanup ownership tests described in I-04.

Repository ownership validation:

```sh
make audit-cleanup-boundary
```

Result: failed with the three ownership errors in I-04.

Reference benchmark:

```sh
/usr/bin/time -l Rscript dev/benchmark_basins_reference.R
```

Additional independent probes:

- changed a cached provenance authority and evidence fingerprint while holding
  all current cache inputs fixed;
- changed Subject15 occupation-density path index after canonical construction;
- inspected the actual source asset's graph and vertex fingerprint fields;
- compared the embedded and freshly computed gflow code manifests;
- inspected source-loaded and installed gflow build identities;
- inspected retained `R CMD check` logs and source-tarball hashes; and
- opened the supplied local app and checked browser diagnostics.

## Required Re-Audit Inputs

The implementation response should map I-01 through I-07 to concrete fixes or
reasoned dispositions and provide:

1. final commit IDs for both repositories;
2. source-alignment rejection tests using real contract fields;
3. same-key field-change and graph-change invalidation tests;
4. provenance-sensitive cache-key tests;
5. passing full source `gflow` and `gflowui` suites;
6. passing `make audit-cleanup-boundary`;
7. final package-check logs;
8. corrected benchmark integrity checks; and
9. reproducible Plotly and actual-RGL basin-layer evidence.

No implementation source, generated documentation, package metadata, tests,
scientific assets, or shared Codex notes were modified by this audit.
