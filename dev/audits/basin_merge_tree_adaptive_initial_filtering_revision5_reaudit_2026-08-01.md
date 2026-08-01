# Adaptive Initial Filtering Revision 5: Re-audit

Date: 2026-08-01

Auditor role: independent specification re-auditor

Audited response:
`/Users/pgajer/current_projects/gflowui/dev/audits/basin_merge_tree_adaptive_initial_filtering_revision4_reaudit_response_2026-08-01.md`

Revised specification:
`/Users/pgajer/current_projects/gflowui/dev/basin_merge_tree_adaptive_initial_filtering_spec_2026-07-31.md`

Revision 4 re-audit:
`/Users/pgajer/current_projects/gflowui/dev/audits/basin_merge_tree_adaptive_initial_filtering_revision4_reaudit_2026-08-01.md`

Pinned revisions:

```text
gflowui: 4284eb8b20acc41561c23dec6d0df06e186a29cf
gflow:   24a671c4927df6ab6e5ac10361aecfd87cfaa0cb
upstream scientific repository:
         4615555547f3f406e79436c308d28fd78985b64e
```

The unrelated untracked gflowui audit, the pre-existing local gflow
`AGENTS.md` change, and the pre-existing scientific-repository changes were
not part of the assessed revision.

## Verdict

**Phase verdict: Revise before acceptance.**

**Response disposition: the normative V4-01 through V4-03 corrections are
substantive, but the reference-evidence claim is incomplete.**

Revision 5 now validates all four ranking measures, separates active attempts
from immutable displayed proposals, and distinguishes Filter None, Show all,
and the complete-tree viewer. The 307-assertion reference file passes, and the
new invalid-measure tests genuinely exercise the declared strict domains.

Two schema blockers remain:

1. Filter None may successfully construct a proposal when mass is invalid or
   unavailable, but proposal schema version 3 requires mass groups,
   denominator, zero count, and coverage without specifying their unavailable
   representations; the view-state reference helper also blocks rather than
   constructs this allowed proposal.
2. The view envelope requires a proposal fingerprint and context match, but
   proposal schema version 3 declares neither a proposal fingerprint nor a
   context fingerprint.

The complete-tree action test also uses `complete` as the filter-state value
even though `complete` is the proposal outcome and the declared filter value is
None. That is a nonblocking test defect because the normative UI text itself
is now clear.

## Blocking Findings

### V5-01 — BLOCKER — The Filter None mass-failure exception is not schema-complete or transition-complete

Audit Charter layer: **Measurement**, **Artifacts and provenance**, then
**Estimator and implementation correctness**.

Affected specification sections: **Source and Ranking-Measure Validation**,
**Versioned Proposal Record**, **View-state envelope**, **Proposal State
Model**, and **Required Validation**.

The specification deliberately allows Filter None to construct a current
complete-component proposal when the only failed measure is trajectory-flow
mass:

- `mass_invalid` may contain missing, negative, or nonfinite values; and
- `mass_unavailable` means the validated vector has zero total positive mass.

That exception is reasonable because the complete canonical tree does not
need a mass-ranked core. Mass-derived annotations, coverage, diagnostics, and
the mass-rank pair plot are disabled.

Proposal schema version 3 nevertheless requires every successful proposal to
contain:

- positive-mass and all-mass ranking groups;
- positive denominator and zero count;
- core coverage and final coverage; and
- the mass-based Important-label contribution.

Those fields cannot have their ordinary valid-mass meanings in the allowed
exception:

- With `mass_invalid`, no complete all-mass grouping exists, the denominator
  is invalid, and even zero count may be based on an incomplete vector.
- With `mass_unavailable`, the positive denominator is exactly zero, positive
  groups are empty, all-mass grouping contains the complete zero group, and
  normalized coverage is undefined rather than zero.

The specification says the views are unavailable but does not declare which
schema fields are null, empty, omitted, or carry typed unavailable values.
Different implementations can therefore serialize incompatible proposal/3
objects for the same valid Filter None state.

The reference evidence exposes the same gap. `reference_ranking_gate()` allows
Filter None with invalid mass and returns `complete`/`renderable`. In contrast,
`reference_view_transition()` has no filter-mode input and treats
`mass_invalid` and `mass_unavailable` as blocked attempts in every case. A
direct check returned:

```text
ranking gate:
  core outcome = complete
  render outcome = renderable

view transition for the same Filter None mass-invalid condition:
  active attempt outcome = blocked
  display source = none
  proposal installed = FALSE
```

Thus no current test constructs, serializes, installs, and displays the full
immutable proposal that the specification permits.

Risk:

- proposal schema version 3 has no unique representation for an allowed state;
- implementations can emit `NaN`, zero, null, empty lists, or partial groups
  for the same fields;
- normalized coverage can be falsely reported as zero;
- a valid canonical-only proposal can be blocked by the view-state layer; and
- the response's mass-only Filter None test claim can pass without exercising
  the actual proposal/view transition.

Required correction:

1. Define field-level availability for every mass-derived proposal field. A
   recommended contract is:

   | Mass state | Positive groups | All-mass groups | Denominator | Zero count | Coverage |
   |---|---|---|---|---|---|
   | `valid` | complete list | complete list | finite and positive | exact | finite |
   | `mass_unavailable` | empty list | one complete zero group | exact zero | component branch count | null/unavailable |
   | `mass_invalid` | null | null | null | null | null/unavailable |

2. Add a typed `mass.derived.available` field and an explicit unavailability
   reason rather than using numeric sentinels such as `NaN`.
3. Define Important-label behavior under Filter None with invalid mass:
   omit only the mass-ranked label contribution, retain the valid peak,
   prominence, support, survivor, and selected contributions, and disclose the
   omission.
4. Make view-state transition logic mode-aware. Filter None with mass invalid
   or unavailable and all other validation valid must produce
   `proposal_created`, `display.source = current`, and one complete immutable
   proposal rather than a blocked attempt.
5. Clarify that a mass change first invalidates the old retained proposal, then
   either installs a newly computed Filter None proposal or leaves no display
   for a mass-based mode.
6. Add full proposal/3 and view-state/1 round-trip tests for both
   `mass_invalid` and `mass_unavailable` under Filter None. Assert every
   mass-derived field, label reason, complete core/final IDs, render outcome,
   active-attempt outcome, and display source.
7. Narrow the response's current verification statement until that integrated
   transition is tested.

### V5-02 — BLOCKER — View-state fingerprint invariants refer to fields absent from proposal schema version 3

Audit Charter layer: **Artifacts and provenance**, then **Estimator and
implementation correctness**.

Affected specification sections: **Versioned Proposal Record**, **View-state
envelope**, and **Required Validation**.

The view envelope requires:

```text
context.fingerprint
display.proposal.fingerprint
display.proposal
```

It then requires the embedded proposal's fingerprint to equal
`display.proposal.fingerprint` and its context to equal
`context.fingerprint`.

The immutable proposal/3 field list contains graph, topology, vertex, field,
estimate, source, construction, and canonical-tree fingerprints, but it does
not declare:

- a proposal fingerprint; or
- a context fingerprint.

The required equality checks are therefore not evaluable from a conforming
proposal/3 record. The reference helper silently supplies
`fingerprint` and `context_fingerprint` fields that the normative proposal
schema does not define, so its passing tests do not prove schema conformance.

The fingerprint algorithms and scopes are also unstated. In particular,
proposal records contain a creation timestamp. If that timestamp participates
in the digest, identical recomputations have different content identities; if
it does not, the exclusion must be explicit. Similar ambiguity applies to
parameter ordering, canonical ID ordering, floating-point serialization, and
the active-attempt fingerprint.

Risk:

- a view state cannot independently validate its embedded proposal;
- equivalent implementations can hash different content;
- a tampered or mismatched proposal can satisfy only an implementation-local
  check; and
- serialized view-state round trips can preserve internally inconsistent
  fingerprint fields.

Required correction:

1. Add explicit `context.fingerprint` and `proposal.fingerprint` fields to
   `gflowui_basin_merge_tree_display_proposal/3`, or remove equality checks
   that depend on them. Adding the fields is recommended.
2. Define the context fingerprint's exact constituents. At minimum it should
   cover graph/topology, vertex map, selected field/source, trajectory and
   canonical constructions, direction, and component.
3. Define a versioned canonical serialization for proposal fingerprints,
   including list/field order, canonical ID order, numeric representation, and
   whether creation time is excluded. Excluding creation time is recommended
   if the fingerprint denotes proposal content rather than an event instance.
4. Define the active-attempt fingerprint analogously from context plus active
   input values and validation-relevant settings.
5. Require deserialization to reject a mismatched embedded proposal,
   proposal fingerprint, or context fingerprint rather than repairing it.
6. Extend tests with independently recomputed fingerprints, reordered inputs,
   timestamp-only changes, one-field tampering, wrong-context proposals, and
   corrupted serialized view states.

## Nonblocking Finding

### V5-03 — MAJOR — The complete-tree control reference test conflates filter state with core outcome

Audit Charter layer: **Estimator and implementation correctness**.

Affected artifacts: **User Interface**, **Required Validation**, and
`tests/testthat/test-basin-merge-tree-adaptive-filtering-fixture.R`.

The normative revision-5 text correctly distinguishes:

- filter-state value None;
- proposal core outcome `complete`; and
- the nonmutating complete-tree viewer action.

The reference helper at test line 506 instead executes:

```r
state$filter_mode <- "complete"
```

The test then asserts that value. `complete` is not one of the declared
`filter.mode` values; it is the proposal's core-selection outcome. The helper
also changes `display_source` to current and replaces only a fingerprint,
without installing the complete immutable `display_proposal` required by the
view-state envelope.

This does not undo the corrected normative contract, but the response
overstates the executable evidence for V4-03.

Recommended correction:

- set the exact serialized filter value used for None, distinct from core
  outcome `complete`;
- construct and install a complete proposal whose fingerprint and context
  satisfy the view-state invariants;
- assert both the filter value and core outcome;
- verify Show all and direct Filter=None produce the same complete proposal;
  and
- retain the existing nonmutation assertion for Open complete interactive
  tree.

## Revision 4 Finding Dispositions

| Revision 4 finding | Revision-5 disposition |
|---|---|
| V4-01 ranking-measure validation | Resolved. All four ranking domains, strict blocking behavior, survivor convention, proposal validation map, and invalid-input tests are explicit. V5-01 concerns the deliberately allowed mass-only Filter None exception. |
| V4-02 retained-last-valid representation | Normatively resolved. Immutable proposals and a separate view envelope now distinguish invalid attempts from retained displays. V5-02 concerns missing fingerprint fields; V5-01 concerns the untested mass-only current-proposal path. |
| V4-03 complete-tree UI semantics | Normatively resolved. Filter None, Show all, and the viewer are distinct. V5-03 identifies a mismatch in the reference helper, not the specification's prose. |

## Independent Verification

### Revision and diff

The assessed gflowui revision is:

```text
4284eb8b20acc41561c23dec6d0df06e186a29cf
```

The revision diff passed:

```sh
git diff --check \
  4b7610476a91dfd371a5f494f3c431d421685689..\
  4284eb8b20acc41561c23dec6d0df06e186a29cf
```

### Revision-5 reference test

From `/Users/pgajer/current_projects/gflowui`:

```sh
Rscript -e \
  'testthat::test_file("tests/testthat/test-basin-merge-tree-adaptive-filtering-fixture.R")'
```

Result:

```text
PASS 307
FAIL 0
WARN 0
SKIP 0
```

The added assertions genuinely cover the declared ranking domains and the
ordinary retained-view transition. They do not provide an integrated Filter
None mass-failure proposal/view test or a schema-defined fingerprint test.

### Reconstruction from pinned source assets

The source ZIP and topology RDS were digest-checked and read independently of
the fixture writer. Reconstruction produced:

```text
maximum branches:             352
canonical IDs exact:          TRUE
canonical parents exact:      TRUE
maximum mass difference:      3.469446951953614e-18
maximum support-size difference:
                              0
maximum peak difference:      2.465190328815662e-32
maximum prominence difference:
                              8.673617379884035e-19
support domain valid:         TRUE
peak domain valid:            TRUE
prominence domain valid:      TRUE
```

The empirical fixture satisfies the new ranking domains. That does not resolve
the schema behavior for future invalid mass.

### Focused current gflow regression

From `/Users/pgajer/current_projects/gflow`:

```sh
Rscript -e \
  'pkgload::load_all(".", quiet = TRUE);
   testthat::test_file("tests/testthat/test-basin-merge-tree-public.R")'
```

Result:

```text
PASS 46
FAIL 0
WARN 0
SKIP 0
```

An additional direct current-tree check found finite birth, death, and
persistence values for the component survivor in the public test fixture.
The future filtered-layout accessor and stronger canonical validation remain
unimplemented.

A full `make check-fast` or `make check` was not run because this revision
contains a specification and reference tests, not package implementation or
generated documentation. Targeted checks are proportionate to the response's
claims; full package QA remains mandatory for implementation acceptance.

## Audit-Charter Assessment

- **Data and provenance:** the pinned 352-branch mapping and all four ranking
  measures were independently reconstructed.
- **Measurement:** revision 5 supplies strict domains for every ranking
  measure. The mass-unavailable denominator and coverage representation still
  require a typed contract.
- **Selection:** Auto, manual modes, sentinels, labels, and ancestor closure
  are deterministic for valid inputs. Filter None's mass-only exception is
  normatively allowed but not represented end to end.
- **Inference:** no inferential claim is made; the adaptive rule remains an
  initial display policy.
- **Artifacts:** proposal/view fingerprint invariants cannot be checked against
  the declared proposal/3 field list.
- **Implementation:** the public gflow accessor, proposal schemas, view state,
  Shiny behavior, and renderers remain future work.
- **Rendering:** no application or viewport artifact exists yet.

No induced-subgraph construction is required or permitted. Every eligible
source still must provide one finite value for every graph vertex.

## Acceptance Conditions

Specification acceptance requires:

1. defining the complete proposal/3 and view-state/1 representation for Filter
   None under `mass_invalid` and `mass_unavailable`;
2. adding proposal and context fingerprints, with canonical hashing semantics,
   to the immutable proposal contract; and
3. adding integrated serialization and transition tests for both blockers.

V5-03 is a reference-test correction and does not block specification
acceptance. Implementation acceptance remains a separate worker-auditor cycle
after the required gflow and gflowui code exists.
