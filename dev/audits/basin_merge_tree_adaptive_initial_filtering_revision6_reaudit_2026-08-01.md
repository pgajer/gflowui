# Adaptive Initial Filtering Revision 6: Re-audit

Date: 2026-08-01

Auditor role: independent specification re-auditor

Audited response:
`/Users/pgajer/current_projects/gflowui/dev/audits/basin_merge_tree_adaptive_initial_filtering_revision5_reaudit_response_2026-08-01.md`

Revised specification:
`/Users/pgajer/current_projects/gflowui/dev/basin_merge_tree_adaptive_initial_filtering_spec_2026-07-31.md`

Revision 5 re-audit:
`/Users/pgajer/current_projects/gflowui/dev/audits/basin_merge_tree_adaptive_initial_filtering_revision5_reaudit_2026-08-01.md`

Pinned revisions:

```text
gflowui: f9a879027dd359d29b1894d4cf23ea0b850f7784
gflow:   24a671c4927df6ab6e5ac10361aecfd87cfaa0cb
upstream scientific repository:
         4615555547f3f406e79436c308d28fd78985b64e
```

The unrelated untracked gflowui audit, the pre-existing local gflow
`AGENTS.md` change, and the pre-existing scientific-repository changes were
not part of the assessed revision.

## Verdict

**Phase verdict: Revise before acceptance.**

**Response disposition: V5-01 and V5-03 are normatively resolved, and V5-02
is materially improved, but the claimed serialization evidence is not yet
schema-complete or envelope-complete.**

Revision 6 now gives Filter None a coherent typed representation when mass is
invalid or unavailable. It correctly keeps the complete canonical IDs,
disables only mass-derived outputs, retains non-mass label contributions, and
separates serialized filter value `none` from proposal outcome `complete`.
The SHA-256 scope, timestamp exclusion, numeric encoding, typed nulls, and
context/proposal fingerprint fields are also meaningful improvements.

The 393-assertion reference file passes. Those passing assertions do not yet
establish the response's claim that complete proposal/3 and view-state/1
records are round-tripped and independently validated:

1. the reference proposal is an abbreviated mock that omits much of the
   normative proposal/3 content, while the normative text itself still does
   not define a closed key-and-type wire schema; and
2. validation, attempt outcome, attempt render outcome, and display source
   are outside every declared fingerprint and are not semantically
   revalidated on deserialization.

An adversarial check changed each of three such envelope fields independently.
`reference_validate_view_state()` accepted all three corrupted objects.

## Blocking Findings

### V6-01 — BLOCKER — Proposal/3 is not a closed wire schema, and the “complete proposal” reference object is incomplete

Audit Charter layer: **Artifacts and provenance**, then **Estimator and
implementation correctness**.

Affected specification sections: **Versioned Proposal Record**,
**Fingerprint contract**, **View-state envelope**, and **Required
Validation**.

The specification says the proposal fingerprint contains every deterministic
scientific or display field in proposal/3 and that each canonical-hash schema
fixes its field set. The proposal definition, however, is still a semantic
bullet list rather than an exact serialized schema. It does not declare the
canonical key path, container type, scalar type, cardinality, or nullability
for each field.

For example, the specification requires all of the following proposal
content:

- separate graph, topology, vertex, field, estimate, source,
  trajectory-flow-construction, and canonical-tree fingerprints;
- component-selection rule, valid component totals, tie-break, fallback
  reason, and direction/component basin counts;
- exact measure names and owning construction identities;
- validated parameters;
- core warnings, boundary, gap, and informational cutoff;
- sentinels with all and primary reasons;
- ancestor-only additions;
- non-overlapping category counts; and
- all mass-derived counts and coverage.

The reference constructor at
`tests/testthat/test-basin-merge-tree-adaptive-filtering-fixture.R:543`
instead creates an abbreviated top-level list containing:

```text
schema
context_fingerprint
proposal_fingerprint
creation_time
context_fields
algorithm
algorithm_version
input_values
validation
mass_derived
label_contributions
label_omission_reasons
core_outcome
core_ids
final_ids
label_ids
render_outcome
```

Most of the required proposal content above is absent. The context test
objects also omit the separately required trajectory-flow construction
identity and canonical-tree construction identity; they carry only the two
construction fingerprints.

`reference_proposal_fingerprint()` hashes whatever fields happen to be in
that abbreviated list. `reference_validate_proposal()` checks the two
digests, but does not check:

- the exact proposal/3 field set;
- required field presence;
- prohibited unknown fields;
- field types, cardinalities, or nullability;
- the proposal schema/version value; or
- the exact context/1 field set.

Consequently, the mass-failure round trip at lines 1347–1442 proves that this
mock R list survives R serialization. It does not prove that either exceptional
mass state has a complete, conforming proposal/3 wire representation. It also
does not prove that two implementations would hash the same conforming
record. The response's statement that the suite constructs and serializes
“complete proposal/3” objects is therefore inaccurate.

Risk:

- implementations can choose different key names and nesting while each
  claims proposal/3 conformance;
- omitted deterministic fields can remain outside a proposal fingerprint;
- a structurally incomplete record can pass reference validation; and
- cross-version or cross-implementation proposal identity is not
  reproducible from the normative document.

Required correction:

1. Define `gflowui_basin_merge_tree_context/1` and
   `gflowui_basin_merge_tree_display_proposal/3` as closed wire schemas. For
   every key path, specify its exact serialized name, type, container,
   cardinality, canonical ordering, and allowed null/empty representation.
2. Include both construction identities and fingerprints in the exact
   context schema, as the current fixed-field list requires.
3. Define how schema evolution handles unknown fields. A proposal/3
   deserializer should reject missing required fields, unknown fields not
   allowed by the version, wrong types, and out-of-domain enum values before
   accepting its digest.
4. Replace `reference_view_proposal()` with a constructor that supplies every
   proposal/3 field, including explicit empty/null values where the schema
   permits them.
5. Assert exact field sets and types in the valid, mass-invalid, and
   mass-unavailable round-trip tests. Add negative tests for missing,
   additional, mistyped, and wrong-version fields.
6. Narrow the response's “complete proposal/3” verification claim until
   those tests pass.

### V6-02 — BLOCKER — Serialized view-state semantics are neither fingerprinted nor recomputed

Audit Charter layer: **Artifacts and provenance**, then **Estimator and
implementation correctness**.

Affected specification sections: **Fingerprint contract**, **View-state
envelope**, **Proposal State Model**, and **Required Validation**.

The active-attempt fingerprint intentionally covers the context and active
input values while excluding computed validation results and displayed state.
The proposal fingerprint covers only the immutable proposal. There is no
view-state fingerprint.

The following serialized fields are therefore outside every digest:

```text
active.attempt.validation
active.attempt.outcome
active.attempt.render.outcome
display.source
```

That exclusion would be safe only if deserialization deterministically
recomputed these fields and rejected disagreements. Revision 6 requires only
recomputation of the context, proposal, and active-attempt fingerprints. The
reference validator at lines 760–785 likewise checks:

- the attempt fingerprint against context plus inputs;
- the displayed proposal fingerprint; and
- the displayed proposal context.

It never verifies the state matrix or recomputes validation and outcomes.

I constructed one valid current view, serialized and deserialized it, then
applied each corruption separately. The existing validator returned:

```text
display_source changed from current to none:       TRUE
active attempt outcome changed to blocked:         TRUE
settings validation changed to settings_invalid:   TRUE
```

Each accepted object contradicts the normative valid-combination table. The
same gap applies to `active.attempt.render.outcome`; combinations such as
`proposal_created` plus `unavailable` are not rejected.

This is not a cryptographic-authentication requirement. It is an internal
consistency requirement for a serializable scientific/display state. A
checksum cannot establish that a caller is trusted, but the deserializer must
at least reject a record whose stored derived state disagrees with its own
inputs and proposal.

Risk:

- a restored state can claim that an invalid attempt was valid, or vice
  versa;
- display provenance can say `none`, `current`, or `retained_last_valid`
  inconsistently with the embedded proposal;
- status text can report validation and render outcomes that were never
  produced by the recorded active inputs; and
- corrupted view state can pass the exact reference validator cited as
  deserialization evidence.

Required correction:

1. Choose and specify one of these coherent contracts:
   - add a versioned `view.state.fingerprint` covering every deterministic
     view-state field, while also enforcing the state matrix; or
   - treat validation, outcomes, and display source as derived fields,
     recompute them from current context, active inputs, and the embedded
     proposal during deserialization, and reject every disagreement.
2. Enforce the complete valid-combination table, including the relationship
   among validation, attempt outcome, attempt render outcome, display source,
   proposal presence, and proposal context.
3. Add independent corruption tests for each envelope field, not only for the
   proposal fingerprint and active inputs.
4. Require `fingerprint_invalid` or a separately named
   `view_state_invalid` result for every inconsistent serialized envelope;
   do not silently normalize it into a valid state.

## Nonblocking Finding

### V6-03 — MINOR — The active-attempt matrix contains a redundant malformed row

Audit Charter layer: **Estimator and implementation correctness**.

The matrix lists both:

```text
peak invalid/source invalid
source invalid
```

The first row conflates a ranking-measure condition with the source-validation
condition, and the second row immediately repeats source invalid. The blocking
precedence and validation map elsewhere are clear enough that this does not
change the contract, but the first row should be renamed `peak invalid` or
removed to avoid implying a combined state.

## Prior-Finding Disposition

### V5-01: substantially resolved in normative text; evidence remains incomplete under V6-01

The typed `valid`, `mass_unavailable`, and `mass_invalid` representations are
coherent. Filter None now produces a current canonical-only proposal with
complete IDs and only the mass-derived contribution omitted. A mass-based mode
correctly remains blocked.

The reference transition genuinely exercises both exceptional mass states,
including old-context replacement. What remains is not the mass-state
semantics themselves, but proof using a complete conforming proposal/3
record.

### V5-02: partially resolved; V6-01 and V6-02 remain

The proposal and context fingerprint fields, SHA-256 scopes, timestamp
exclusion, deterministic map ordering, canonical ID ordering, numeric
encoding, and rejection rather than repair are now specified.

The remaining problems are:

- the hashed proposal/context field sets are not exact closed wire schemas;
- the reference proposal does not contain the declared full field set; and
- mutable/derived view-state semantics are not covered or recomputed.

### V5-03: resolved

The serialized filter value is `none`; the proposal core outcome is
`complete`. Filter None and Show all install the same complete proposal, while
the complete interactive viewer changes only viewer-open state. The reference
test covers renderable and overflow cases.

## Verification

From `/Users/pgajer/current_projects/gflowui`:

```sh
Rscript -e \
  'testthat::test_file("tests/testthat/test-basin-merge-tree-adaptive-filtering-fixture.R")'
```

Result:

```text
PASS 393
FAIL 0
WARN 0
SKIP 0
```

From `/Users/pgajer/current_projects/gflow`, with the package loaded from the
working tree:

```sh
Rscript -e \
  'pkgload::load_all(".", quiet = TRUE); testthat::test_file("tests/testthat/test-basin-merge-tree-public.R")'
```

Result:

```text
PASS 46
FAIL 0
WARN 0
SKIP 0
```

The first direct `test_file()` attempt without `pkgload::load_all()` produced
function-not-found errors because that unsupported invocation did not load
`gflow`; it is not counted as a package failure.

Additional checks:

```text
git diff --check
  PASS

adversarial view-state corruption check
  display.source mutation accepted:             TRUE
  active.attempt.outcome mutation accepted:     TRUE
  active-attempt validation mutation accepted:  TRUE
```

## Acceptance Gate

Do not begin implementation from Revision 6 as an accepted specification.
Revise the serialization contract and reference evidence until:

1. context/1 and proposal/3 have exact closed wire schemas;
2. reference tests construct and validate every required proposal field;
3. deserialization rejects every inconsistent view-state combination; and
4. the focused regression suite remains green.

This remains a specification-phase verdict. It makes no claim about
application implementation, the future public `gflow` filtered-layout
accessor, or scientific acceptance of adaptive filtering or EOD
interpretation.
