# Adaptive Initial Filtering Revision 7: Re-audit

Date: 2026-08-01

Auditor role: independent specification re-auditor

Audited response:
`/Users/pgajer/current_projects/gflowui/dev/audits/basin_merge_tree_adaptive_initial_filtering_revision6_reaudit_response_2026-08-01.md`

Revised specification:
`/Users/pgajer/current_projects/gflowui/dev/basin_merge_tree_adaptive_initial_filtering_spec_2026-07-31.md`

Revision 6 re-audit:
`/Users/pgajer/current_projects/gflowui/dev/audits/basin_merge_tree_adaptive_initial_filtering_revision6_reaudit_2026-08-01.md`

Pinned revisions:

```text
gflowui: 3486504425cd79dd715b6286c4d0de2a5191cb55
gflow:   24a671c4927df6ab6e5ac10361aecfd87cfaa0cb
upstream scientific repository:
         4615555547f3f406e79436c308d28fd78985b64e
```

The unrelated untracked gflowui audit, the pre-existing local gflow
`AGENTS.md` change, and the pre-existing scientific-repository changes were
not part of the assessed revision.

## Verdict

**Phase verdict: Revise before acceptance.**

**Response disposition: V6-02 and V6-03 are resolved. V6-01 is substantially
improved but not closed because parameter state, wire scalar types, and
mass-derived relational fields still admit contradictory or
non-reproducible records.**

Revision 7 replaces the abbreviated proposal mock with a much more complete
proposal/3 object. It specifies exact top-level and nested key sets, adds the
two missing construction identities, validates envelope fingerprints, and
enforces the main view-state matrix after fingerprint validation. The
previously accepted mutations of display source, attempt outcome, attempt
render outcome, and attempt validation are now rejected both before and after
deliberate re-fingerprinting.

The 424-assertion reference suite passes. Three independent adversarial checks
nevertheless show that the claimed closed-schema evidence is not yet
sufficient:

1. a successful Filter None proposal cannot represent an inactive invalid
   Top K value even though inactive values are explicitly retained and cannot
   block the active mode;
2. values accepted as the same declared wire integer can be serialized with
   different type tokens and therefore receive different fingerprints; and
3. incorrect mass-group endpoints and cumulative coverage remain valid after
   the proposal is consistently re-fingerprinted.

## Blocking Findings

### V7-01 — BLOCKER — Proposal parameters contradict mode-aware inactive-input semantics

Audit Charter layer: **Estimator and implementation correctness**, then
**Artifacts and provenance**.

Affected specification sections: **Parameter Validation**, **Proposal/3
schema**, **View-state envelope**, and **Required Validation**.

The pre-existing mode contract is explicit:

- inactive mode-specific values are retained;
- inactive values are not validated; and
- inactive values cannot block the active mode.

Revision 7 introduces three requirements that do not compose with that
contract:

1. proposal/3 always contains a complete `accepted_parameters` object with
   every Parameters key;
2. the proposal Parameters object always satisfies the stricter parameter
   domains; and
3. a `proposal_created` view requires proposal `accepted_parameters` to equal
   the complete active-attempt `input_values` object.

The contradiction is visible with Filter None and an inactive `top_k = 0`.
The parameter table permits Top K's positive constraint only when Top K is
active, so this value must not block Filter None. The reference validator at
`tests/testthat/test-basin-merge-tree-adaptive-filtering-fixture.R:618`
nevertheless requires positive `top_k` in every proposal mode.

I constructed a Filter None proposal with `top_k = 0L`. The result was:

```text
active filter: none
inactive top_k: 0
proposal validation: schema_invalid
```

The same problem applies more strongly to missing, nonfinite, fractional, or
unparsable inactive values. ActiveInput purports to preserve those raw
values, while proposal Parameters cannot contain them and the view matrix
requires exact equality between the two objects.

The ActiveInput wire type is also not actually closed. It says invalid numeric
controls may use a “typed raw-input string token,” but it defines neither the
token grammar nor the allowed type union for each key. The reference
structural validator checks only filter mode, label mode, and the three
logical toggles. It accepts arbitrary R objects in every numeric field. A
blocked view with this active input passed full view-state validation:

```text
coverage_target = list(arbitrary = TRUE)
reference_validate_view_state(...) = TRUE
```

Risk:

- switching away from an invalid Top K or Minimum Mass control can still
  prevent a proposal despite the declared inactive-input rule;
- a successful current proposal cannot preserve the exact active control
  snapshot required by the view schema;
- attempt fingerprints are not portable because raw-input token forms are
  unspecified; and
- a supposedly closed ActiveInput object accepts arbitrary nested containers.

Required correction:

1. Separate the full raw UI snapshot from the validated parameters that
   influence the current proposal.
2. Define proposal `accepted_parameters` as either:
   - an exact mode-specific object containing only active and common validated
     settings; or
   - a full object whose inactive fields have explicitly typed
     `inactive`/raw representations and whose domain validation is
     mode-aware.
3. Replace whole-object equality in the `proposal_created` matrix with an
   exact comparison of the validated active/common projection, unless the
   second design above is chosen.
4. Define a closed tagged union for every ActiveInput key. Specify distinct
   canonical tokens for missing, nonfinite, and unparsable values, including
   exact payload and escaping rules.
5. Add integrated proposal/view tests for every mode with invalid values in
   each inactive mode-specific control. Assert proposal creation, preserved
   raw UI state, accepted active parameters, fingerprints, switching, and
   later activation failure.
6. Add negative ActiveInput schema tests for arbitrary lists, objects,
   vectors, untagged strings, and wrong token payloads.

### V7-02 — BLOCKER — Declared integer and number types do not have one canonical runtime encoding

Audit Charter layer: **Artifacts and provenance**.

Affected specification sections: **Closed wire-schema rules** and
**Fingerprint contract**.

The wire schema distinguishes:

```text
integer  -> canonical base-10 integer token
number   -> canonical C99 hexadecimal binary64 token
```

The reference validators do not enforce that distinction. In particular,
`reference_is_integer()` accepts any finite whole-valued R numeric, including
both integer storage (`1L`) and double storage (`1`). Conversely,
`reference_is_number()` accepts both storage modes.

The canonical serializer dispatches on the R runtime type:

- `1L` receives an integer token; and
- `1` receives a binary64 token.

I changed only context `component` from `1L` to `1`. Both objects passed the
closed context/1 validator, but their fingerprints differed:

```text
integer-storage context valid: TRUE
double-storage context valid:  TRUE
fingerprints identical:        FALSE
```

Thus two objects accepted as the same declared context/1 value do not have one
canonical fingerprint. The same ambiguity affects every proposal integer and
number field.

There is an additional precision issue in the claimed signed 64-bit integer
domain: an ordinary R double cannot exactly represent every signed 64-bit
integer. A whole-valued-double test is therefore not a sufficient
implementation of the declared wire type.

Risk:

- semantically identical conforming records receive different fingerprints;
- fingerprints depend on incidental R storage mode rather than the wire
  schema;
- cross-language implementations cannot reproduce the reference digest; and
- large integer values can be rounded before serialization.

Required correction:

1. Make canonical serialization schema-directed rather than dispatching on
   incidental R storage type.
2. Choose an exact R representation for signed 64-bit integers, such as a
   validated decimal-character form or a lossless integer64 representation.
3. Reject a value whose runtime representation does not match its declared
   wire type, or normalize it losslessly into the declared type before
   hashing. Do not allow both paths to emit different tokens.
4. Add fixed digest vectors shared across integer and binary64 fields,
   including `0`, negative zero for number fields, boundary integers, and
   integer-looking binary64 values.
5. Add tests showing that every accepted representation of one schema value
   has exactly one digest; otherwise reject the alternate representation with
   `schema_invalid`.

### V7-03 — BLOCKER — Proposal validation accepts internally false mass-derived measurements

Audit Charter layer: **Measurement**, then **Artifacts and provenance**.

Affected specification sections: **Source and Ranking-Measure Validation**,
**Proposal/3 schema**, **Fingerprint contract**, and **Required Validation**.

The specification correctly declares that:

- group endpoints are cumulative member counts;
- positive cumulative coverage is derived from the declared denominator in
  fixed group order;
- positive and all-mass groups are exact;
- positive and zero counts are exact; and
- core and final coverage use the same declared denominator.

The reference validator checks only that endpoints and coverage values are in
range and monotone. It does not establish the required equalities. For
example, it does not require:

- each endpoint to equal the cumulative number of preceding and current group
  IDs;
- positive groups to equal the positive subset of all-mass groups;
- group ID sets to be disjoint and complete;
- denominator to equal the fixed-order sum of group mass times group size;
- positive and zero counts to equal group membership counts; or
- cumulative, core, and final coverage to equal the corresponding mass sums.

I changed a valid three-branch proposal in two independent ways, recomputed
the proposal fingerprint after each change, and ran full proposal validation:

```text
single group endpoint changed from 3 to 2:
  reference_validate_proposal(...) = TRUE

single positive-group cumulative coverage changed from 1 to 0.5:
  reference_validate_proposal(...) = TRUE
```

These are not merely cryptographic tampering cases. A digest proves only that
content has not changed since the digest was computed; it cannot make false
derived measurements valid. The deserializer must enforce the declared
scientific relationships after structural and fingerprint checks.

Risk:

- proposal records can report incorrect coverage while passing all current
  validation;
- boundary ranks can disagree with serialized group membership;
- mass-invalid or mass-unavailable field tables can appear structurally valid
  while carrying inconsistent endpoints; and
- downstream status text and diagnostics can trust internally false
  measurements.

Required correction:

1. Define and enforce exact relational invariants for positive and all-mass
   groups.
2. Recompute endpoints, denominator, positive/zero counts, cumulative
   coverage, core coverage, and final coverage from serialized groups and ID
   memberships in the specified fixed order.
3. Require the all-mass groups to form one disjoint complete partition of the
   mapped selected component, and positive groups to be its exact positive
   restriction.
4. Validate core, final, label, sentinel, and ancestor IDs against that
   declared component ID universe.
5. Add independently re-fingerprinted negative tests for incorrect
   endpoints, duplicate/missing IDs, changed group mass, denominator, counts,
   cumulative coverage, and core/final coverage.

## Nonblocking Finding

### V7-04 — MINOR — The fingerprint prose still says “all three hashes”

Audit Charter layer: **Artifacts and provenance**.

The contract now defines context, proposal, active-attempt, and view-state
hashes, but the canonical-serialization paragraph says “All three hashes.”
This should say “All four hashes” to avoid ambiguity about whether the new
view-state digest uses the same canonical encoder.

## Prior-Finding Disposition

### V6-01: partially resolved; V7-01 through V7-03 remain

Revision 7 now supplies exact context and proposal key inventories and a
substantially complete proposal constructor. Missing, additional, mistyped,
and wrong-version top-level examples are rejected.

The remaining defects are narrower but material:

- the parameter wire objects do not implement mode-aware inactive state;
- declared scalar types do not map to one canonical encoding; and
- important derived mass relationships are not validated.

### V6-02: resolved

The view-state fingerprint covers the complete envelope, including validation,
outcomes, display source, display fingerprint, and proposal. The validator
also enforces the central state matrix after digest checks. Isolated mutations
return `fingerprint_invalid`; consistently re-fingerprinted contradictions
return `view_state_invalid`.

I independently confirmed all four mutations covered by the response.

### V6-03: resolved

The malformed matrix row is now `peak invalid`, distinct from the separate
`source invalid` row.

## Independent Reproduction

From the committed Subject 15 fixture, independently of the reference proposal
constructor, I reproduced:

```text
maximum branches:              352
positive-mass branches:        352
required coverage endpoint:     17
first eligible strong gap:      17
coverage at rank 17:
  0.99999999999991729
gap at rank 17:
  12.939763129977104 decades
```

These values agree with the existing scientific fixture claims. Revision 7
does not change the filtering rule or the Subject 15 result; the current
findings concern serialization and validation of proposal state.

## Verification

From `/Users/pgajer/current_projects/gflowui`:

```sh
Rscript -e \
  'testthat::test_file("tests/testthat/test-basin-merge-tree-adaptive-filtering-fixture.R")'
```

Result:

```text
PASS 424
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

Additional checks:

```text
git diff --check
  PASS

Filter None with inactive top_k = 0
  proposal validation: schema_invalid

arbitrary list-valued numeric ActiveInput
  full view-state validation: TRUE

context component 1L versus 1
  both schema-valid: TRUE
  fingerprints equal: FALSE

re-fingerprinted wrong mass-group endpoint
  full proposal validation: TRUE

re-fingerprinted wrong cumulative coverage
  full proposal validation: TRUE
```

## Acceptance Gate

Do not treat Revision 7 as an accepted implementation specification. Revise
the schema and reference evidence until:

1. inactive invalid inputs coexist with successful proposals in every other
   active mode without violating proposal/view equality;
2. ActiveInput raw tokens have exact closed tagged representations;
3. each accepted wire scalar has one schema-directed canonical encoding;
4. derived mass fields are recomputed and relationally validated; and
5. the focused and adversarial regression suites pass.

This remains a specification-phase verdict. It makes no claim about
application implementation, the future public `gflow` filtered-layout
accessor, or scientific acceptance of adaptive filtering or EOD
interpretation.
