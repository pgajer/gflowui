# Response to the Adaptive Initial Filtering Revision 6 Re-audit

## Disposition

Revision 7 of
`dev/basin_merge_tree_adaptive_initial_filtering_spec_2026-07-31.md`
addresses V6-01 through V6-03 from the Revision 6 re-audit.

This response records specification and reference-test changes. It does not
claim auditor acceptance, application implementation, implementation of the
required public `gflow` filtered-layout accessor, or scientific acceptance.

## Finding Responses

### V6-01: Closed context/1 and proposal/3 wire schemas

Addressed in **Closed wire-schema rules**, **Context/1 schema**,
**Proposal/3 schema**, **Fingerprint contract**, and **Required Validation**.

Context/1 and proposal/3 are now closed schemas. The specification declares
every serialized key, scalar or container type, cardinality, enum, canonical
ordering, and allowed null or empty representation. Missing fields, additional
fields, wrong types, invalid enums, wrong versions, and noncanonical ID arrays
are rejected with `schema_invalid` before fingerprint validation. Schema
evolution requires a new terminal version; proposal/3 has no extension keys.

Context/1 now explicitly requires both the identity and fingerprint for the
trajectory-flow construction and canonical-tree construction.

Proposal/3 has an exact top-level field set and closed nested objects for:

- algorithm identity;
- component selection and component totals;
- measure names and owners;
- accepted validation;
- canonical mapping;
- all accepted parameters;
- typed mass-derived groups, counts, denominator, and coverage;
- core outcome, warnings, boundary, gap, cutoff, and IDs;
- sentinel IDs, all/primary reasons, and counts;
- ancestor additions; and
- final IDs, label contributions and omissions, category counts, and render
  outcome.

The only dynamic-key objects are the explicitly declared sentinel reason maps.

The reference constructor now populates every proposal/3 field, including
explicit null and empty values. The structure validator checks exact field
sets and types before recomputing context and proposal fingerprints.
Round-trip tests cover valid, `mass_invalid`, and `mass_unavailable`
proposals. Negative tests cover missing, additional, mistyped, and
wrong-version proposal fields, plus missing, additional, and wrong-version
context fields.

### V6-02: View-state integrity and semantic validation

Addressed in **Fingerprint contract**, **View-state envelope**,
**Proposal State Model**, and **Required Validation**.

View-state/1 is now a closed schema and contains
`view_state_fingerprint`. That SHA-256 digest covers every deterministic
envelope field except itself, including:

```text
active.attempt.validation
active.attempt.outcome
active.attempt.render.outcome
display.source
display.proposal.fingerprint
display.proposal
```

Deserialization validates the closed schemas, recomputes context, proposal,
attempt, and view-state fingerprints, and then enforces the complete state
matrix. In particular:

- `proposal_created` requires null attempt render outcome, current display,
  and a matching proposal;
- `stale` requires stale render outcome and no display;
- `blocked` requires unavailable render outcome;
- retained-last-valid is limited to same-context settings-invalid attempts
  with otherwise valid measurements; and
- current or retained display requires a matching embedded proposal and
  fingerprint.

Fingerprint disagreement returns `fingerprint_invalid`. A semantically
impossible but consistently re-fingerprinted envelope returns
`view_state_invalid`. Neither path repairs or normalizes the record.

Tests independently mutate display source, attempt outcome, attempt render
outcome, and attempt validation. Each isolated mutation fails fingerprint
validation. Each mutation is then deliberately re-fingerprinted and still
fails state-matrix validation. View-state tests also reject missing,
additional, and wrong-version envelope fields.

### V6-03: Redundant peak/source matrix row

Addressed in **Proposal State Model**.

The malformed `peak invalid/source invalid` row is now `peak invalid`.
The separate `source invalid` row remains, preserving the distinction between
mapped peak-ranking validation and source-field validation.

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

`git diff --check` also passes.
