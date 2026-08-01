# Response to the Adaptive Initial Filtering Revision 7 Re-audit

## Disposition

Revision 8 of
`dev/basin_merge_tree_adaptive_initial_filtering_spec_2026-07-31.md`
addresses V7-01 through V7-04 from the Revision 7 re-audit.

This response records specification and executable reference-test changes. It
does not claim auditor acceptance, application implementation, implementation
of the required public `gflow` filtered-layout accessor, or scientific
acceptance.

## Finding Responses

### V7-01: Raw UI state and accepted proposal parameters

Addressed in **Parameter Validation**, **Proposal/3 schema**, **View-state
envelope**, and **Required Validation**.

The complete raw control snapshot and the proposal's validated inputs are now
different closed objects:

- ActiveInput retains every control using closed tagged numeric-input objects.
- `accepted_parameters` contains only the common validated settings and the
  settings used by the active mode.
- `proposal_created` compares the proposal parameters with the normalized
  active/common projection, not the complete raw snapshot.

NumericControlInput has exactly `state` and `payload`. Integer and binary64
controls have distinct parsed states. Missing, nonfinite, and unparsable
states have exact payload rules; untagged values, arbitrary nested objects,
vectors, additional keys, and state/control mismatches are rejected.
Unparsable UTF-8 payloads are preserved byte-for-byte and framed only by the
canonical string encoder's byte-length prefix.

The integrated regression loops over all five modes. For every inactive
mode-specific control, it installs missing, nonfinite, unparsable, and
domain-invalid values and verifies:

- successful proposal construction;
- preservation of the complete raw ActiveInput;
- omission of the inactive field from accepted parameters;
- valid attempt, proposal, and view-state fingerprints;
- successful view-state validation; and
- `settings_invalid` when that field's mode is subsequently activated.

Separate negative tests reject malformed ActiveInput objects and token
payloads.

### V7-02: One canonical scalar encoding

Addressed in **Closed wire-schema rules**, **Fingerprint contract**, and
**Required Validation**.

Wire serialization is now schema-directed. Signed-64 integers use a canonical
decimal-character implementation carrier and an integer wire token. The
grammar, signed-64 bounds, and forbidden alternatives are explicit. R integer
or double values in integer fields are rejected.

Number fields accept only finite R doubles and serialize as lowercase C99
hexadecimal binary64. An R integer in a number field is rejected. Negative
zero is normalized to positive zero. The encoder no longer chooses a token
from incidental runtime storage.

Fixed digest vectors now cover:

- integer zero;
- both signed-64 integer boundaries;
- binary64 zero and negative zero;
- an integer-looking binary64 value; and
- alternate runtime-type rejection in context and proposal fields.

The earlier `component = 1L` versus `component = 1` ambiguity is therefore
closed: neither is a conforming wire integer; only canonical decimal integer
text is accepted by the R reference representation.

### V7-03: Relational validation of mass-derived records

Addressed in **Proposal/3 schema**, **Fingerprint contract**, and **Required
Validation**.

Mapping now serializes `component_ids`, the complete mapped canonical ID
universe for the selected component. Proposal validation no longer infers the
universe from final IDs or mass groups.

For valid mass, deserialization reconstructs and checks:

- a disjoint, complete all-mass partition of `mapping.component_ids`;
- strictly descending distinct group masses and exact cumulative endpoints;
- the exact positive restriction, preserving group order, masses, IDs, and
  endpoints;
- positive and zero membership counts;
- the fixed-order positive-mass denominator;
- every cumulative group coverage;
- core and final coverage against the same denominator; and
- membership of all core, sentinel, ancestor, final, label, contribution,
  reason-map, and survivor IDs in the declared universe.

The all-zero mass-unavailable representation is checked against the same
universe. The universe remains available for membership checks when mass is
invalid.

The adversarial tests mutate and consistently re-fingerprint each of the
following independently: endpoint, duplicate ID, missing ID, group mass,
denominator, positive count, zero count, cumulative coverage, core coverage,
final coverage, and component universe. Every record is rejected with
`schema_invalid`; a recomputed digest cannot legitimize false derived
measurements.

### V7-04: Hash-count wording

Addressed in **Fingerprint contract**.

“All three hashes” is corrected to “All four hashes,” covering context,
proposal, active-attempt, and view-state digests.

## Verification

From `/Users/pgajer/current_projects/gflowui`:

```sh
Rscript -e \
  'testthat::test_file("tests/testthat/test-basin-merge-tree-adaptive-filtering-fixture.R")'
```

Result:

```text
PASS 811
FAIL 0
WARN 0
SKIP 0
```

`git diff --check` also passes.

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
