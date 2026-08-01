# Response to the Adaptive Initial Filtering Revision 5 Re-audit

## Disposition

Revision 6 of
`dev/basin_merge_tree_adaptive_initial_filtering_spec_2026-07-31.md`
addresses V5-01 through V5-03 from the Revision 5 re-audit.

The specification also retains the requested first-use terminology:
the canonical rule is the filtration-value elder rule, specialized here as
the density-value elder rule. Mass and support remain display-ranking
measures, not merge-survivor rules.

This response records specification and reference-test changes. It does not
claim auditor acceptance, application implementation, or implementation of
the required public `gflow` filtered-layout accessor.

## Finding Responses

### V5-01: Filter None under invalid or unavailable mass

Addressed in **Typed mass-derived availability**, **Label Policy**,
**Versioned Proposal Record**, **View-state envelope**, **Proposal State
Model**, and **Required Validation**.

Every successful proposal now contains typed mass-derived availability and an
explicit unavailability reason. The exact field-level contract is:

| Mass state | Positive groups | All-mass groups | Denominator | Positive count | Zero count | Coverage |
|---|---|---|---|---|---|---|
| `valid` | complete | complete | finite positive | exact | exact | finite |
| `mass_unavailable` | empty list | one complete zero group | `0` | `0` | branch count | null |
| `mass_invalid` | null | null | null | null | null | null |

No unavailable value is represented by `NaN` or fabricated zero coverage.
Filter None preserves complete core and final IDs in both exceptional mass
states. Its mass-ranked Important-label contribution is empty with a typed
omission reason, while valid peak, prominence, support, survivor, and selected
contributions remain active.

The view transition is now mode-aware. A mass-invalid or mass-unavailable
Filter None attempt with otherwise valid inputs records
`proposal_created`, installs one current immutable canonical-only proposal,
and retains its true render outcome. The same mass state in a mass-based mode
is blocked and displays no proposal. A mass-source change invalidates the
prior retained proposal before either path is evaluated.

The reference suite constructs, serializes, installs, and revalidates complete
proposal/3 and view-state/1 objects for both mass states. It asserts every
typed mass field, omission reason, surviving non-mass labels, complete IDs,
active outcome, display source, and render outcome.

### V5-02: Proposal and view fingerprint invariants

Addressed in **Versioned Proposal Record**, the new **Fingerprint contract**,
**View-state envelope**, and **Required Validation**.

Proposal/3 now explicitly contains:

```text
context.fingerprint
proposal.fingerprint
```

The context fingerprint covers project and subject identity; graph, topology,
and vertex map; selected field and source; estimate; trajectory-flow and
canonical constructions; direction; and component.

The proposal fingerprint covers every deterministic proposal field and
excludes only itself and creation time. The active-attempt fingerprint covers
the context, exact serialized filter mode, active inputs, and
validation-relevant settings. The specification defines SHA-256 canonical
serialization, including fixed schema order, canonical ID ordering, UTF-8
strings, canonical base-10 integers, C99 hexadecimal floating-point values,
typed nulls, and normalized zero.

Deserialization independently recomputes all fingerprints. A mismatch returns
`fingerprint_invalid`, clears the display, and is never repaired in place.

Reference tests now recompute fingerprints rather than accepting caller
labels. They cover reordered context and input maps, timestamp-only proposal
changes, proposal-field tampering, wrong-context content, corrupted envelope
fingerprints, corrupted active inputs, and serialization round-trips.

### V5-03: Complete-tree filter state versus core outcome

Addressed in **Parameter Validation**, **User Interface**, and **Required
Validation**, with the reference helper corrected accordingly.

The persistent Filter None value is serialized as `none`. Its proposal core
outcome is `complete`; those values are no longer conflated. Direct
Filter=None and Show all install the same complete immutable proposal,
including its context and proposal fingerprints. Open complete interactive
tree remains a nonmutating viewer action.

The reference test asserts filter value, core outcome, embedded proposal,
fingerprints, and viewer nonmutation in both renderable and overflow states.

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

`git diff --check` also passes.
