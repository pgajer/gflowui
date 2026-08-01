# Response to the Adaptive Initial Filtering Revision 4 Re-audit

## Disposition

Revision 5 of
`dev/basin_merge_tree_adaptive_initial_filtering_spec_2026-07-31.md`
addresses V4-01 through V4-03 from the Revision 4 re-audit.

This response records specification and reference-test changes. It does not
claim auditor acceptance, application implementation, or implementation of the
required public `gflow` layout accessor.

## Finding Responses

### V4-01: Mandatory ranking-measure validation

Addressed in **Version 1 Measure Contract**, **Direction and Component Scope**,
**Source and Ranking-Measure Validation**, **Mandatory Sentinels**, **Label
Policy**, **Required Public `gflow` Layout Contract**, **Versioned Proposal
Record**, **Proposal State Model**, and **Required Validation**.

All four ranking vectors are validated after one-to-one canonical mapping and
across the whole selected direction before component selection:

- trajectory-flow mass must be finite and nonnegative;
- trajectory-flow support size must be finite, nonnegative, and whole-number;
- mapped source peak must be finite; and
- canonical prominence must be finite and nonnegative.

Zero support and zero prominence are valid. Missing values are not silently
omitted, and fractional support is neither rounded nor coerced. The elder-rule
survivor uses finite persistence from its birth to the selected-field minimum
of its component; `NA` and infinity are not survivor conventions.

The immutable proposal preserves its accepted typed validation map, and the
active attempt records invalid mass, support, peak, or prominence states.
`support_invalid`, `peak_invalid`, and
`prominence_invalid` block every filter mode in version 1 and produce no core,
sentinel, label, final, or layout IDs. The existing canonical-only Filter None
exception remains limited to mass-invalid or mass-unavailable state, with all
mass-derived views disabled and disclosed.

The public `gflow` layout contract now requires validation of finite branch
birth, death, and persistence and nonnegative persistence for the complete
canonical tree, including component survivors.

Reference tests cover missing, nonfinite, negative, fractional, and zero
support; missing, nonfinite, negative, zero, and survivor prominence; missing
and nonfinite peak; blocking outputs; and the mass-only Filter None exception.
The Subject 15 fixture directly validates all four ranking domains.

### V4-02: Active attempt versus retained visible proposal

Addressed in **Versioned Proposal Record**, **View-state envelope**,
**Proposal State Model**, and **Required Validation**.

A successful algorithm proposal is now immutable and serializable as
`gflowui_basin_merge_tree_display_proposal/3`. An invalid active attempt is not
a proposal and never receives prior canonical IDs.

Transient state is separately serializable as
`gflowui_basin_merge_tree_view_state/1`, with:

```text
context.fingerprint
active.attempt.fingerprint
active.attempt.validation
active.input.values
active.attempt.outcome
active.attempt.render.outcome
display.source
display.proposal.fingerprint
display.proposal
```

The specification defines current, retained-last-valid, absent, blocked,
recovered, stale, and context-change combinations. Retention is allowed only
for invalid parameter edits in an unchanged context while the retained
proposal independently revalidates. Invalid source, mapping, ranking measure,
or identity clears the retained proposal. A later valid recomputation
atomically replaces it.

Active-input status reports the current invalid controls. Displayed-proposal
status is derived only from the immutable proposal that is actually visible.
The blocked attempt never borrows that proposal's settings, IDs, or render
outcome.

The reference tests now exercise:

- initial valid computation;
- same-context invalid-setting retention;
- invalid setting without a prior proposal;
- full active-attempt validation fields;
- proposal and view fingerprints;
- separation of active and retained input values;
- serialization round-trip;
- valid recovery and replacement;
- source, mapping, mass, support, peak, prominence, and stale invalidation; and
- context-change invalidation.

The prior Revision 4 tests established validation/core/render precedence but
did not establish this view-state relationship. Revision 5 narrows that
historical claim and adds the missing executable transition evidence.

### V4-03: Complete-tree control semantics

Addressed in **Other filter modes**, **Final Rendering Budget and Overflow**,
**User Interface**, and **Required Validation**.

- Filter None is the persistent complete-core filter state.
- Show all is a shortcut that sets Filter to None and recomputes; it is not a
  temporary visual override.
- Open complete interactive tree is a viewer action. Launching it changes no
  filter mode, manual setting, selected ID, active attempt, retained proposal,
  or static render outcome.

Reference tests exercise all three actions in both renderable and
`core_overflow` states.

## Verification

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

`git diff --check` also passes.
