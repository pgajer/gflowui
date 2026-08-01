# Adaptive Initial Filtering Revision 4: Re-audit

Date: 2026-08-01

Auditor role: independent specification re-auditor

Audited response:
`/Users/pgajer/current_projects/gflowui/dev/audits/basin_merge_tree_adaptive_initial_filtering_spec_audit_response_followup_reaudit_response_2026-08-01.md`

Revised specification:
`/Users/pgajer/current_projects/gflowui/dev/basin_merge_tree_adaptive_initial_filtering_spec_2026-07-31.md`

Follow-up re-audit:
`/Users/pgajer/current_projects/gflowui/dev/audits/basin_merge_tree_adaptive_initial_filtering_spec_audit_response_followup_reaudit_2026-08-01.md`

Pinned revisions:

```text
gflowui: 4b7610476a91dfd371a5f494f3c431d421685689
gflow:   24a671c4927df6ab6e5ac10361aecfd87cfaa0cb
upstream scientific repository:
         4615555547f3f406e79436c308d28fd78985b64e
```

The unrelated untracked gflowui audit, the pre-existing local gflow
`AGENTS.md` change, and the pre-existing scientific-repository changes were
not part of the assessed revision.

## Verdict

**Phase verdict: Revise before acceptance.**

**Response accuracy: accepted for FR-01 through FR-04.**

Revision 4 correctly resolves the prior manual-mode, orthogonal-status,
nominal-Top-N, and accessor-naming findings as far as those findings went. The
new manual-mode examples are executable and pass. The response also remains
appropriately limited: it does not claim implementation or auditor acceptance.

Fresh review from the measure contract outward found two remaining
specification blockers:

1. mandatory sentinel and label rankings use trajectory-flow support size and
   canonical prominence without validating either input or defining failure
   behavior; and
2. the proposal record does not define how an invalid current attempt and a
   separately retained last-valid view coexist, despite exposing one
   `proposal.availability` field.

These issues can change final canonical IDs or serialize incompatible UI
states. They should be resolved before implementation rather than delegated
to the Shiny and proposal-helper code.

## Blocking Findings

### V4-01 — BLOCKER — Mandatory non-mass ranking measures lack validation and failure semantics

Audit Charter layer: **Measurement**, then **Estimator and implementation
correctness**.

Affected specification sections: **Version 1 Measure Contract**, **Source and
Mass Validation**, **Mandatory Sentinels**, **Label Policy**, **Versioned
Proposal Record**, **Proposal State Model**, and **Required Validation**.

The measure contract declares four ranking inputs:

- trajectory-flow `primary.support.mass`;
- trajectory-flow `primary.support.size`;
- selected-field peak value; and
- canonical persistence/prominence.

Peak values are protected by the requirement that the selected source field
contain one finite value per graph vertex. Mass has explicit missing,
negative, nonfinite, and unavailable states. There is no equivalent validation
for `primary.support.size` or canonical prominence.

Both unvalidated measures are operationally mandatory:

- the sentinel union always includes Top-N support and prominence branches
  when their toggles are active; and
- Important labels include Top-N support and prominence branches.

The proposal schema records source, mapping, mass, and settings validation, but
no support-size, prominence, or general ranking-measure validation. The state
precedence consequently has no outcome for an invalid mandatory sentinel or
label measure.

This is reachable independently of the valid Subject 15 fixture. For example,
with IDs `a`, `b`, and `c`, support values `10, NA, 5`, and N = 2, a typical R
ranking implementation silently returns `a,c`; another implementation can
reject the vector or propagate an unknown ID. With `10, Inf, 5`, the nonfinite
entry may instead rank first. A negative or fractional support count is also
not covered by any declared domain.

Canonical-tree validity does not close this gap in the current pinned gflow
implementation. Its schema validator checks table structure and branch/event
identity but does not establish that every prominence value is finite and
nonnegative.

Risk:

- mandatory sentinel and Important-label IDs can vary across implementations;
- an invalid value can be silently dropped, ranked first, or cause an error;
- category counts, ancestor closure, final IDs, and overflow cause can all
  change; and
- a proposal can report all declared validation axes as valid despite using an
  invalid ranking input.

Required correction:

1. Define exact validity domains for every ranking measure after canonical
   mapping:
   - peak values finite in the selected source field;
   - trajectory-flow support sizes finite, nonnegative whole numbers; and
   - canonical prominence values finite and nonnegative, including an explicit
     convention for the component survivor.
2. Add independent validation fields or a typed per-measure validation map to
   proposal schema version 2.
3. Define blocking precedence. The recommended version-1 rule is to block the
   current coordinated proposal when any required ranking vector is invalid,
   because default mandatory sentinels, Important labels, and the Plot
   Workspace rely on those measures. If measure-specific degradation is
   preferred, specify it by active sentinel toggle and label mode and disclose
   every omitted protection.
4. State whether validation runs across the whole mapped direction or only the
   selected component. Whole-direction validation is simpler and consistent
   with the existing mass/mapping pre-pass; selected-component validation is
   acceptable only if component switching revalidates before use.
5. Add missing, nonfinite, negative, fractional-support, zero-support, and
   invalid-prominence tests. Assert validation fields, proposal availability,
   sentinel and label behavior, final IDs, and render outcome.
6. Extend the public gflow layout/tree validation contract if finite canonical
   prominence is intended to be a canonical-tree invariant.

### V4-02 — BLOCKER — Retained-last-valid presentation is not represented as a deterministic two-state transition

Audit Charter layer: **Artifacts and provenance**, then **Estimator and
implementation correctness**.

Affected specification sections: **Parameter Validation**, **Versioned
Proposal Record**, **Proposal State Model**, and **Required Validation**.

The specification requires all of the following:

- an invalid active setting prevents a new current core;
- the invalid attempt has `settings_invalid` and render outcome
  `unavailable`;
- the UI continues displaying the last valid proposal;
- the retained view is marked `retained_last_valid`; and
- cached IDs are never relabeled as current.

One proposal record cannot express those facts without an additional
relationship. The invalid attempt has no core or final IDs. The retained valid
proposal has valid settings, a nonnull core, final IDs, and its previous render
outcome. The current text does not say whether:

1. the prior immutable proposal is mutated from `current` to
   `retained_last_valid`;
2. a new invalid proposal copies the prior IDs while carrying
   `settings_invalid`;
3. a new unavailable attempt is stored separately from the retained proposal;
   or
4. `proposal.availability` belongs to UI view state rather than to the
   serialized algorithm proposal.

These choices produce different serialized records and different answers to
which validation fields and render outcome describe the visible tree.

The focused reference test does not resolve the ambiguity. Its
`reference_proposal_state()` result contains identity, source, mapping, mass,
settings, core outcome, and render outcome, but it does not contain
`proposal_availability`, retained proposal identity, retained IDs, or a link
between the invalid attempt and visible snapshot. The test at lines 475-524
therefore exercises blocking precedence but not the retained-last-valid state
claimed by proposal schema version 2.

Risk:

- invalid inputs can be serialized together with stale prior IDs as though
  they formed one proposal;
- the visible tree's settings and render outcome can be mislabeled;
- a retained view can survive a construction or identity change without an
  explicit invalidation edge; and
- workers can implement incompatible session-state and serialization models
  while passing the current reference test.

Required correction:

1. Keep the algorithm proposal immutable and move display availability into an
   explicit view-state envelope. Recommended fields are:

   ```text
   active.attempt.validation
   active.input.values
   display.source: current | retained_last_valid | none
   display.proposal.fingerprint
   display.proposal
   ```

   An equivalent two-record design is acceptable if the relationship is
   explicit.
2. Define valid combinations for current, retained, absent, unavailable, and
   stale states. Do not attach prior canonical IDs to the invalid current
   attempt.
3. Define how a newly valid recomputation replaces the retained view and how
   graph, source, construction, direction, or component changes invalidate
   both the active attempt and retained proposal.
4. Specify which status line reports the invalid active input and which fields
   describe the retained visible tree.
5. Extend the reference state helper and tests to include display source,
   proposal fingerprint, current versus retained IDs, transition ordering,
   serialization round trips, recovery after valid input, and stale-identity
   invalidation.
6. Narrow the response's current test claim: the test covers validation/core/
   render precedence, but not all independent proposal-schema fields.

## Nonblocking Finding

### V4-03 — MAJOR — `None`, `Show all`, and `Open complete interactive tree` need distinct UI semantics

Audit Charter layer: **Estimator and implementation correctness**, then
**Rendering fidelity**.

Affected specification sections: **Other filter modes**, **Final Rendering
Budget and Overflow**, and **User Interface**.

The policy text repeatedly uses `None/Show All` as one filter mode, while the
control list separately exposes:

- Filter = None;
- Open complete interactive tree; and
- Show all.

The likely design is sensible: None is the persistent complete-core filter
mode, Show all is a shortcut that activates None, and Open complete interactive
tree is a nonmutating viewer action. The text does not state this, however.
An implementation could instead treat Show all as a temporary override that
does not update the filter proposal, or make both actions open the same
viewer.

Recommended correction:

- define None as the persistent filter-state value;
- define whether Show all is a shortcut that sets Filter to None or remove the
  redundant control;
- define Open complete interactive tree as nonmutating with respect to filter
  mode, manual settings, selected IDs, and the retained proposal; and
- add UI-state tests for each action in renderable and overflow states.

## Prior Finding Dispositions

| Follow-up finding | Revision-4 disposition |
|---|---|
| FR-01 complete manual-mode contract | Resolved. Mode-aware inputs, raw Minimum Mass units, all-mass zero groups, ordinary outcomes, controls, and reference examples are explicit. |
| FR-02 orthogonal proposal state | Substantially resolved for validation/core/render axes. V4-02 concerns the still-undefined relationship between an invalid current attempt and a retained visible proposal. |
| FR-03 nominal Top-N boundaries | Resolved. N is a nominal boundary, complete ties may expand it, and requested/expanded counts are required. |
| FR-04 accessor naming | Resolved. `get.basin.merge.tree.layout()` is an ordinary public dot-delimited function name and does not imply nonexistent `layout()` dispatch. |

## Independent Verification

### Revision and diff

The assessed gflowui revision is:

```text
4b7610476a91dfd371a5f494f3c431d421685689
```

The revision diff passed:

```sh
git diff --check \
  71d6c35e26955023bb48e35a5510c948a2bdce71..\
  4b7610476a91dfd371a5f494f3c431d421685689
```

### Revision-4 reference test

From `/Users/pgajer/current_projects/gflowui`:

```sh
Rscript -e \
  'testthat::test_file("tests/testthat/test-basin-merge-tree-adaptive-filtering-fixture.R")'
```

Result:

```text
PASS 141
FAIL 0
WARN 0
SKIP 0
```

The 108 new assertions correctly exercise the requested manual-mode examples,
mode activation and retention of values, and validation/core/render
precedence. They do not exercise the future application implementation,
proposal serialization, `proposal.availability`, invalid non-mass ranking
measures, or viewport behavior.

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
maximum prominence difference:
                              8.673617379884035e-19
```

The empirical fixture contains finite values for all four Subject 15 ranking
measures. That confirms this fixture but does not define failure behavior for
future invalid inputs.

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

This checks current public merge-tree behavior. The filtered-layout accessor
and stronger prominence validation do not exist yet.

A full `make check-fast` or CRAN-style `make check` was not run because this is
a specification/reference-test revision and no package implementation or
generated documentation changed. Targeted tests are sufficient for the
claims currently made, but not for future implementation acceptance.

### Falsification checks

The revision-4 manual examples were independently recomputed:

```text
mass = 0.6, 0.4, 0, 0; Top K = 3:
  a,b,c,d; outcome top_k; warning tie_overflow

mass = 0.6, 0.4, 0, 0; Minimum Mass = 0:
  a,b,c,d; outcome minimum_mass

mass = 0.4, 0.3; raw Minimum Mass = 0.5:
  empty; outcome threshold_empty
```

The non-mass ranking counterexample produced:

```text
support = 10, NA, 5:
  finite = FALSE; a typical Top-N helper silently selected a,c

support = 10, Inf, 5:
  finite = FALSE; the same helper selected a,b

support = 10, -1, 5:
  finite = TRUE but invalid as a support count; the helper selected a,c
```

The disagreement is not a row-order or floating-point-tolerance issue. It
arises because the validation/failure policy is absent.

## Audit-Charter Assessment

- **Data and provenance:** pinned source hashes and the 352-branch mapping,
  parents, mass, support, and prominence were independently reproduced.
- **Measurement:** the empirical fixture is complete, but the specification
  validates only peak/source and mass, not every mandatory ranking measure.
- **Selection:** Auto and all manual core modes are deterministic for valid
  masses after revision 4. Mandatory sentinel/label selection remains
  underdetermined for invalid support or prominence.
- **Inference:** no inferential or uncertainty claim is made; the policy
  remains an initial display proposal.
- **Artifacts:** the response accurately describes the revision, but the
  reference state helper omits the proposal-availability field it says the
  schema now records.
- **Implementation:** the public gflow accessor, proposal helper,
  serialization, Shiny state, and renderers remain future work.
- **Rendering:** no new UI or viewport artifact exists to audit.

No induced-subgraph construction is required or permitted. Every eligible
source still must provide one finite value for every graph vertex.

## Acceptance Conditions

Specification acceptance requires:

1. validating every mandatory ranking measure and defining its failure
   behavior as required by V4-01;
2. defining and testing the current-attempt versus retained-view relationship
   in V4-02; and
3. adding the corresponding cases to the required validation contract.

V4-03 is a UI-state clarification and does not block specification acceptance.
Implementation acceptance remains a separate worker-auditor cycle after the
required gflow and gflowui code exists.
