# Adaptive Initial Filtering First Re-audit Response: Follow-up Re-audit

Date: 2026-08-01

Auditor role: independent specification re-auditor

Audited response:
`/Users/pgajer/current_projects/gflowui/dev/audits/basin_merge_tree_adaptive_initial_filtering_spec_audit_response_reaudit_response_2026-07-31.md`

Revised specification:
`/Users/pgajer/current_projects/gflowui/dev/basin_merge_tree_adaptive_initial_filtering_spec_2026-07-31.md`

First re-audit:
`/Users/pgajer/current_projects/gflowui/dev/audits/basin_merge_tree_adaptive_initial_filtering_spec_audit_response_reaudit_2026-07-31.md`

Related second re-audit:
`/Users/pgajer/current_projects/gflowui/dev/audits/basin_merge_tree_adaptive_initial_filtering_spec_second_reaudit_2026-07-31.md`

Pinned revisions:

```text
gflowui: 71d6c35e26955023bb48e35a5510c948a2bdce71
gflow:   24a671c4927df6ab6e5ac10361aecfd87cfaa0cb
upstream scientific repository:
         4615555547f3f406e79436c308d28fd78985b64e
```

The pre-existing local modification to
`/Users/pgajer/current_projects/gflow/AGENTS.md` and unrelated untracked audit
material were not part of the assessed revisions.

## Verdict

**Phase verdict: Revise before acceptance.**

**Response accuracy: accepted as a factual response to R-01 through R-05.**

The response accurately describes the changes made for all five first
re-audit findings and appropriately states that it does not claim re-audit
acceptance or implementation completion. Generic core overflow, component
fallback, filtered-coordinate semantics, parameter-domain validation, and the
bounded Subject 15 reference test are all present as claimed.

Fresh audit of the complete revised contract found two remaining blockers.
First, the Minimum Mass, Top K, and None/Show All modes do not yet have a
complete executable selection contract. Second, validation failures, core
selection outcomes, and proposal staleness are still conflated in the core
status vocabulary, leaving mass-disabled and stale final states
underdetermined.

These findings do not invalidate the Subject 15 rank-17 evidence or the
bounded Auto rule. They concern deterministic behavior of the other public
filter modes and the serialized proposal state machine.

## Blocking Findings

### FR-01 — BLOCKER — Manual filter modes lack complete inputs, units, tie semantics, and success statuses

Affected specification sections: **Source and Mass Validation**, **Parameter
Validation**, **Tie Groups**, **Configurable defaults**, **Other filter modes**,
**User Interface**, **Typed Status Summary**, and **Required Validation**.

The specification declares `top.k` and `minimum.mass` as parameters at lines
189-190, but:

- neither has a configurable default at lines 239-252;
- neither has a control in the UI control list at lines 508-520;
- the text does not say whether inactive mode-specific parameters are
  validated or ignored; and
- choosing Top K or Minimum Mass therefore does not identify a complete,
  valid proposal input.

This is more than a presentation omission. The strict-validation rule says
the UI does not recompute until all settings validate, but there is no valid
initial value for either missing setting. Conversely, ignoring both settings
until their modes become active requires a mode-aware validation rule that is
not currently specified.

Minimum Mass also has no declared unit. The specification says normalization
occurs after validation and defines a selected-component positive-mass
denominator, but “includes every complete tie group meeting the threshold” at
line 304 does not say whether `minimum.mass` is compared with:

- raw trajectory-flow `primary.support.mass`; or
- mass normalized by the selected component's positive total.

These interpretations can select different branches. With component masses
`a = 0.4` and `b = 0.3` and threshold `0.5`, raw comparison selects no
branches, while selected-component normalization selects `a`.

Zero-mass behavior is independently ambiguous. The formal tie-group section
at lines 221-235 groups positive masses only, while:

- zero is a valid stored mass;
- `top.k` may extend to the complete selected-component branch count;
- `minimum.mass = 0` is valid; and
- line 312 says no mode splits a mass tie.

For IDs `a`, `b`, `c`, and `d` with masses `0.6, 0.4, 0, 0`:

- Top K = 3 returns `{a, b, c, d}` if the complete zero group is
  indivisible, but can return `{a, b, c}` under positive-only grouping; and
- Minimum Mass = 0 returns all four IDs under literal threshold comparison,
  but can return only `{a, b}` if only the defined positive groups are
  eligible.

Finally, the typed core-status list has no ordinary successful value for:

- nonempty Minimum Mass;
- non-tied Top K; or
- None/Show All.

Every serialized proposal must store a core status, so implementations would
have to invent values or misuse automatic statuses such as `coverage`.

Risk:

- the same valid data and visible mode can select different canonical IDs;
- an activated manual mode may have no valid input state;
- exact zero ties may be split contrary to a stated invariant; and
- serialized status values can differ across implementations.

Required correction:

1. Define whether `top.k` and `minimum.mass` have explicit defaults or are
   required only when their corresponding mode is active.
2. Add conditional Top K and Minimum Mass controls to the UI contract and
   specify mode-aware validation, retention, and switching behavior.
3. Declare the Minimum Mass scale. The recommended default is raw validated
   trajectory-flow `primary.support.mass`, because that preserves the named
   measure's absolute meaning across components. If normalized component share
   is intended, rename and label the mode accordingly and disclose its
   denominator.
4. Define exact all-mass ranking groups, including one complete zero group,
   for Top K and Minimum Mass. Keep positive-only groups separately for
   logarithms, gaps, and normalized coverage.
5. State that Minimum Mass = 0 includes the complete zero group and therefore
   every branch in the selected component.
6. State that a Top K boundary entering the zero group includes the complete
   zero group and returns `tie_overflow`.
7. Add ordinary success statuses such as `minimum_mass`, `top_k`, and `all`
   or `complete`. These are serialized enum values, not R symbol names, so
   the existing snake-case status vocabulary is appropriate.
8. Add exact tests for mode activation, missing and retained conditional
   inputs, both Minimum Mass scale interpretations, all ordinary mode
   statuses, `c(0.6, 0.4, 0, 0)` with Top K = 3, and the same vector with
   Minimum Mass = 0.

### FR-02 — BLOCKER — Validation state, core outcome, and final availability are not orthogonal

Affected specification sections: **Direction and Component Scope**, **Source
and Mass Validation**, **Versioned Proposal Record**, and **Typed Status
Summary**.

The current “Core statuses” list mixes three different kinds of state:

- selection outcomes such as `strong_gap`, `coverage`, and `tie_overflow`;
- input or measurement failures such as `source_invalid`,
  `mapping_invalid`, `mass_invalid`, and `settings_invalid`; and
- identity invalidation through `stale`.

This becomes contradictory in the documented exception for None/Show All.
Lines 167-169 say `mass_invalid` and `mass_unavailable` disable the four
mass-based modes, but a valid canonical tree may still use None/Show All.
For that successful canonical-only proposal, `mass_invalid` cannot
simultaneously serve as its core-selection outcome, yet no ordinary Show All
core status exists.

The proposal schema compounds the problem. It records source and mapping
validation results, settings validation, and a core status, but it does not
explicitly require a separate mass-validation result. An implementation
cannot therefore preserve `mass_invalid` as measurement state while also
recording that None successfully selected the complete component.

Final status is also incomplete. Lines 615-617 define `unavailable` only when
source, mapping, or settings validation prevents a current branch set. They
omit:

- invalid or unavailable mass blocking the currently selected mass-based
  mode; and
- a stale proposal whose cached IDs must not be rendered.

Risk:

- None/Show All can be incorrectly disabled by bad mass despite its stated
  canonical-only fallback;
- a successful complete-component core can lose the mass-validation warning;
- stale cached IDs can receive no declared final state; and
- consumers cannot determine from the serialized record whether a status is a
  validation result, a selection result, or a rendering result.

Required correction:

1. Separate proposal identity/validation state, measure-validation state, core
   selection outcome, and final rendering outcome in both the typed-status
   section and serialized record.
2. Require an explicit mass-validation field independent of core status.
3. Define `unavailable` for any active mode whose blocking validation state
   prevents a current branch set, including mass-invalid or mass-unavailable
   mass-based modes and stale identities.
4. Permit None/Show All to record a successful complete-component core and
   render status when canonical identity remains valid, while separately
   retaining and disclosing `mass_invalid` or `mass_unavailable`.
5. Add a mode-by-validation-state test matrix covering Auto, all three manual
   mass modes, and None under valid, mass-invalid, mass-unavailable,
   source-invalid, mapping-invalid, settings-invalid, and stale conditions.

## Nonblocking Findings

### FR-03 — MAJOR — “Up to N” contradicts mandatory complete tie expansion

Affected specification sections: **Parameter Validation**, **Mandatory
Sentinels**, and **Label Policy**.

Lines 197-200 define sentinel and label counts as “up to N.” Lines 325 and 402
require complete ties at Top-N boundaries. If rank N belongs to a tie extending
beyond N, the returned count necessarily exceeds N. For values `3, 2, 2` and
N = 2, complete tie inclusion returns three branches.

The complete-tie rule is the more consistent invariant. Reword N as a nominal
rank boundary, state that ties may expand the result beyond N, record the
expanded count, and test straddling ties for each sentinel and label measure.

### FR-04 — MINOR — The proposed accessor name resembles an S3 method for a nongeneric

Affected specification section: **Required Public `gflow` Layout Contract**.

The example `layout.basin.merge.tree()` matches the existing
`basin.merge.tree` class spelling, but its `generic.class` form denotes an S3
method. `graphics::layout()` is neither primitive nor an S3 generic, so normal
`layout(object)` dispatch will not call this function.

Because the specification permits an equivalent public accessor, this is not
a contract blocker. Unless `gflow` intentionally introduces an exported
`layout()` generic, prefer an ordinary dot-delimited accessor such as
`get.basin.merge.tree.layout()`. This also exposes the existing private layout
capability without implying unavailable S3 dispatch.

## Response Finding Dispositions

| Response claim | Follow-up disposition |
|---|---|
| R-01 generic core overflow resolved | Accurate. `core_overflow`, cause precedence, all-mode handling, and direct complete presentation are specified. |
| R-02 component fallback resolved | Accurate. Whole-direction validation and stable invalid/all-zero fallbacks are specified. FR-02 concerns the later proposal status model, not fallback selection. |
| R-03 coordinate contract resolved | Accurate. Canonical vertical values and filtered horizontal layout are separated as claimed. |
| R-04 parameter domains resolved | Accurate for the listed scalar domains and rejection rules. FR-01 concerns missing mode inputs, mode-aware validation, and Minimum Mass units. |
| R-05 bounded fixture regression resolved | Accurate. The new reference test exercises the revision-3 Auto rule and keeps `which.max(gap)` only as raw fixture evidence. |
| No acceptance or implementation claim | Accurate and appropriately calibrated. |

## Independent Verification

### Revision and artifact checks

The assessed gflowui revision is
`71d6c35e26955023bb48e35a5510c948a2bdce71`. The response and specification
are unchanged tracked files at that revision.

The revision passed:

```sh
git diff --check \
  51998e3776c2ee0e74a747f9b14089d0e36a9da9..\
  71d6c35e26955023bb48e35a5510c948a2bdce71
```

### Portable Subject 15 reference test

From `/Users/pgajer/current_projects/gflowui`:

```sh
Rscript -e \
  'testthat::test_file("tests/testthat/test-basin-merge-tree-adaptive-filtering-fixture.R")'
```

Result:

```text
PASS 33
FAIL 0
WARN 0
SKIP 0
```

This confirms the response's R-05 claim for the portable Auto fixture. It does
not exercise Minimum Mass, Top K, None/Show All, multiple components, invalid
mass, or the future UI.

### Reconstruction from pinned source assets

The source ZIP and topology RDS were independently digest-checked and read
without invoking the fixture writer. Reconstruction produced:

```text
raw maximum branches:       352
canonical IDs exact:        TRUE
canonical parents exact:    TRUE
maximum mass difference:    3.469446951953614e-18
maximum prominence difference:
                            8.673617379884035e-19
```

This reproduces the fixture mapping and measurements from the pinned source
assets rather than trusting only the committed CSV.

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

This protects the existing public merge-tree behavior. The required filtered
layout accessor is not implemented and therefore is not covered by these
tests.

### Adversarial selection checks

The following specification counterexamples were evaluated:

```text
masses 0.6, 0.4, 0, 0; Top K = 3:
  all-mass complete tie -> a,b,c,d
  positive-only reading -> a,b,c

masses 0.6, 0.4, 0, 0; Minimum Mass = 0:
  literal threshold     -> a,b,c,d
  positive-only reading -> a,b

masses 0.4, 0.3; Minimum Mass = 0.5:
  raw comparison        -> empty
  component-normalized  -> a

values 3, 2, 2; Top N = 2 with full ties:
  returned count        -> 3
```

Each discrepancy follows from two plausible readings of the current text, not
from floating-point tolerance or row order.

The public API naming check returned:

```text
graphics::layout is primitive: FALSE
layout is an S3 standard generic: FALSE
```

## Audit-Charter Assessment

- **Data and provenance:** the pinned source digests, 352-branch mapping,
  parents, mass, and prominence were independently reproduced.
- **Measurement:** trajectory-flow `primary.support.mass` remains distinct
  from canonical tree mass. Their Subject 15 rank-17 agreement is not evidence
  that the vectors are interchangeable.
- **Selection:** the bounded Auto rule is reproducible, but manual-mode
  selection is not deterministic under the counterexamples in FR-01.
- **Inference:** the specification makes no uncertainty or inferential claim.
  The adaptive rule remains a display policy.
- **Artifacts:** the portable fixture and response claims are internally
  consistent at the pinned revision.
- **Implementation:** the adaptive helper, gflow filtered-layout API,
  renderers, and Shiny controls do not yet exist; implementation acceptance is
  not available.
- **Rendering:** existing figures demonstrate the readability problem only.
  They do not validate a universal filtering threshold.

No induced-subgraph construction is required or permitted by the default
workflow. Every eligible source must still provide one finite value for every
graph vertex.

## Acceptance Conditions

Specification acceptance requires:

1. completing the manual-mode input, unit, tie, and success-status contract in
   FR-01;
2. separating validation, measure, core-selection, and final-render state as
   required by FR-02;
3. reconciling the Top-N wording in FR-03; and
4. adding the corresponding required-validation cases.

FR-04 is a naming recommendation and does not block specification acceptance.
Implementation acceptance remains a separate worker-auditor cycle after the
required `gflow` API and `gflowui` behavior exist.
