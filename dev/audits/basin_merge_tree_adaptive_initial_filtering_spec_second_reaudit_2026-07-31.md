# Adaptive Initial Filtering Specification: Second Re-audit

Date: 2026-07-31

Auditor role: independent specification re-auditor

Audited handoff:
`/Users/pgajer/current_projects/gflowui/dev/basin_merge_tree_adaptive_initial_filtering_auditor_handoff_2026-07-31.md`

Revised specification:
`/Users/pgajer/current_projects/gflowui/dev/basin_merge_tree_adaptive_initial_filtering_spec_2026-07-31.md`

First re-audit:
`/Users/pgajer/current_projects/gflowui/dev/audits/basin_merge_tree_adaptive_initial_filtering_spec_audit_response_reaudit_2026-07-31.md`

First re-audit response:
`/Users/pgajer/current_projects/gflowui/dev/audits/basin_merge_tree_adaptive_initial_filtering_spec_audit_response_reaudit_response_2026-07-31.md`

Pinned revisions:

```text
gflowui: 71d6c35e26955023bb48e35a5510c948a2bdce71
gflow:   24a671c4927df6ab6e5ac10361aecfd87cfaa0cb
upstream scientific repository:
         4615555547f3f406e79436c308d28fd78985b64e
```

The pre-existing local modification to
`/Users/pgajer/current_projects/gflow/AGENTS.md` and unrelated untracked
gflowui audit material were not included in the assessed revision.

## Verdict

**Revise before acceptance.**

Revision 3 resolves all four blockers and the test-evidence gap from the first
re-audit. In particular, it now defines generic `core_overflow`, deterministic
component fallbacks, canonical vertical values versus compressed filtered x
coordinates, strict parameter domains, and a bounded Subject 15 reference
test. The handoff remains appropriately factual and does not prescribe an
auditor's questions or verdict.

One narrower implementation-contract blocker remains. The specification
requires every proposal to carry a core status, but it does not declare the
ordinary successful status for Minimum Mass, non-tied Top K, or None/Show All.
The same sections define tie groups only for positive masses while allowing
Minimum Mass threshold zero and Top K ranks among all component branches.
Consequently, conforming implementations can disagree about whether the
zero-mass branches form one indivisible tie group and which of them enter the
core.

This is not evidence against the adaptive filtering concept or the Subject 15
result. It is a deterministic API and state-machine defect that should be
settled before implementation.

## Blocking Finding

### S2-01 — BLOCKER — Non-Auto success statuses and zero-mass tie semantics are incomplete

Affected specification sections: **Source and Mass Validation**, **Parameter
Validation**, **Tie Groups**, **Other filter modes**, **Versioned Proposal
Record**, and **Typed Status Summary**.

The proposal record always stores a `core status`, and the typed-status list is
presented as exhaustive. The ordinary success paths for three modes have no
declared value:

- Minimum Mass defines only the empty result `threshold_empty`;
- Top K defines `tie_overflow` only when rank K is straddled; and
- None/Show All has no ordinary success status.

Using an existing automatic status such as `coverage` would be misleading,
while inventing mode-specific values during implementation would make the
serialized contract implementation-dependent.

There is a related boundary ambiguity. The specification says:

- zero is a valid mass and zero-mass branches remain available to Show All,
  sentinels, and ancestry;
- `top.k` may be any positive whole number up to the selected-component branch
  count;
- `minimum.mass` may be zero;
- no mode splits a mass tie; but
- the formal tie-group section groups **positive masses** only.

Consider canonical branch IDs `a`, `b`, `c`, and `d` with masses
`0.6, 0.4, 0, 0`.

- For Top K = 3, treating exact zero as an all-mass tie returns
  `{a, b, c, d}` and `tie_overflow`; applying positive-only tie groups can
  return `{a, b, c}`.
- For Minimum Mass = 0, literal threshold comparison returns all four
  branches; applying only the defined positive tie groups returns `{a, b}`.

Both interpretations are plausible under the current text. The latter also
splits or discards an exact mass tie despite the global no-split statement.

Mass failure status has one final inconsistency. `mass_invalid` and
`mass_unavailable` explicitly disable all mass-based modes, while None/Show All
may still use a valid canonical tree. However, final status `unavailable` is
defined only for source, mapping, or settings failure. The required final
status is therefore not declared when a mass-based mode has no current branch
set because mass is invalid or unavailable. The corresponding behavior for a
stale proposal should also be stated explicitly.

Risk:

- serialized proposals can carry different or undeclared status values;
- Top K and Minimum Mass can produce different canonical ID sets from the
  same valid inputs;
- exact zero ties can be split contrary to a stated invariant; and
- UI state can disagree about whether a mass-disabled proposal is
  `unavailable`.

Required correction:

1. Add ordinary successful core statuses for Minimum Mass, Top K, and
   None/Show All, for example `minimum_mass`, `top_k`, and `all` or
   `complete`. These are serialized enum values, not R symbol names, so the
   existing snake-case status vocabulary is appropriate.
2. Distinguish positive-mass tie groups used by logarithms and normalized
   coverage from exact all-mass ranking groups used by Top K and Minimum Mass.
   The latter must include one complete zero-mass group when zeros are present.
3. State that Minimum Mass = 0 includes the complete zero group and therefore
   all branches in the selected component.
4. State that Top K whose boundary enters the zero group includes the complete
   zero group and returns `tie_overflow`.
5. Define final status for mass-invalid, mass-unavailable, and stale current
   proposals. A natural rule is `unavailable` for a disabled mass-based mode,
   while None/Show All may produce a current proposal if the canonical tree
   and identity checks remain valid.
6. Add exact tests for all ordinary mode statuses and for
   `c(0.6, 0.4, 0, 0)` under Top K = 3 and Minimum Mass = 0.

## Nonblocking Findings

### S2-02 — MAJOR — “Up to N” conflicts with mandatory complete tie expansion

Affected specification sections: **Parameter Validation**, **Mandatory
Sentinels**, and **Label Policy**.

The parameter section says sentinel and label counts mean “up to N.” The
sentinel and label policies later require the rank-N boundary to include
complete ties. If the Nth-ranked value belongs to a tie group extending beyond
N, the returned count necessarily exceeds N.

This does not make the intended behavior difficult to infer: preserving the
complete tie is the stronger and more consistent invariant. The parameter
description should nevertheless call N a nominal rank boundary rather than a
hard upper count.

Recommended correction:

- say that fewer than N eligible branches returns all available branches;
- say that a boundary tie may expand the result beyond N;
- record or expose the tie-expanded count; and
- test a tie straddling each sentinel and label boundary.

### S2-03 — MINOR — The proposed layout accessor name looks like an S3 method without a generic

Affected specification section: **Required Public `gflow` Layout Contract**.

The example `layout.basin.merge.tree()` follows the package's dot-delimited
function-name convention, but its `generic.class` shape conventionally denotes
an S3 method. Base `graphics::layout()` is not an S3 generic, so users cannot
obtain this result through normal `layout(object)` dispatch.

The specification already permits an “equivalent” public API, so this is not a
contract blocker. Unless `gflow` intentionally introduces and exports a
`layout()` generic, prefer an ordinary function name such as
`get.basin.merge.tree.layout()` or `compute.basin.merge.tree.layout()`. The
former is the shorter recommendation.

## First Re-audit Finding Dispositions

| First re-audit finding | Second re-audit disposition |
|---|---|
| R-01 generic core overflow | Resolved. Core and final statuses are independent; `core_overflow` has explicit precedence and applies to every mode. |
| R-02 invalid/all-zero component fallback | Resolved. Whole-direction validation precedes component choice, and both error paths have stable component-ID fallbacks and recorded reasons. |
| R-03 canonical versus filtered coordinates | Resolved. Canonical vertical values and identities remain exact; filtered x coordinates are deterministic and shared only between renderers of the same selection. |
| R-04 parameter domains | Resolved. Domains, relationships, noncoercion, typed failure, and last-valid retention are explicit. |
| R-05 obsolete largest-gap regression | Resolved. The test now separates raw fixture integrity from the bounded reference algorithm and asserts groups, boundaries, IDs, sentinels, closure, coverage, and statuses. |

## Independent Verification

### Revision and scope checks

The assessed gflowui revision is
`71d6c35e26955023bb48e35a5510c948a2bdce71`. The revision is
specification/test work only; neither the adaptive gflowui feature nor the
required public gflow layout accessor is implemented.

The revision diff passed:

```sh
git diff --check 51998e3776c2ee0e74a747f9b14089d0e36a9da9..71d6c35e26955023bb48e35a5510c948a2bdce71
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

The revised test now verifies the bounded revision-3 rule rather than treating
the raw global `which.max(gap)` statistic as the algorithm. It asserts the
positive-mass groups, denominator, coverage/minimum boundaries, eligible
boundary range 17 through 50, first strong gap at rank 17, the 17 expected
canonical core IDs, mandatory additions, ancestor closure, final IDs,
coverage, `strong_gap`, and `renderable`.

### Independent fixture reconstruction

The current Subject 15 canonical maximum tree was independently reconstructed
from the registered graph and density field using the pinned gflow code. It
contained 352 branches. All 352 fixture canonical IDs and all 352 parents
matched, and the maximum absolute prominence difference was
`1.387779e-17`.

This confirms that the portable fixture still represents the current canonical
tree. It does not by itself validate the future gflowui filtering
implementation.

### Focused gflow regression

From the current gflow checkout:

```sh
Rscript -e \
  'pkgload::load_all("/Users/pgajer/current_projects/gflow", quiet=TRUE);
   testthat::test_file("/Users/pgajer/current_projects/gflow/tests/testthat/test-basin-merge-tree-public.R")'
```

Result:

```text
PASS 46
FAIL 0
WARN 0
SKIP 0
```

This protects the current public basin-merge-tree behavior. It does not test
the not-yet-implemented filtered-layout accessor.

### Adversarial contract checks

The typed-status enumeration was compared with every ordinary filter-mode
success path. `strong_gap` covers ordinary Auto and `coverage` covers ordinary
Cumulative Mass. No declared status covers ordinary successful Minimum Mass,
non-tied Top K, or None/Show All.

The mass vector `c(0.6, 0.4, 0, 0)` was then evaluated under the two readings
permitted by the current tie text. Top K = 3 yielded either three or four IDs,
and Minimum Mass = 0 yielded either two or four IDs. This is the concrete
counterexample underlying S2-01.

## Scientific and Audit-Scope Assessment

- The version-1 declared display-ranking quantity remains trajectory-flow
  `primary.support.mass`; canonical merge-tree prominence remains a separate
  topology/layout quantity.
- The earlier observed difference between trajectory-flow support mass and
  canonical branch mass is not treated as an implementation error merely
  because the Subject 15 rank-17 boundary agrees.
- No induced-subgraph construction is part of the default workflow. Every
  eligible source must provide one finite value per graph vertex.
- The bounded filtering rule is a display proposal, not a new inferential
  estimator or scientific threshold.
- Subject 15 is one empirical fixture. Synthetic and UI-level adversarial
  tests remain required during implementation.
- Minima/sublevel behavior remains outside version 1.
- No scientific acceptance of adaptive filtering, EOD interpretation, or the
  future implementation is claimed by this audit.

## Acceptance Conditions

The specification is ready for implementation acceptance after:

1. resolving S2-01 with exhaustive successful mode statuses, exact zero-mass
   tie behavior, and mass-disabled final-state behavior;
2. reconciling the “up to N” wording with mandatory tie expansion in S2-02;
   and
3. adding the corresponding status and zero-boundary cases to Required
   Validation.

S2-03 is a naming recommendation and does not block specification acceptance.
Implementation acceptance remains a separate worker-auditor cycle after the
new gflow API, gflowui proposal helper, renderers, UI, and required tests
exist.
