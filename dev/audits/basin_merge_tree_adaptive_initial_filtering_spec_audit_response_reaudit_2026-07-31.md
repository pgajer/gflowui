# Adaptive Initial Filtering Specification Audit Response: Re-audit

Date: 2026-07-31

Auditor role: independent specification re-auditor

Re-audited response:
`/Users/pgajer/current_projects/gflowui/dev/audits/basin_merge_tree_adaptive_initial_filtering_spec_audit_response_2026-07-31.md`

Revised specification:
`/Users/pgajer/current_projects/gflowui/dev/basin_merge_tree_adaptive_initial_filtering_spec_2026-07-31.md`

Original audit:
`/Users/pgajer/current_projects/gflowui/dev/audits/basin_merge_tree_adaptive_initial_filtering_spec_audit_2026-07-31.md`

Pinned revisions:

```text
gflowui: 51998e3776c2ee0e74a747f9b14089d0e36a9da9
gflow:   24a671c4927df6ab6e5ac10361aecfd87cfaa0cb
upstream scientific repository:
         4615555547f3f406e79436c308d28fd78985b64e
```

## Verdict

**Revise before acceptance.**

Revision 2 is a substantial and mostly successful response. It resolves the
measure-ownership ambiguity, bounds the automatic gap search, makes ties
indivisible, scopes proposals to one maximum/component, specifies invalid and
zero-mass behavior, assigns filtered layout ownership to `gflow`, defines a
versioned proposal record, and supplies a portable Subject 15 fixture. The
replacement handoff is also materially compliant with audit-independence
requirements.

The Subject 15 fixture is valid: independent reconstruction from the graph and
selected field produced the same 352 canonical branches, all 352 parents, the
same survivor, and prominence values agreeing to `1.39e-17`. The revised
bounded rule also defeats the original late-tail counterexample.

Four residual contract defects still require implementation-time choices that
the specification should settle first:

1. a non-tied or user-selected core can exceed the final rendering budget, but
   no final status covers that case;
2. automatic component selection is undefined when mass is invalid or all
   component masses are zero;
3. the filtered-layout validation appears to require x-coordinates to be
   identical to the complete layout, which conflicts with readable subset
   layout; and
4. configurable parameter domains and invalid-setting behavior are absent.

These are narrower than the original findings, but they affect deterministic
control flow, status disclosure, and the public `gflow` contract.

## Blocking Findings

### R-01 — BLOCKER — Generic mass-core overflow has no final render status

Affected specification sections: **Other filter modes**, **Final Rendering
Budget and Overflow**, **Typed Status Summary**, and **Required Validation**.

The final overflow rules at
`dev/basin_merge_tree_adaptive_initial_filtering_spec_2026-07-31.md:277-295`
cover:

- a complete core tie group exceeding the final budget (`tie_overflow`);
- the pre-closure sentinel union exceeding it (`sentinel_overflow`); and
- ancestor closure causing the excess (`closure_overflow`).

They do not cover a core that exceeds the final budget without a tie. This is
reachable in several conforming configurations:

- `core.branch.budget = 50`, `final.render.budget = 20`, with 50 distinct core
  masses;
- Minimum Mass selecting 200 distinct branches, because that mode is not
  explicitly constrained by the core budget; or
- None/Show All selecting the complete component.

In each case the final set already exceeds the rendering budget before any
sentinel or closure addition. Calling it `sentinel_overflow` would assign the
wrong cause, while `tie_overflow` is false.

Risk: two implementations can either compress an unreadable core, silently
drop branches, misreport the cause, or invent different fallback behavior.

Required correction:

1. Add a final `core_overflow` status whenever the complete core exceeds
   `final.render.budget` and no more specific tie status is intended.
2. Define precedence independently for core status and final render status.
   For example, a core may have core status `tie_overflow` and final status
   `core_overflow`, with the tie retained as a reason/warning.
3. Apply the final budget to cores produced by every filter mode.
4. Specify that None/Show All routes directly to the complete interactive
   presentation rather than attempting the initial static tree when over
   budget.
5. Add non-tied Auto, Minimum Mass, Top K, and Show All core-overflow tests.

### R-02 — BLOCKER — Initial component selection is undefined when mass cannot rank components

Affected specification sections: **Direction and Component Scope** and
**Source and Mass Validation**.

The initial component is defined at
`dev/basin_merge_tree_adaptive_initial_filtering_spec_2026-07-31.md:116-120`
as the component with greatest validated trajectory-flow positive mass.
However, mass validation is then described for “the selected component” at
lines 133-140. This creates a circular order and leaves no initial component
when:

- every component has zero total positive mass (`mass_unavailable`);
- a branch mass is missing, negative, or nonfinite (`mass_invalid`); or
- component totals cannot be validated before selection.

Risk: the UI can fail before it can show the specified typed status, or choose
a component by incidental row order.

Required correction:

- Validate the declared mass vector across all maximum branches before
  automatic component selection, or define a separate global validation pass.
- When valid positive component totals exist, select the greatest total with
  the stated stable tie break.
- When all totals are zero, deterministically select the smallest stable
  component ID (or require explicit selection) and expose
  `mass_unavailable`.
- When mass is invalid, disable mass-based automatic component selection,
  choose a declared deterministic fallback or require the user to choose, and
  expose `mass_invalid` without attempting a mass proposal.
- Record the component-selection rule, totals, and fallback reason in the
  proposal record.

### R-03 — BLOCKER — Complete-tree coordinate preservation conflicts with filtered layout

Affected specification sections: **Required Public `gflow` Layout Contract**
and **Required Validation**.

The public accessor contract correctly requires static and interactive
renderers to consume the same filtered layout. The validation requirement at
`dev/basin_merge_tree_adaptive_initial_filtering_spec_2026-07-31.md:552-556`,
however, says filtered layouts preserve complete-tree births, deaths, parents,
events, survivor, **and coordinates** for the selected IDs.

Canonical scalar coordinates and display-layout coordinates are different
things:

- birth, death, and merge-level y coordinates are canonical and must remain
  exact; but
- leaf/trunk x coordinates depend on the displayed leaf set.

A readable 17-branch layout normally compresses its selected order to 17
positions. It cannot also retain the same absolute x positions used in a
352-branch layout without large gaps and scale-dependent behavior. Requiring
full-layout x coordinates would undercut the purpose of filtering; recomputing
them would fail the current literal validation.

Required correction:

1. Require exact preservation of canonical IDs, parents, events, births,
   deaths, merge levels, persistence, and survivor identity.
2. Define the selected leaf order as the complete canonical order restricted
   to selected IDs, unless `gflow` documents another deterministic
   crossing-free invariant.
3. Permit filtered x positions to be deterministically reindexed/compressed
   for the selected layout.
4. Require static and interactive renderers of the *same filtered selection*
   to use identical filtered coordinates; do not require those x coordinates
   to equal the complete-tree layout.
5. Split tests into canonical-value preservation, restricted-order stability,
   row-permutation determinism, and same-selection renderer equivalence.

### R-04 — BLOCKER — Configurable parameter domains and invalid-setting behavior are unspecified

Affected specification sections: **Configurable defaults**, **Bounded rule**,
**Other filter modes**, and **User Interface**.

The defaults are explicit, but their admissible types, ranges, and relationships
are not. Examples include:

- `core.branch.budget = 0`, for which no complete endpoint may exist;
- noninteger or nonfinite branch/Top-N counts;
- negative or nonfinite gap thresholds;
- coverage outside its probability range;
- a negative Minimum Mass threshold; and
- `final.render.budget < core.branch.budget`, which exposes R-01.

Risk: invalid settings can create undefined indices, silent coercion, or
different results between the pure helper and Shiny controls.

Required correction:

- Define `coverage.target` as finite and in a declared interval, recommended
  `(0, 1]`.
- Define `strong.gap.decades` as finite and nonnegative.
- Define minimum core and both budgets as positive whole numbers.
- Define sentinel and important-label counts as nonnegative whole numbers.
- Define Top K as a positive whole number and Minimum Mass as finite and
  nonnegative.
- State whether the final budget must be at least the core budget. If it need
  not be, R-01's `core_overflow` behavior is mandatory.
- Reject invalid settings with a typed validation result and retain the last
  valid proposal; do not silently clamp or coerce values.
- Test every boundary and invalid-value path in both pure helpers and UI input
  handling.

## Nonblocking Finding

### R-05 — MAJOR — The portable regression still asserts the obsolete largest-gap statistic

Affected response section: **Verification Performed in This Revision**.

The response says the fixture regression reproduces the bounded Subject 15
proposal. The committed test instead computes every adjacent gap and asserts:

```r
which.max(gap) == 17L
```

at
`tests/testthat/test-basin-merge-tree-adaptive-filtering-fixture.R:45-59`.
That is the original unbounded largest-gap statistic. It does not compute tie
groups, `j.coverage`, `j.minimum`, the budget, or the earliest eligible strong
gap.

The fixture itself is sufficient: an independent in-memory implementation of
the revised rule returned `j.coverage = 17`, `j.minimum = 3`, and the earliest
eligible strong gap at rank 17. Therefore this is an evidence/test gap, not a
fixture-data failure.

Required correction before implementation acceptance:

- replace or supplement `which.max(gap)` with a clean-checkout reference test
  of the revision-2 rule;
- assert ordered exact tie groups, coverage denominator, eligible boundary
  set, first qualifying boundary, final canonical IDs, and status
  `strong_gap`; and
- retain the raw mass/gap assertions as fixture-integrity checks rather than
  calling them an algorithm regression.

Implementation blocker for the specification phase: **No**. The response
already admits that the feature-level algorithm tests remain future work, but
its current verification wording should be narrowed.

## Original Finding Dispositions

| Original finding | Re-audit disposition |
|---|---|
| A-01 measure ownership | Resolved. Measures, owners, mapping, labels, and proposal provenance are explicit. |
| A-02 late-tail gap and ties | Resolved for valid default parameters. The revised rule selected rank 3 for the late-tail counterexample, capped the smooth tail at 50, and retained the complete 100-way tie. R-04 addresses parameter validation. |
| A-03 public filtered layout | Normatively resolved, subject to the coordinate correction in R-03. The response correctly states the API is not implemented yet. |
| A-04 forests/components | Mostly resolved; R-02 remains for unavailable/invalid mass during initial component selection. |
| A-05 invalid/zero/singleton mass | Resolved at the mass-proposal level. R-02 concerns the preceding component-selection order. |
| A-06 sentinel/closure overload | Mostly resolved; R-01 remains for core-caused overflow. |
| A-07 direction/state/proposal identity | Resolved. Version 1 is maxima-only and the serialized identity/state contract is sufficient. |
| A-08 portable Subject 15 fixture | Resolved as an artifact/provenance requirement. R-05 narrows the test claim. |
| A-09 independence wording | Resolved. The peak-floor agreement is correctly described as same-field corroboration. |
| A-10 handoff independence | Resolved. The replacement handoff supplies facts and limitations without prescribing audit questions or a verdict. |

## Independent Evidence

### Fixture and canonical tree

The committed fixture test passed with 17 expectations and no failures,
warnings, or skips.

Independent reconstruction of the maximum superlevel merge tree from the
registered Subject 15 graph and selected field produced:

```text
tree status:                    ok
canonical branches:            352
fixture IDs matched:           352
exact parent matches:          352
parent mismatches:             0
maximum prominence difference: 1.387779e-17
maximum peak-value difference: 5.724587e-17
survivor identity:             exact
```

The source digests independently matched the provenance record:

```text
ZIP:
15d575fea00267de49b12192060aeecdd373df6edfdea52cd250d68d2202c275

topology RDS:
afb7863d761932e31f4f1816f95b496db16fc58028663f26cb036ec6aa1af000
```

In-memory regeneration from those sources matched all fixture identities,
parents, support sizes, and survivor flags. Numeric differences after CSV
round-trip were at most `3.47e-18` for mass, `2.47e-32` for peak value, and
`8.67e-19` for prominence.

Additional structural checks found:

```text
primary support-size sum: 6529
finite nonnegative masses: true
positive branches:         352
parent cycles:              none
maximum ancestry depth:     6
```

### Revised-algorithm falsification

An independent implementation of the written revision-2 algorithm returned:

| Distribution | Revised result | Assessment |
|---|---:|---|
| 99.1% in first 3 plus a `1e-100` terminal value | `strong_gap`, rank 3, gap 3.856528 | original rank-199 failure corrected |
| `m_i = 1/i`, 400 branches | `coverage_capped`, rank 50, coverage 0.6848179 | truthful bounded fallback |
| 100 equal masses | `tie_overflow`, all 100, coverage 1 | tie remains indivisible |
| Subject 15 fixture | `strong_gap`, rank 17 | revised example reproduced independently |

These checks support the revised mass-core algorithm for valid defaults. They
do not remove the control-domain and final-overflow gaps above.

### Public API baseline

The response accurately states that implementation has not begun:

```text
layout.basin.merge.tree exported:       false
plot argument basin.ids:                absent
plot argument close.ancestors:          absent
```

The existing focused `gflow` public merge-tree test file passed all 46
expectations. That is baseline evidence only, not filtered-layout acceptance.

## Validation Commands

```sh
Rscript -e 'testthat::test_file("tests/testthat/test-basin-merge-tree-adaptive-filtering-fixture.R")'

Rscript -e 'pkgload::load_all("/Users/pgajer/current_projects/gflow", quiet=TRUE); testthat::test_file("/Users/pgajer/current_projects/gflow/tests/testthat/test-basin-merge-tree-public.R")'

git diff --check 925ed84..51998e3
```

Additional read-only R probes independently reconstructed the Subject 15 tree,
regenerated fixture contents in memory, verified ancestry, and exercised the
revision-2 algorithm on adversarial distributions.

No full package check was necessary for this documentation/fixture-only
revision. The new focused test and unchanged `gflow` API baseline passed; the
future code-bearing implementation will require targeted suites followed by
the repositories' Makefile QA targets.

## Conditions for Acceptance

Revision 2 is ready for another re-audit after it:

1. defines `core_overflow` or an equivalent truthful final status for every
   filter mode;
2. defines initial-component fallback when mass is invalid or unavailable;
3. separates canonical scalar coordinates from filtered display coordinates;
4. validates every configurable parameter and invalid-setting transition; and
5. narrows or strengthens the current fixture-regression claim.

The required changes are confined to specification and fixture-test contracts;
the main architecture and the portable Subject 15 evidence do not need to be
redesigned.
