# Canonical Basin Complex and Basin Inspector Modification Plan

Status: Revised after independent re-audit; ready for final re-audit
Primary UI repository: `/Users/pgajer/current_projects/gflowui`
Required dependency repository: `/Users/pgajer/current_projects/gflow`
Planning baseline: `gflowui` `main` at `a74da1f6eb38d74b23c374a677c42190dc86c91b`
Audited `gflow` baseline: `main` at
`92a61c086f2fa1fa77223edfb02b74a1be3f1a28`
Audited `gflow` Git-describe output: `v0.2.0-1-g92a61c08-dirty`; the dirty state is
limited to `AGENTS.md` and untracked planning/audit artifacts, while the
constructor-related source files inspected for this plan match the recorded
commit
Plan date: 2026-07-28

## 1. Goal

Replace the direction-specific, reduced Top-K basin workflow in `gflowui` with
a canonical, two-direction basin-complex workflow based on:

```r
gflow::create.basin.complex(
  ...,
  method = "trajectory_flow",
  direction = "both",
  method.params = list(
    modulation = "CLOSEST",
    plateau.policy = "connected_exact",
    edge.length.quantile.thld = 1,
    long.edge.fallback = "allow_and_flag",
    primary.assignment.policy = "backend_primary"
  )
)
```

The workflow must:

1. construct maximum and minimum basins together;
2. use a strengthened canonical `summary.basin_complex()` API;
3. rank occupation-density basins by scientifically meaningful probability
   mass when available;
4. rank generic conditional-expectation basins by support size unless an
   independent, scientifically justified vertex-mass vector is supplied;
5. present a selectable, documented basin table in a resizable inspector below
   the graph;
6. provide persistent per-basin colors and graph display controls; and
7. remove the current `Flow direction` selector.

## 2. Decisions

### D-01: Do not transform a conditional expectation into vertex mass

A generic conditional expectation estimate is the scalar field whose geometry
defines the trajectories. It is not automatically a probability distribution
or a vertex-support measure.

The implementation must not min-max transform a conditional expectation and
pass the result as `vertex.mass`. Reasons:

- conditional expectations can be signed;
- their units are outcome units, not probability mass;
- min-max weights depend on the observed field range;
- the transformation changes under harmless affine changes of the outcome;
- low conditional-expectation values would receive low mass even when ranking
  minimum basins, creating a directionally inappropriate weighting; and
- the resulting value would be mislabeled as basin mass.

If a later scientific specification requires a field-integral ranking, it must
be introduced as a separately named measure with an explicit baseline and
direction-aware definition. It must not be called `primary.support.mass`.

### D-02: Define `rank.by = "auto"` from usable measures, not mass presence

The canonical summary method must resolve `rank.by = "auto"` independently for
each direction after retention filtering and before Top-K filtering. A
candidate ranking measure is usable only when:

- the direction has at least one post-retention-filter row;
- every candidate value for those rows is finite and nonnegative; and
- at least one candidate value is strictly positive.

An all-`NA`, partially unavailable, or all-zero vector is not usable.

The ordered candidate hierarchy is:

1. `primary.support.mass`;
2. `raw.allocated.mass`, when its allocation is current for the retained
   basin rows;
3. `retained.support.mass`;
4. `raw.support.mass`;
5. `primary.support.size`;
6. `retained.support.size`; and
7. `raw.support.size`.

Mass candidates are considered only when a vertex-mass vector was supplied and
passed constructor numeric validation, but that fact alone does not make any
aggregate mass candidate usable or verify its scientific meaning; each
candidate must still pass the direction-specific value checks above. The
hierarchy therefore supports canonical methods without primary assignments,
including methods for which primary support is entirely unavailable.

`raw.allocated.mass` is intentionally ahead of overlapping support-mass
measures because it uses canonical membership weights and conserves supplied
mass within a direction when all raw memberships are represented. By contrast,
`retained.support.mass` and `raw.support.mass` are coverage measures: the same
vertex mass can contribute fully to multiple overlapping basins, so their
directional totals can exceed one.

The allocated measure is usable by `auto` only when canonical provenance says
that its membership allocation is current for the post-retention-filter rows.
Refinement that merges, expands, or otherwise changes basin supports without
recomputing allocated mass makes it unavailable to `auto`. It remains an
explicit ranking option, but an explicit request also errors when the
allocation is stale. Its pre-refinement meaning is stated in the column
definition. Tests must include overlapping memberships for which coverage and
allocated mass have different totals or basin ordering.

If no candidate is usable for a nonempty direction, the summary must return a
specific unranked status or error; it must not fabricate a rank from basin ID.
A direction with no post-retention-filter rows returns an empty table,
`rank.resolved = NA` for that direction, and an explicit `empty` status rather
than an error. Setting that direction's Top-K to zero does not change ranking
availability or construction status; it only produces an empty displayed
direction after ranking.

The resolved measure must be returned as a direction-keyed value, because
maximum and minimum availability may differ. The rule must not inspect UI
labels such as "occupation density" or "conditional expectation."

For current `gflowui` sources:

- occupation-density probability mass is a valid `vertex.mass`;
- a generic conditional expectation supplies no `vertex.mass`;
- a future manifest may supply an independent sampling, quadrature, or
  population mass vector, provided that its meaning and alignment are
  validated explicitly.

The initial gflowui workflow is fixed to trajectory-flow objects with
`backend_primary` assignments, so its normal resolved measures remain primary
support mass or primary support size. The broader hierarchy is required because
`summary.basin_complex()` is public for all canonical methods.

### D-03: Fix canonical CLOSEST construction settings

The user-facing workflow must keep these settings fixed:

- `method = "trajectory_flow"`
- `direction = "both"`
- `modulation = "CLOSEST"`
- `plateau.policy = "connected_exact"`
- `edge.length.quantile.thld = 1`
- `long.edge.fallback = "allow_and_flag"`
- `symmetric.seeding = FALSE`
- `tie.breaking = FALSE`
- `primary.assignment.policy = "backend_primary"`

`store.trajectories` and `max.trajectory.length` are operational settings, not
scientific display settings. The implementation must benchmark trajectory
storage on the reference graph, record the final choice, and keep that choice
fixed and provenance-bearing. The default planning preference is
`store.trajectories = FALSE` because this basin-table workflow does not display
paths, but the choice is not final until the canonical backend is verified to
honor it without changing assignments.

### D-04: Do not expose edge-length threshold or fallback controls

CLOSEST first chooses the shortest strictly improving edge at or below the
edge-length threshold. If no improving edge is below the threshold:

- `allow` and `allow_and_flag` use the shortest improving edge anyway;
- `allow_and_flag` also records attention telemetry; and
- `forbid` terminates the trajectory at the current vertex.

Therefore, with a permissive fallback, changing the quantile normally changes
telemetry but not the selected CLOSEST trajectory. With `forbid`, it changes
the basin definition and can introduce threshold-induced terminals even when a
strictly improving graph neighbor exists.

The intended basin definition is unrestricted CLOSEST flow to graph-local
extrema. Setting `edge.length.quantile.thld = 1` makes every graph edge
admissible and makes the fallback inactive. These parameters must appear only
in read-only construction provenance.

### D-05: Compute both directions once

`create.basin.complex(direction = "both")` returns one assignment row per
finite vertex for each requested direction. `gflowui` must not run separate
maximum and minimum reconstructions and must remove the `Flow direction`
selector.

### D-06: Treat Top-K as a summary/display filter

`Largest maximum basins` and `Largest minimum basins` must not change the
canonical basin complex. They control which ranked rows are initially included
in the inspector and eligible for bulk selection.

Changing either value must rerun only summary/filtering and rendering logic,
not `create.basin.complex()`.

### D-07: Use immutable gflow build identity

Semantic package version is display metadata and must not be the sole
construction provenance or cache invalidator.

The gflow implementation phase must provide a build identity containing:

- semantic package version;
- source revision when available;
- source-dirty state;
- a deterministic, build-generated manifest and digest of the complete package
  code input set, rather than a manually curated constructor-file list;
- a digest of the installed native artifact or package build artifact; and
- a combined immutable build ID.

The code-input manifest must cover, at minimum, all files under `R/` and
`src/`, `DESCRIPTION`, `NAMESPACE`, build configuration such as
`configure*`/`Makevars*`, and any other file loaded or compiled into package
behavior. The manifest format itself is versioned. Adding, removing, or
changing any listed code input must change the combined build ID.

For source-loaded development, the identity must distinguish changes to
package code even when `DESCRIPTION` remains unchanged. For an installed
release without Git metadata, the build must embed the complete source/build
manifest digest during packaging.

R version, platform/architecture, native ABI, and versions of imported or
linked behavior-relevant dependencies must be recorded as runtime provenance.
The initial cache design must conservatively include a runtime-compatibility
digest over those values; narrowing it later requires evidence that omitted
differences cannot change construction results.

### D-08: Store typed mass provenance with explicit trust boundaries

The constructor must accept and retain typed `vertex.mass` provenance rather
than asking the summary method to infer semantic meaning. It must not upgrade
an upstream attestation into a constructor-verified fact.

Append both new formals after the existing `verbose` argument so every legacy
unnamed positional call retains its meaning:

```r
vertex.id = seq_along(field)
vertex.mass.provenance = NULL
```

`vertex.id` accepts character or integer vectors, must be unique, nonmissing,
nonempty after character encoding, and length-aligned, and rejects factors
rather than depending on their level encoding. Integer IDs are converted with
locale-independent decimal formatting; character IDs are preserved as UTF-8
after rejecting invalid encoding. The default preserves backward compatibility
with internal integer identity. `gflowui` must pass the reviewed external IDs
in graph order.

Existing canonical assignment, membership, extrema, and support-list columns
retain internal integer indices. Additional external-ID columns/list columns
expose the corresponding `vertex.id` values without redefining existing
fields.

The stored provenance record has three explicitly labeled layers:

1. **Constructor-computed facts**
   - actual input-mass fingerprint;
   - input total and normalization facts;
   - ordered internal graph fingerprint computed from `vertex.id`,
     adjacency, and edge lengths; and
   - immutable gflow build and runtime-compatibility identities.
2. **Constructor-validated declarations**
   - controlled enum/schema values;
   - a declared mass fingerprint, checked against `vertex.mass`;
   - a declared ordered-vertex fingerprint, checked against `vertex.id`; and
   - a declared internal-graph fingerprint, checked against the constructor's
     graph inputs.
3. **Upstream attestations**
   - scientific mass kind, such as occupation probability, empirical weight,
     quadrature weight, population weight, or unspecified explicit mass;
   - source or asset identity and source-content fingerprint;
   - external graph identity; and
   - the claim that source values, mass, external vertex IDs, and graph were
     aligned before construction.

Every upstream attestation must store the claim, validation authority,
validator/contract name and version, algorithm, evidence fingerprint, and
status. `gflow` schema-validates this record and preserves it, but does not
claim to have verified the external asset, scientific interpretation, or
upstream alignment.

`gflow` must reject recomputable mass, supplied-vertex-ID, or internal-graph
digest mismatches. `gflowui` must reject external source, graph-ID, and
vertex-order mismatches before calling the constructor.

`summary.basin_complex()` must return the provenance layers without relabeling
or flattening their verification scope. A free-form summary argument must not
be allowed to assign semantic meaning retroactively. There is no global
`validation.status = "validated"` label.

### D-09: Require complete finite fields

Every default-workflow source must provide exactly one finite scalar value per
aligned graph vertex. `NA`, `NaN`, or infinite values are blocking source
errors.

The current automatic finite-induced-subgraph fallback must be removed. Partial
field support is deferred until a real use case has a separately reviewed
scientific specification for the changed domain and graph semantics.

## 3. Verified Current State

### 3.1 Current `gflowui` reconstruction

`R/basin_display_helpers.R` already calls:

```r
gflow::create.basin.complex(
  method = "trajectory_flow",
  direction = direction,
  method.params = list(
    modulation = "CLOSEST",
    plateau.policy = "connected_exact",
    edge.length.quantile.thld = 1,
    long.edge.fallback = "allow_and_flag",
    ...
  )
)
```

The helper currently:

- accepts only one direction;
- ranks and truncates basins inside `gflowui`;
- maps the selected direction to a categorical graph vector; and
- reduces the canonical basin table to rank, basin ID, mass, support,
  extremum vertex, and extremum value.

The reconstruction itself is not an internal `gflowui` trajectory engine.

### 3.2 Current precomputed exception

For a precomputed occupation-density path in the maximum direction,
`R/app_server.R` can use stored upstream basin assignments instead of invoking
the canonical constructor. Other cases call
`gflowui_estimate_basin_overlay()`.

The new workflow must not retain two silent basin semantics. Existing
precomputed assignments may be used only as:

- migration evidence;
- a comparison fixture; or
- a cache after they are regenerated as a canonical `basin_complex` with a
  matching graph fingerprint, field fingerprint, parameter record, and gflow
  immutable build ID.

### 3.3 Current canonical summary API

`summary.basin_complex()` currently returns aggregate fields:

- method;
- direction;
- status;
- numbers of vertices, components, basins, memberships, assignments, and
  diagnostics; and
- whether vertex mass is present.

The detailed canonical table is available from:

```r
gflow::get.basin.table(object)
```

The comprehensive `summary.basin_cx()` remembered from the older API belongs to
the retired `create.basin.cx()` object class. It must not be revived as a
parallel UI dependency.

### 3.4 Audited gflow source and worktree provenance

The source decisions in this plan were inspected against:

```text
Repository: /Users/pgajer/current_projects/gflow
Commit: 92a61c086f2fa1fa77223edfb02b74a1be3f1a28
Describe: v0.2.0-1-g92a61c08-dirty
Semantic package version: 0.2.0
```

The worktree was dirty because of a modified `AGENTS.md` and untracked
synthetic-data planning/audit artifacts. The inspected constructor-related R
and native files were not modified relative to the recorded commit.

The equality of semantic version `0.2.0` across distinct source revisions is
the motivating cache-collision example for D-07.

## 4. Requirement Traceability

| ID | Requirement | Planned ownership | Acceptance evidence |
|---|---|---|---|
| R-01 | Use canonical `create.basin.complex()` | `gflowui` adapter | Server test and stored provenance |
| R-02 | Compute maximum and minimum basins together | `gflowui` adapter | `direction == "both"` and `n` assignments per direction |
| R-03 | Strengthen canonical summary | `gflow` | Unit tests for returned table and compatibility |
| R-04 | Do not derive mass from a generic conditional expectation | `gflowui` source contract | Conditional-expectation test with `vertex.mass = NULL` |
| R-05 | Make `auto` deterministic and provenance-bearing | `gflow` | Direction-specific usability tests across mass, size, missing, partial, all-zero, and empty cases |
| R-06 | Remove `Flow direction` | `gflowui` UI | UI construction test and live inspection |
| R-07 | Independent maximum/minimum Top-K filters | `gflow` summary and `gflowui` UI | Ranking and no-reconstruction invalidation tests |
| R-08 | Select basins by row | `gflowui` inspector | Checkbox state and graph overlay tests |
| R-09 | Persistent per-basin colors | `gflowui` inspector/render state | Reorder/filter/rebuild persistence tests |
| R-10 | Accommodate wide summary table | `gflowui` viewer layout | Resizable bottom inspector and viewport QA |
| R-11 | Explain non-obvious columns | `gflow` summary metadata and `gflowui` tooltips | Column-definition coverage test |
| R-12 | Preserve raw source values | Both repositories | Equality tests before and after display operations |
| R-13 | Avoid silent precomputed/canonical semantic divergence | `gflowui` | Source-path tests and provenance status |
| R-14 | Support Plotly and RGL behavior without false validation claims | `gflowui` | Renderer-specific tests and recorded manual QA |
| R-15 | Make public auto ranking valid across canonical methods and availability patterns | `gflow` | RTCB, overlap-cell, partial, empty, and direction-specific tests |
| R-16 | Preserve typed mass provenance without overstating verification authority | `gflow` and `gflowui` | Layered source-to-constructor-to-summary attestation tests |
| R-17 | Reject incomplete/non-finite default-workflow fields | `gflowui` | Pre-construction blocking and cache-exclusion tests |
| R-18 | Prevent same-version build cache collisions | `gflow` and `gflowui` | Distinct-build identity/cache-key test |
| R-19 | Support every zero-Top-K UI state | `gflowui` | Empty-table, selection, neutral-render, and restoration tests |
| R-20 | Record reference compute and memory evidence | Both repositories | Benchmark record and human-facing usability disposition |
| R-21 | Distinguish overlapping coverage mass from conserved membership allocation | `gflow` | Overlap fixture with divergent measures, definitions, and resolved-rank tests |
| R-22 | Make external vertex identity an explicit constructor input | `gflow` and `gflowui` | Supplied-ID digest and upstream-alignment boundary tests |
| R-23 | Make build-digest coverage complete by construction | `gflow` | Versioned code-input-manifest mutation tests |

## 5. Required `gflow` API Modification

### 5.1 Compatibility requirement

Existing fields returned by `summary.basin_complex()` must remain available
with their current names and meanings.

New fields may be added without removing or silently redefining existing
fields.

### 5.2 Proposed method signature

The exact argument spelling should be settled in `gflow`, but the intended
contract is:

```r
summary.basin_complex <- function(
  object,
  rank.by = c(
    "auto",
    "primary.support.mass",
    "raw.allocated.mass",
    "primary.support.size",
    "retained.support.mass",
    "retained.support.size",
    "raw.support.mass",
    "raw.support.size"
  ),
  top.k.max = Inf,
  top.k.min = Inf,
  include.unretained = FALSE,
  include.vertex.lists = FALSE,
  ...
)
```

`top.k.max` and `top.k.min` must accept:

- a nonnegative whole number;
- `Inf` for all basins; and
- zero for no rows of that type.

### 5.3 Ranking behavior

The summary method must:

1. obtain the canonical table through the canonical object schema;
2. filter unretained basins unless requested;
3. evaluate candidate availability separately for maximum and minimum rows;
4. resolve `rank.by` independently by direction when `auto` is requested;
5. reject an explicitly requested measure that is unusable for any requested
   nonempty direction;
6. return an explicit empty status for a direction with no
   post-retention-filter rows;
7. return an explicit unranked status or error when a nonempty direction has no
   usable candidate;
8. rank maxima and minima independently;
9. break ties deterministically by `basin.id`;
10. add a direction-local integer `rank`;
11. apply `top.k.max` and `top.k.min` after ranking; and
12. return the requested and direction-specific resolved ranking measures.

A candidate measure is usable only when all post-retention-filter values for
the direction are finite and nonnegative and at least one is positive. This
rule excludes:

- all-`NA` primary mass;
- partially unavailable measures;
- all-zero primary support created by `primary.assignment.policy = "none"`;
- measures unavailable for only one direction; and
- empty tables, which have their own explicit status.

`raw.allocated.mass` has one additional availability condition: the object
must record that membership allocation is current for every ranked retained
basin. A refinement that changes basin membership or support without
recomputing the allocation marks the measure stale/unavailable. Its
directional definition must state that it is membership-weighted mass, while
support-mass definitions must state that they are overlapping coverage mass.

For `rank.by = "auto"`, candidates must be evaluated in the D-02 hierarchy.
The implementation must not infer usability from `vertex.mass` presence,
method name, or assignment policy alone.

For any explicit `rank.by` value:

- the selected measure must pass the same direction-specific usability test;
- an unavailable measure must produce a specific error naming the affected
  direction and availability reason; and
- the method must not silently fall back.

Only `rank.by = "auto"` may move to the next usable candidate.

### 5.4 Proposed additional summary fields

```text
rank.requested
rank.resolved
rank.status
rank.measure.definition
measure.availability
raw.allocation.current
mass.available
mass.provenance
top.k.max
top.k.min
basin.table
maxima
minima
column.definitions
```

`rank.resolved` and `rank.status` are direction-keyed values. `basin.table` is
the combined filtered table. `maxima` and `minima` are direction-specific views
with identical column schemas.

`rank.measure.definition` must distinguish unique-assignment mass,
membership-allocated mass, overlapping coverage mass, and vertex count.
`raw.allocation.current` records whether raw allocation still corresponds to
the retained rows after refinement.

`column.definitions` must provide a stable machine-readable mapping from column
name to:

- short display label;
- plain-language definition;
- unit or scale;
- availability condition; and
- whether the column is recommended in compact view.

This prevents `gflowui` from independently inventing scientific definitions.

### 5.5 Canonical vertex identity and mass provenance

Append the new formals after all existing formal positions:

```r
create.basin.complex(
  adj.list,
  edge.length.list,
  field,
  method,
  direction,
  vertex.mass,
  vertex.density,
  graph.params,
  method.params,
  simplify.params,
  verbose,
  vertex.id = seq_along(field),
  vertex.mass.provenance = NULL
)
```

The abbreviated new portion is:

```r
vertex.id = seq_along(field)
vertex.mass.provenance = NULL
```

The constructor applies the canonical type/encoding rules in D-08, validates
that `vertex.id` is unique, nonmissing, and aligned with all graph and field
inputs, and computes ordered-vertex and internal-graph fingerprints from
inputs it actually receives.

When `vertex.mass` is absent, mass provenance must be `NULL`. When mass is
present:

- omitted provenance is recorded with declaration kind
  `unspecified_explicit`, preserving backward compatibility without inventing
  semantics or an external validation claim;
- gflowui's reviewed sources supply typed upstream attestations;
- the constructor recomputes the mass fingerprint, total, and normalization
  facts;
- declared mass, ordered-vertex, and internal-graph fingerprints must match
  the underlying constructor inputs;
- controlled enums and attestation schemas must be valid; and
- the canonical object must retain computed facts, validated declarations, and
  upstream attestations through refinement, serialization, conversion where
  supported, caching, and summary.

The summary returns `mass.provenance` with its trust layers intact. It must not
accept a label or semantic override through `...`, describe upstream
attestations as constructor verification, or collapse the layers to one
ambiguous validation flag.

The source-to-object contract requires round-trip and rejection tests for:

- occupation probability attested by a versioned gflowui/manifest contract;
- manifest-provided scientific weights;
- unspecified explicit mass;
- no mass;
- mismatched mass fingerprint rejected by gflow;
- mismatched supplied `vertex.id` fingerprint rejected by gflow;
- mismatched internal graph fingerprint rejected by gflow;
- external source, graph-ID, or vertex-order mismatch rejected by gflowui
  before construction;
- schema-invalid attestation rejected by gflow;
- valid upstream attestation preserved without being upgraded to
  constructor-verified; and
- complete legacy unnamed positional constructor call preserving the existing
  eleven-argument behavior;
- factor, missing, duplicate, empty, invalid-encoding, character, and integer
  `vertex.id` behavior;
- unchanged internal integer columns plus correct external-ID companion
  columns; and
- serialized/cached object restoration.

### 5.6 Default compact summary columns

The summary should make these scalar columns easy to consume:

- `basin.id`
- `extremum.id`
- `type`
- `rank`
- `extremum.vertex`
- `extremum.value`
- `primary.support.size`
- `primary.support.mass`
- `raw.support.size`
- `raw.support.mass`
- `retained.support.size`
- `retained.support.mass`
- `raw.allocated.mass`
- `assignment.status`
- `retention.status`

List columns containing vertex IDs remain canonical but should be omitted from
the default UI payload when `include.vertex.lists = FALSE`.

### 5.7 `gflow` tests

Add tests for:

- both-direction CLOSEST construction;
- exactly one assignment per vertex per direction;
- independent maximum and minimum ranking;
- deterministic ties;
- trajectory-flow mass-present `auto`;
- trajectory-flow mass-absent `auto`;
- mass-bearing RTCB and overlap-cell objects without primary assignments;
- fallback to usable retained or raw measures;
- overlap memberships where `raw.allocated.mass` and support mass have
  different totals or basin ordering;
- `raw.allocated.mass` selected by `auto` only when allocation-current
  provenance is true;
- explicit `raw.allocated.mass` behavior and stale-allocation rejection;
- all-zero candidate measures;
- partially unavailable candidate measures;
- empty retained tables;
- direction-specific availability and direction-specific resolution;
- explicit mass ranking without mass errors;
- support-size ranking with and without mass;
- Top-K values `0`, finite positive values, and `Inf`;
- retained/unretained filtering;
- plateau basins;
- layered mass-provenance round trips, trust-scope preservation, and
  layer-specific mismatch rejection;
- two constructor builds with the same semantic version producing distinct
  immutable build IDs and cache identities;
- backward compatibility of existing summary fields; and
- completeness of column definitions for every returned scalar column.

Documentation and `NAMESPACE` changes must be generated from roxygen source
rather than editing generated files by hand.

## 6. `gflowui` Source and Mass Contract

### 6.1 Estimate source

An eligible source must provide:

- exactly one finite scalar field value per graph vertex;
- graph adjacency and aligned edge lengths;
- graph set ID and actual `k`;
- project ID;
- explicit, unique external vertex IDs in graph order;
- source key and human-readable label; and
- source type and provenance.

The source is invalid if alignment is inferred only from equal vector lengths
or if any field value is `NA`, `NaN`, or infinite.

`gflowui` owns the external alignment boundary. It validates the source IDs,
graph IDs, ordered external vertex IDs, and asset content before construction,
then passes the ordered IDs as `vertex.id`. `gflow` can verify the digest of
the supplied IDs and its internal graph inputs; it cannot independently verify
that those IDs came from the claimed external asset.

### 6.2 Occupation-density source

For a normalized occupation-density field:

```r
field <- raw_probability_mass
vertex.mass <- raw_probability_mass
```

The source adapter must also construct typed mass provenance with:

- kind `occupation_probability`;
- source/asset ID;
- raw asset fingerprint;
- external graph ID/fingerprint;
- ordered external vertex-ID fingerprint;
- mass-vector fingerprint; and
- an upstream attestation for scientific meaning and alignment.

The attestation records:

- authority, for example a named manifest or `gflowui` source contract;
- validator/contract name and version;
- validation algorithm;
- evidence fingerprint; and
- status.

The mass-vector fingerprint is constructor-recomputed. Normalization facts and
the internal graph fingerprint are constructor-computed. The scientific kind,
asset identity, and external alignment remain explicitly upstream-attested.

The same vector has two explicitly recorded roles:

- field values determine ascending and descending CLOSEST trajectories;
- probability mass determines basin support mass.

Display log transforms and palette transformations must not be used in either
role.

### 6.3 Conditional-expectation source

For a generic conditional expectation:

```r
field <- conditional_expectation
vertex.mass <- NULL
```

With the fixed `backend_primary` trajectory-flow construction, the normal
default and auto ranking is therefore primary support size. The canonical
summary still applies the complete direction-specific hierarchy in D-02 if
primary support is unavailable.

A separate vertex-mass asset may be accepted only through an explicit manifest
contract that states its meaning and proves graph/vertex alignment. The
conditional-expectation values themselves must not be min-max transformed into
mass. Any accepted mass asset must produce the typed provenance required by
D-08, including the authority and evidence for its upstream claims.

### 6.4 Non-finite field values

The canonical constructor requires finite values, and the default gflowui
workflow requires full-domain fields.

Before cache lookup or construction, the source adapter must reject:

- length mismatch;
- missing vertex IDs;
- duplicate vertex IDs;
- vertex-order or graph-fingerprint mismatch; and
- any `NA`, `NaN`, or infinite field value.

These are gflowui source-contract rejections. After construction begins,
gflow separately rejects mismatches it can recompute from supplied mass,
`vertex.id`, adjacency, and edge-length inputs.

A failed source must not populate, retrieve, or reuse a construction cache and
must not leave an older basin result displayed as current.

The current finite-induced-subgraph adapter must be removed from the default
workflow. Partial-domain construction is deferred. Any future partial-field
mode requires a separate scientific specification covering changed graph
semantics, complete remapping of all vertex-bearing fields and encoded IDs,
visible domain warnings, and separate domain/cache fingerprints.

## 7. Revised Basins Sidebar

### 7.1 Controls

| Control | Widget and default | Function | Must not do |
|---|---|---|---|
| Estimate source | Select input; currently applied density preferred, otherwise an eligible conditional expectation | Selects the scalar field and aligned graph used for construction | Must not compute or display a basin by itself |
| Largest maximum basins | Whole-number input; default 6; minimum 0 | Sets the maximum-direction Top-K summary/display filter | Must not rerun `create.basin.complex()` |
| Largest minimum basins | Whole-number input; default 6; minimum 0 | Sets the minimum-direction Top-K summary/display filter | Must not rerun `create.basin.complex()` |
| Ranking measure | Select input; default Auto | Chooses Auto, Primary support mass, or Primary support size for the trajectory-flow UI; the canonical API also supports allocated and coverage measures for other methods | Must not transform a conditional expectation or silently use an unavailable measure |
| Compute Basin Complex | Primary action button | Runs or retrieves the canonical both-direction construction, then computes its summary | Must not silently use legacy precomputed assignments |
| Open Basin Inspector | Secondary action button; disabled until a successful result exists | Opens/focuses the bottom inspector | Must not recompute |
| Construction details | Read-only disclosure | Shows method, direction, resolved fixed parameters, layered mass provenance, graph/field fingerprints, gflow build/runtime identity, warnings, and cache status | Must not expose scientific construction choices casually or imply that upstream attestations were constructor-verified |

### 7.2 Ranking-measure behavior

The choices are:

1. **Auto**
   - uses the canonical direction-specific availability hierarchy;
   - occupation-density trajectory flow normally resolves to primary support
     mass;
   - generic conditional-expectation trajectory flow normally resolves to
     primary support size;
   - the status area shows the resolved measure for maxima and minima
     independently.
2. **Primary support mass**
   - enabled only when the primary mass measure is usable for every requested
     nonempty direction;
   - otherwise disabled with explanatory text.
3. **Primary support size**
   - enabled only when the primary size measure is usable for every requested
     nonempty direction;
   - trajectory flow with `backend_primary` normally satisfies this condition.

The UI must show the resolved measure in status text, for example:

```text
Ranked by primary support size because this conditional-expectation source
does not provide vertex mass.
```

### 7.3 Construction details instead of advanced settings

The previously proposed `Advanced reconstruction settings` control is removed
from the first implementation.

Changing the edge-length threshold or fallback changes either telemetry or the
scientific basin definition. Other simplify/refinement parameters also change
the scientific object. Such settings require a separate reviewed
specification before becoming interactive.

The read-only details disclosure should show:

```text
Method: trajectory_flow
Direction: both
Modulation: CLOSEST
Plateau handling: connected_exact
Edge-length quantile: 1.0 (all graph edges admissible)
Long-edge fallback: allow_and_flag (inactive when all edges are admissible)
Primary assignment: backend_primary
Vertex mass: occupation probability / absent / manifest asset <id>
Mass vector: constructor verified / absent
Mass semantics: occupation_probability
  (attested by manifest <id>, contract <name/version>)
Source/vertex alignment: validated by gflowui contract <name/version>
  (evidence <fingerprint>)
Internal graph fingerprint: constructor computed
gflow semantic version: <version>
gflow immutable build ID: <digest>
gflow runtime compatibility ID: <digest>
gflow source revision: <revision or embedded source ID>
```

## 8. Basin Inspector Layout

### 8.1 Placement

Add a collapsible, vertically resizable panel at the bottom of the right graph
viewer. It must:

- share the viewer width rather than the sidebar width;
- leave the graph above it;
- preserve its height and open/closed state across unrelated updates;
- support a practical minimum and maximum height;
- offer a maximize-table action; and
- avoid rebuilding the graph when resized.

Suggested structure:

```text
┌─────────────────────────────────────────────────────────────────────┐
│                         3D graph viewer                             │
│                                                                     │
├─ Basin Inspector ───────────────────────────────────────────────────┤
│ Max K: 6  Min K: 6  Rank: Primary mass  Display: Both  Opacity: .8 │
│ [Select displayed] [Clear] [Reset colors] [Compact/Full columns]    │
│ Show │ Color │ Type │ Rank │ Basin │ Extremum │ Value │ Support ...│
│  ☑   │   ■   │ Max  │  1   │ M1    │ v1574    │ ...   │ ...       │
│  ☐   │   ■   │ Min  │  1   │ m1    │ v2041    │ ...   │ ...       │
└─────────────────────────────────────────────────────────────────────┘
```

### 8.2 Table behavior

The table must provide:

- one row per returned maximum or minimum basin;
- a leading `Show` checkbox;
- a per-row color control;
- sticky headers;
- frozen Show, Color, Type, Rank, and Basin columns;
- horizontal scrolling;
- deterministic sorting;
- Maximum, Minimum, and All filters;
- Select displayed, Clear displayed, and Clear all actions;
- compact and full column modes;
- expandable row details for vertex lists and provenance; and
- accessible keyboard operation and labels.

Selection must be keyed by `direction + basin.id`, not row number.

Sorting, filtering, changing Top-K, opening/closing the inspector, and changing
columns must not lose selection or colors for basins still present in the
canonical object.

### 8.3 Column explanations

Every non-obvious heading must have a tooltip sourced from the canonical
summary's `column.definitions`.

The inspector must also provide a compact `Column definitions` disclosure.
Required definitions include:

- **Primary support:** vertices assigned uniquely to this basin in the stated
  flow direction.
- **Raw support:** all raw basin members before primary assignment and
  refinement; raw memberships may overlap, so support mass is coverage mass
  and need not sum to one across basins.
- **Retained support:** support remaining after canonical refinement stages;
  retained supports may overlap, so retained support mass is also coverage
  mass.
- **Primary support mass:** normalized mass of uniquely assigned vertices;
  unavailable when no vertex mass was supplied.
- **Raw allocated mass:** membership-weighted mass before primary assignment;
  unlike overlapping support mass, it is conserved within a direction when
  all raw memberships are represented. It may be unavailable for automatic
  ranking after refinements that change support without recomputing allocation.
- **Extremum value:** raw scalar-field value at the basin's representative
  local extremum.
- **Persistence:** birth-to-death field-level persistence; normally unavailable
  for unrefined trajectory-flow basins.
- **Retention status:** canonical reason that a basin remains or was removed.

Columns that are entirely unavailable for the selected construction method
should be hidden in compact mode rather than filled with unexplained dashes.

## 9. Basin Color and Display Controls

### 9.1 Per-basin colors

Each basin row receives a persistent color selected from a deterministic
default palette:

- maximum basins: warm/default qualitative sequence;
- minimum basins: cool/default qualitative sequence.

Color state is keyed by:

```text
project ID
graph set ID
graph k
field fingerprint
direction
basin ID
```

Changing row order or Top-K must not reassign existing colors.

### 9.2 Global display controls

The inspector toolbar should include:

- display mode: Maximum, Minimum, or Both;
- global basin opacity;
- unselected-vertex color;
- unselected-vertex opacity;
- Reset colors; and
- optional basin legend visibility.

### 9.3 Simultaneous maximum/minimum rendering

Every finite vertex has one maximum assignment and one minimum assignment.
One fill color cannot represent both without losing information.

Proposed encoding:

- maximum basin: vertex fill;
- minimum basin: larger halo/outline layer;
- extrema: existing maximum/minimum markers and optional labels;
- unselected vertices: neutral background styling.

Display mode behavior:

- **Maximum:** maximum fill only;
- **Minimum:** minimum fill only;
- **Both:** maximum fill plus minimum halo.

The legend and inspector help must state this mapping. If renderer limitations
prevent a clear halo in RGL, the implementation must fail over to a documented
alternative rather than claim equivalent rendering.

## 10. Server State and Invalidation Model

### 10.1 State objects

Introduce separate state for:

1. `basin_construction_state`
   - canonical `basin_complex`;
   - source and graph identity;
   - construction parameters;
   - cache provenance;
   - warnings and diagnostics.
2. `basin_summary_state`
   - requested/resolved ranking;
   - Top-K values;
   - canonical summary table;
   - column definitions.
3. `basin_selection_state`
   - selected maximum basin IDs;
   - selected minimum basin IDs.
4. `basin_color_state`
   - per-basin colors;
   - global opacity and background styling.
5. `basin_inspector_state`
   - open/closed;
   - height;
   - active type filter;
   - compact/full columns;
   - sort state.

### 10.2 Lifecycle

Use explicit statuses:

```text
unavailable
ready_to_compute
computing
ready
stale
failed
```

Source, graph, field, or construction-parameter changes make the construction
stale. Top-K, ranking, row selection, colors, inspector layout, and display
mode do not.

### 10.3 Cache key

The construction cache key must include:

- project ID;
- graph set ID;
- actual `k`;
- upstream-attested external graph fingerprint and attestation-evidence
  fingerprint;
- constructor-computed internal ordered-graph fingerprint;
- supplied ordered-`vertex.id` fingerprint;
- field/source fingerprint;
- immutable gflow build ID;
- gflow runtime-compatibility ID;
- gflow semantic package version as descriptive metadata, not identity;
- method, direction, and all resolved construction parameters; and
- vertex-mass fingerprint or an explicit no-mass marker;
- typed mass-provenance fingerprint; and
- full-domain validation status.

Summary and display settings must not be part of the construction cache key.

### 10.4 gflow build identity

The construction result and cache record must store the complete identity from
D-07. The combined build ID must be computed from immutable build inputs, not
from `packageVersion("gflow")`.

For source-loaded development:

- Git revision and dirty state are recorded;
- the versioned complete package-code manifest is generated and hashed;
- the loaded native artifact is hashed; and
- any package-code file addition, removal, or content change changes the
  combined build ID.

For installed packages:

- the package build embeds the versioned complete code-input manifest and its
  source/build digest;
- the installed native artifact digest is recorded; and
- absence of Git metadata does not collapse distinct builds to the same ID.

The manifest generator must be tested by mutating representative R, native,
namespace/configuration, and newly added code files and confirming that every
mutation changes the build ID.

The runtime-compatibility identity records R version, platform/architecture,
native ABI, and imported/linked dependency versions. Cache retrieval must
compare both the combined build ID and runtime-compatibility ID exactly. A
semantic-version match with either identity mismatch is a cache miss.

## 11. Failure and Warning Behavior

The UI must distinguish:

- missing or misaligned graph;
- missing or non-finite field;
- incomplete field domain;
- invalid or unaligned vertex mass;
- constructor-recomputable mass, supplied-vertex, or internal-graph mismatch;
- invalid mass-provenance schema;
- failed or missing required upstream source/alignment attestation;
- canonical constructor failure;
- threshold/fallback telemetry;
- incomplete assignments;
- stale cached result;
- gflow build-ID mismatch;
- unsupported renderer behavior; and
- summary/ranking errors.

Warnings must be visible in the Basin Inspector and construction details.
They must not be reduced to transient notifications only.

Explicit mass ranking without mass must produce a clear blocking message.
`auto` must resolve to support size without warning when mass is legitimately
absent.

## 12. Implementation Sequence

### Phase A: Canonical `gflow` summary

1. Define and test immutable gflow build identity from a versioned complete
   package-code manifest and runtime-compatibility identity.
2. Add explicit `vertex.id` and the layered typed mass-provenance
   constructor/object contract.
3. Write the generic ranked-summary contract, allocated-versus-coverage
   definitions, and availability tests.
4. Extend `summary.basin_complex()` without removing existing fields.
5. Add column definitions, direction-specific ranking status, and stored mass
   provenance.
6. Regenerate roxygen-derived documentation and `NAMESPACE`.
7. Run focused basin tests and package QA.
8. Install the validated gflow build used by gflowui and record its immutable
   build ID.

Exit condition: the canonical summary can independently rank and filter both
directions across supported availability patterns, and the canonical object
round-trips layered mass attestations without changing their verification
scope, plus immutable build/runtime provenance.

### Phase B: `gflowui` construction adapter

1. Replace the single-direction helper with a both-direction adapter.
2. Remove automatic finite-induced-subgraph construction.
3. Remove the precomputed-maxima semantic shortcut.
4. Add full-domain source validation, explicit ordered `vertex.id`, and typed
   source/mass attestation contracts.
5. Add immutable build- and runtime-aware cache identity.
6. Add server state and invalidation tests, including blocking non-finite
   fields before cache access.

Exit condition: an applied density or conditional expectation yields one
canonical object with aligned maximum and minimum assignments.

### Phase C: Sidebar and inspector

1. Remove `Flow direction`.
2. Add the revised sidebar controls.
3. Add the viewer-bottom resizable inspector.
4. Add summary table, checkboxes, column help, and persistent colors.
5. Add compact/full table modes.

Exit condition: basin selection and color control work without graph-state or
estimate-state resets.

### Phase D: Renderer integration

1. Add maximum fill rendering.
2. Add minimum fill rendering.
3. Add combined maximum-fill/minimum-halo rendering.
4. Integrate extrema markers and legends.
5. Verify state persistence under renderer and sidebar changes.

Exit condition: selected basins render consistently in all supported,
actually-tested renderers.

### Phase E: Acceptance and migration

1. Compare canonical reconstruction with existing precomputed reference assets.
2. Record expected differences rather than silently accepting them.
3. Benchmark uncached construction with trajectory storage on and off.
4. Record elapsed time, peak/process memory evidence, canonical object size,
   cache-hit latency, and final trajectory-storage choice.
5. Record a human-facing usability disposition based on measured values.
6. Run focused and full test suites.
7. Perform live QA on the Subject 15 reference project.
8. Update handoff and user-facing source-loaded app link.

Exit condition: discrepancies, validation evidence, and renderer coverage are
recorded explicitly.

## 13. Validation Matrix

### 13.1 `gflow`

- focused canonical basin-complex tests;
- plateau-flow tests;
- summary compatibility tests;
- mass-bearing RTCB and overlap-cell availability tests;
- overlapping-membership tests that distinguish coverage mass from conserved
  `raw.allocated.mass`;
- partial, all-zero, empty, and direction-specific ranking tests;
- layered mass-provenance, explicit-vertex-ID, verification-scope, and
  layer-specific mismatch tests;
- same-version/distinct-build identity and cache-key tests;
- complete code-input-manifest tests covering file changes, additions, and
  removals plus representative R/native/configuration inputs;
- runtime-compatibility identity tests;
- package documentation/build/check flow;
- installed-package verification;
- example with `direction = "both"` and mass absent;
- example with `direction = "both"` and normalized mass present.

### 13.2 `gflowui`

- source tests proving non-finite or incomplete fields fail before canonical
  construction and cannot populate or reuse a cache;
- source and mass contract tests;
- tests proving gflowui rejects external source, graph-ID, and vertex-order
  mismatches before construction and records its validation authority,
  contract version, algorithm, evidence fingerprint, and status;
- app-server tests for stale/ready transitions;
- no-reconstruction invalidation tests for Top-K, ranking, colors, and table
  state;
- row-selection and per-basin-color tests;
- maximum Top-K zero with minimum Top-K positive;
- minimum Top-K zero with maximum Top-K positive;
- both Top-K values zero;
- empty table, disabled/empty bulk selection, neutral graph rendering, and
  restoration after increasing Top-K;
- Plotly trace/data tests;
- RGL tests when the environment supports RGL;
- complete package test suite; and
- live source-loaded QA.

### 13.3 Reference-project acceptance

For `hmp_subject15_k03_heat_basin_path`:

- graph has 6,529 aligned vertices;
- path index 4 remains the Brier-selected default marker;
- both-direction canonical reconstruction succeeds;
- full-domain validation confirms all 6,529 field values are finite and aligned;
- maximum and minimum summaries are independently ranked;
- occupation-density `auto` resolves to primary support mass;
- changing Top-K does not recompute the basin complex;
- row checkboxes update the graph;
- colors persist across sorting, filtering, sidebar changes, and graph redraws;
- maximum fill and minimum halo are distinguishable;
- raw occupation probability is unchanged; and
- uncached elapsed time is recorded;
- peak resident-memory or equivalent process-memory evidence is recorded;
- canonical object size is recorded;
- cache-hit latency is recorded;
- trajectory storage on/off is benchmarked and the final fixed choice is
  recorded;
- a human-facing usability disposition records whether the measured behavior is
  acceptable or requires optimization; and
- the source-loaded app link is reported.

The first benchmark establishes the baseline; this plan does not impose an
uninformed hard latency or memory threshold in advance.

## 14. Acceptance Gates

Implementation is not complete unless:

1. `Flow direction` is absent from the Basins sidebar.
2. Provenance shows `direction = "both"` and fixed CLOSEST settings.
3. Generic conditional expectations are never silently converted to mass.
4. `auto` reports its resolved ranking measure.
5. `auto` never selects an all-NA, partially unavailable, or all-zero measure
   and resolves independently by direction.
6. Explicit ranking is unavailable or errors when its measure is unusable.
7. Constructor-computed facts, constructor-validated declarations, and
   upstream attestations remain distinctly labeled through the canonical
   object, summary, cache, and construction details.
8. `gflow` rejects recomputable mass, supplied-vertex-ID, and internal-graph
   mismatches; `gflowui` rejects external source, graph-ID, and vertex-order
   mismatches before construction.
9. Every upstream attestation records authority, validator/contract version,
   algorithm, evidence fingerprint, and status, and is never displayed as
   constructor verification.
10. Non-finite or incomplete fields block before construction and cache access.
11. Same-version gflow builds with different build identities cannot share a
    cache entry.
12. Adding, removing, or changing any package code input changes the versioned
    code-manifest digest, and runtime-compatibility mismatches are cache misses.
13. Coverage mass and membership-allocated mass have distinct definitions;
    `auto` uses allocated mass only while its allocation-current provenance is
    true.
14. Maximum and minimum Top-K changes do not reconstruct the canonical object.
15. All three zero-Top-K combinations produce valid empty/partial table and
    neutral-render states and recover when Top-K is increased.
16. The bottom inspector remains usable at the supported desktop width.
17. Every displayed non-obvious column has a definition.
18. Selection and color state survive unrelated UI updates.
19. Precomputed and freshly canonical results are not silently mixed.
20. Generated gflow documentation is regenerated from canonical roxygen
    sources.
21. Reference elapsed time, memory, object size, cache-hit latency, trajectory
    storage choice, and usability disposition are recorded.
22. Validation records distinguish Plotly-only QA from actual RGL QA.

## 15. Files Expected to Change During Implementation

Expected `gflow` source areas:

- `R/basin_complex.R`
- a build-identity source/helper, versioned complete code-input-manifest
  generator, and embedded build metadata mechanism;
- focused basin-complex tests;
- roxygen-generated `man/summary.basin_complex.Rd`;
- `NAMESPACE` only if the documented public contract requires generated export
  changes.

Expected `gflowui` source areas:

- `R/basin_display_helpers.R`
- basin-owned portions of `R/app_server.R`
- `R/app_ui.R` or a new viewer-inspector UI helper;
- `inst/app/www/styles.css`;
- a narrowly scoped JavaScript file if resize/table state requires it;
- `tests/testthat/test-occupation-density.R`;
- `tests/testthat/test-app-constructs.R`; and
- renderer-focused tests.

This list is an implementation forecast, not authorization to modify unrelated
files.

## 16. Out of Scope

- changing CLOSEST to another trajectory modulation;
- interactive plateau-policy changes;
- interactive edge-threshold or fallback changes;
- automatic conversion of conditional expectations into probability mass;
- automatic induced-subgraph construction from non-finite fields;
- redefining upstream occupation-density estimates;
- altering Brier selection;
- changing the frozen reference graph or layout;
- editing derived HMP assets by hand;
- reviving retired `create.basin.cx()` or its object schema; and
- interpreting local extrema or selected basins as EODs without an upstream
  scientific specification.
