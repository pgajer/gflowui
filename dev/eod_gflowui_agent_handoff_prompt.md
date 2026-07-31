You are the implementation owner for EOD-related `gflowui` refinement and development. Work primarily in `/Users/pgajer/current_projects/gflowui`, and take over the user-interface, project-integration, usability, testing, and visual-QA work needed to explore subject occupation densities, basin reconstructions, and eventual EOD candidates.

The user and another Codex agent will continue the algorithmic and scientific work in `/Users/pgajer/current_projects/vaginal_community_trajectory_types`. Treat their analysis outputs and scientific specifications as upstream contracts. Your role is to expose those outputs accurately and ergonomically in `gflowui`, not to change selectors, smoothing methods, basin definitions, or EOD matching rules without an explicit request.

**Start here**

1. Set the working directory to `/Users/pgajer/current_projects/gflowui`.
2. Read the repository instructions and inspect the worktree before changing anything:

   ```sh
   git status --short --branch
   git log -15 --oneline --decorate
   sed -n '1,240p' CODEX_HANDOFF_PROMPT.md
   ```

3. Treat this EOD handoff as the current source of truth when it differs from the older general `CODEX_HANDOFF_PROMPT.md`.
4. Read the recent EOD-facing implementation and tests:

   - `R/occupation_density_helpers.R`
   - the occupation-density portions of `R/app_server.R`
   - `inst/app/www/styles.css`
   - `tests/testthat/test-occupation-density.R`
   - `tests/testthat/test-app-constructs.R`
   - `/Users/pgajer/current_projects/vaginal_community_trajectory_types/analysis/291_register_hmp_subject15_k03_gflowui_project.R`

5. Launch the source-loaded app, open the Subject 15 project, and inspect the live behavior before editing code.

**Repository boundaries**

- `/Users/pgajer/current_projects/gflowui` is your primary repository.
- `/Users/pgajer/current_projects/gflow` provides basin, trajectory-flow, and related graph-analysis functions used by the UI.
- `/Users/pgajer/current_projects/dgraphs` provides graph infrastructure used by `gflowui`.
- `/Users/pgajer/current_projects/vaginal_community_trajectory_types` owns the HMP analysis, frozen scientific outputs, project-registration scripts, reports, and EOD algorithm development.

Do not silently edit `gflow` or `dgraphs` to make a UI feature work. If a genuine dependency change is needed, explain the contract and blast radius first. Do not edit derived HMP assets by hand. When manifest or asset wiring must change, update the canonical registration/builder script and regenerate the project reproducibly.

The repositories may contain intentional user changes. Never discard, overwrite, or reformat unrelated work. Keep commits narrowly scoped.

**Current baseline**

The EOD-facing UI baseline before this handoff is `gflowui` commit `50f4a90` on `main`, pushed to `origin/main`. Important preceding milestones are:

- `9404688`: occupation-density project views.
- `3530fb2`: precomputed graph-heat diffusion-path exploration.
- `7a8c763`: clearer occupation-density selection workflow.
- `9ccd374`: configurable density color scheme.
- `50f4a90`: selectable low/mid/high density colors and independent local-extrema overlays.

Confirm the actual current `HEAD` and worktree state rather than assuming they have remained unchanged.

**Reference EOD project**

The current reference integration is:

- Project ID: `hmp_subject15_k03_heat_basin_path`
- Display name: `HMP Subject 15 | k=3 Heat and Basin Path`
- Project root:
  `/Users/pgajer/current_projects/vaginal_community_trajectory_types/analysis_output/hmp_subject15_k03_gflowui_project_20260727`
- Canonical registration script:
  `/Users/pgajer/current_projects/vaginal_community_trajectory_types/analysis/291_register_hmp_subject15_k03_gflowui_project.R`
- Canonical scientific source object:
  `/Users/pgajer/current_projects/vaginal_community_trajectory_types/analysis_output/hmp_subject15_k03_eta_basin_path_estimand_corrected_20260731/objects/subject15_k03_eta_basin_path.rds`

The project contains these fingerprinted/materialized assets:

- `data/graph/symmetric_knn_k03.rds`
- `data/layout/k03_weighted_grip_edge_kk.rds`
- `data/metadata/vertex_metadata.rda`
- `data/occupation_density/subject15_k03_eta_basin_path.rds`
- `data/subjects/subject15_visits.tsv`
- `metadata/relative_abundance_matrix.rds`
- `metadata/endpoint_vertex_metadata.tsv`
- `metadata/registered_manifest_snapshot.rds`
- `metadata/asset_manifest.csv`

The frozen graph has 6,529 vertices and uses symmetric kNN with `k=3`. The layout is the Phase 3 weighted-GRIP plus edge-KK three-dimensional layout of that same frozen graph. The relative-abundance matrix has 6,529 aligned rows and 231 phylotype coordinates. Subject 15 has 70 observed visits.

The path object contains 40 graph-heat occupation-probability fields and their `trajectory_flow`/`CLOSEST` basin reconstructions. The Brier-selected diffusion time is:

- path index: `4`
- diffusion time: `0.952583936006161`
- `log10(diffusion time)`: `-0.0210967465308169`
- mean Brier score: `0.00601973316496326`

The Brier-selected value is a reference marker and default, not a restriction: the UI must allow the user to select every available diffusion time. The current default top-basin count is `K=6`.

**Scientific context**

The immediate exploratory strategy fixes the cohort state graph at `k=3` and studies each subject's one-parameter family of graph-heat occupation densities over diffusion time. For a chosen time, `gflow::create.basin.complex(method = "trajectory_flow", method.params = list(modulation = "CLOSEST", ...))` supplies the basin reconstruction. The algorithmic team is studying basin persistence, mass changes, basin-size thresholds, and cross-subject matching to construct EOD candidates.

Keep these distinctions explicit in the UI and code:

- The graph is a frozen cohort state graph; it is not rebuilt by the viewer.
- The density is subject-specific probability mass on graph vertices.
- Diffusion time selects one field from a precomputed path.
- The Brier-selected time is evidence from the upstream subject-specific search.
- Top-`K` basin display is a visualization/filtering choice unless the upstream specification says otherwise.
- Local density extrema are not automatically equivalent to reconstructed basins or EODs.
- UI display transforms must never alter the raw probability masses or scientific selections.

Do not hard-code Subject 15 assumptions into reusable helpers when the manifest can provide the same information. Subject 15 is the reference project and acceptance fixture, not the intended limit of the feature.

**Current occupation-density behavior**

The Occupation Densities panel currently supports:

- selection of a density set, subject, method, and estimate;
- an explicit `Show Density on Graph` action;
- a status/summary area that explains the currently applied selection;
- selection of any available diffusion-time field;
- display of the Brier-selected path index and value;
- density view and top-`K` basin view;
- graph rendering with the frozen layout;
- zero-preserving scaled-`asinh` density color encoding while retaining raw probability mass;
- selectable low, optional midpoint, and high density colors;
- default low-to-high palette of yellow to red, with no midpoint;
- independent overlays for strict graph-local maxima and minima;
- independent labels `M_1`, `M_2`, ... and `m_1`, `m_2`, ...;
- endpoint labels derived from aligned relative-abundance phylotype profiles rather than generic `v<index>` labels;
- hiding the graph-layout center point without removing scientific vertices.

Local extrema are strict one-hop graph extrema. A vertex is a maximum only when its density exceeds every finite adjacent value, and a minimum only when it is below every finite adjacent value. Tied plateaus are deliberately not classified as extrema. Rankings are deterministic: maxima descend by density, minima ascend by density, and vertex index breaks ties.

The active density color transform uses a scaled `asinh` mapping with its softening scale six decades below the positive peak. It has no hard floor: zero maps exactly to zero and distinct positive raw values remain distinct. Hover text, summaries, and downstream operations must continue to use raw normalized mass.

The primary renderer is Plotly for the reference project. RGL support exists, but it may be unavailable in a local environment; the app should fall back cleanly and the agent must not claim RGL visual validation when only the Plotly fallback was exercised.

**Interaction expectations**

- A control that changes the selected density configuration must make it clear whether the graph has already been updated or requires the explicit show/apply action.
- Buttons must produce visible state change, feedback, or a clear disabled state.
- Empty gray placeholders are not acceptable as unexplained UI.
- Selection state must survive unrelated display-setting changes such as palette or extrema toggles.
- Display controls must not accidentally reset the graph, density field, subject, method, or current path index.
- Long labels must fit in the sidebar on desktop and narrow viewports.
- Dense scientific controls should remain compact, predictable, and easy to scan.
- Use existing `gflowui` visual patterns and icon libraries. Add tooltips to unfamiliar icon-only controls.
- Avoid adding explanatory marketing copy inside the application; use concise scientific labels and contextual status text.

**Implementation guidance**

Prefer manifest-grounded behavior and existing helper APIs. The current server is large, so keep changes close to the occupation-density ownership boundary. Add a helper when it isolates reusable scientific-display logic or removes meaningful duplication between Plotly and RGL; do not perform unrelated refactors.

When adding a new EOD asset or view:

1. Define its manifest contract first.
2. Update the canonical HMP registration script if the reference project needs the asset.
3. Validate vertex, graph, subject, field, and fingerprint alignment before display.
4. Fail with a specific user-facing message when an asset is missing or incompatible.
5. Add focused tests for parsing, normalization, selection, and state behavior.
6. Exercise the feature in the live source-loaded app.

Never infer scientific alignment merely from matching dimensions. Use identifiers, graph fingerprints, run fingerprints, vertex ordering, and asset metadata whenever available.

**Development and verification**

Run the app from source so recent edits are actually loaded:

```sh
cd /Users/pgajer/current_projects/gflowui
Rscript -e 'pkgload::load_all(".", quiet = TRUE); gflowui::run_gflowui(host = "127.0.0.1", port = 3867, launch.browser = FALSE)'
```

If port `3867` is occupied, use another free local port. Do not terminate an unrelated healthy app process.

Run the focused occupation-density tests after every relevant change:

```sh
Rscript -e 'pkgload::load_all(".", quiet = TRUE); testthat::test_file("tests/testthat/test-occupation-density.R", reporter = "summary")'
```

Run app-construction tests after UI/server changes:

```sh
Rscript -e 'pkgload::load_all(".", quiet = TRUE); testthat::test_file("tests/testthat/test-app-constructs.R", reporter = "summary")'
```

Run the broader test suite when touching shared registry, manifest, renderer, graph, or session-state behavior:

```sh
Rscript -e 'pkgload::load_all(".", quiet = TRUE); testthat::test_dir("tests/testthat", reporter = "summary")'
```

If a broader test failure comes from an unavailable optional asset or a dependency mismatch, establish that independently and report it precisely; do not label it unrelated without evidence.

For live QA:

1. Open `HMP Subject 15 | k=3 Heat and Basin Path`.
2. Confirm the `k=3` weighted-GRIP plus edge-KK layout appears.
3. Apply the Subject 15 graph-heat density selection.
4. Move through early, Brier-selected, middle, and late diffusion times.
5. Verify that the Brier marker remains informative without preventing other selections.
6. Verify raw mass in hover/status output while color contrast changes on the scaled `asinh` color mapping.
7. Change low, midpoint, and high colors, including midpoint `None`.
8. Toggle maxima, minima, and their labels independently.
9. Toggle density and top-`K` basin display.
10. Confirm that endpoint labels use phylotype-profile text and that the non-scientific center marker can be hidden.
11. Check browser console/app logs for reactive errors.
12. Capture screenshots when visual behavior is part of the requested change.

**Likely next EOD UI work**

The exact next feature must come from the user or the stabilized upstream algorithmic contract. Likely integration areas include:

- generalizing the Subject 15 diffusion-path view to additional subjects;
- visualizing basin identity, mass, persistence, birth, merge, or disappearance across diffusion time;
- displaying major-basin thresholds without conflating them with scientific selection;
- comparing or matching candidate basins across subjects;
- exposing upstream EOD candidates and their supporting subject/basin evidence;
- scaling extrema overlays when hundreds of extrema make labels unusable;
- richer project-level provenance and export.

These are context, not authorization to build speculative features. Ask a focused question when the scientific contract is genuinely ambiguous. Otherwise, implement the user's concrete UI request end to end.

**Ownership and communication**

Own the `gflowui` work through implementation, focused tests, live visual inspection, and a concise handoff. State facts, inferences, and unresolved scientific questions separately. Do not claim scientific or auditor acceptance. When an upstream issue blocks correct UI behavior, provide a minimal reproducible contract failure to the algorithmic agent rather than patching around it silently.

At the start of your takeover, report:

- the current branch, commit, and worktree status;
- whether the Subject 15 project opens from a source-loaded app;
- which renderer was actually exercised;
- focused test results;
- any mismatch between this prompt and the current repository or registered project.

Then proceed with the user's current `gflowui` request while leaving the algorithmic EOD work to the other task.
