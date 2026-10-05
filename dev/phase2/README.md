# Classification catalogues and coverage presets

This optional project configuration adds pure unordered CST annotations and
fixed-reference coverage filters to sample layouts. Projects without the
configuration keep the ordered dCST controls and their existing behavior.

The catalogue is an RDS asset declared in
`manifest$metadata$classification_catalogue$file`. Relative paths resolve from
`project_root`. It contains:

- `samples`: unique `sample_id`, `udcst_level1`, `udcst_level2` columns.
- `levels`: named definitions with display labels.
- `subsets`: named definitions, including `All`, each with immutable sample
  `ids`, a display `label`, retained-state count and classification `policy`.
- `pairs`: unique group labels with original feature names `a` and `b`.
  Unordered pairs use canonical feature-dictionary order, never current
  dominance. The two directions of dominance share one chart.
- `palettes`: named colors per classification level; project overrides live
  in `metadata$classification_catalogue$palettes`.
- Dictionary, orientation and source checksum for reproducibility.

Ordered `dcst_level1:3` annotations remain in the existing graph assets. The
catalogue adds `udcst_level1:2` by sample ID, including in local views. Missing
IDs receive missing annotations; All preserves them, while a named core
contains only its explicitly saved IDs.

`defaults$classification_state` saves the selected subset, classification type,
last level for each type, checkbox groups per level and color source. This is
independent of layout identity. Dropdown label-menu defaults remain separate
and retain their normal precedence when applied by the browser. Palette edits
are saved immediately; control changes are saved after a short debounce.

Color by and CST type share one classification choice. Type changes preserve
source-dataset/numeric coloring; selecting dCST or udCST coloring changes type.
Switching from ordered level 3 to unordered restores the last unordered level.

The renderer's existing `keep_idx` mask now intersects the coverage preset,
CST groups, source datasets, component selection, local preview and linked
selection. Both 3D renderers, endpoints, arms, subject edges, the linked 2D
plot and the configured project's source cross-table consume that mask.
Coverage does not recompute paths, layouts, or connected components. Source
summary/table counts explicitly describe the current region before filters;
the cross-table describes currently visible compositions. A valid empty mask
remains empty in both renderers, endpoint visibility and export. Arm paths preserve
gaps, and subject displays filter original temporal edges without bridging
hidden visits.

Above 40 categories, configured projects use one per-vertex-colored 3D trace;
the full CST table is the legend. Smaller selections retain normal legends.
This avoids thousands of traces for the 1,192 observed unordered pair states.

## Copy, validation and deployment

`prepare-copy.R` takes the research proposal directory and a new registry
folder. It copies mutable project assets, rebases registry-owned paths, and
shares numerical graph/layout inputs read-only. Run the copied app with
`options(gflowui.projects_data_dir = copy_folder)` on a separate port.

`validate-data.R` checks the full saved memberships under two metrics and two
embedding routes, all saved atlas regions, and a canonical unordered pair
containing both dominance directions. `browser-check.mjs` checks rendered
membership, controls, linked views and persistence in the isolated instance.
Run `prepare-browser-fixtures.R copy_folder` before the browser checks. Its
local expected-ID JSON is made from the saved catalogue, not inferred from
rendered counts. The browser scripts target the isolated app on port 3875.

`deploy.R` takes the checked copy and a fresh backup directory. It backs up the
live manifest, registry and mutable project assets, then adds only the catalogue
configuration and initial classification controls to the **latest live manifest**.
It never deploys the test copy's endpoints, palettes, selections or atlas state.
An equality check verifies that all other manifest fields survive unchanged.
Restart the existing app from its original launch script; reload the viewer.

For immediate rollback before subsequent user edits, restore the backed-up
manifest and restart. After new user edits, remove only
`metadata$classification_catalogue` and `defaults$classification_state` from the
current manifest instead of replacing it wholesale. The additive asset can
remain on disk. Existing ordered annotations and saved layouts need no rollback.

This implements sample-view Phase 2. State graphs, witness inspection, robust
geometric filtering and registration of recomputed core regions are later phases.
