# Project configuration cleanup

01_oct_2026

The legacy **AGP** viewer registration was retired. Its two graph collections,
15 endpoint-run registrations, original manifest, and research assets remain
available locally. The separate **American Gut: Fermat graphs and dominant
CSTs** project remains registered with its 76 graph collections.

**Symptoms** now uses explicit feature, taxonomy and subject files declared in
its manifest. Its four graph collections, conditional-expectation collection,
endpoint run, and saved defaults were preserved. Its preparation and registration
code lives in the Symptoms repository, rather than in gflowui.

## Application changes

- Removed implicit searches for legacy endpoint sweeps, metadata, graph dimensions,
  selection diagnostics and layout indexes. Symptoms now declares its aliases,
  metadata source and diagnostic paths explicitly.
- Removed AGP and Symptoms runtime branches and their dataset-specific discovery
  profiles. Folder names no longer determine project behavior.
- Added an optional `taxonomy_map_file` to the generic endpoint provider.
  Feature IDs are retained; taxonomy supplies readable labels.
- Opening defaults can be supplied through `open_graph_set_id`, `open_graph_k`
  and `open_panels` in the manifest's `defaults` list.
- Removed the Arms module's fallback that loaded a developer-specific gflow
  checkout. It now uses the loaded/installed gflow dependency and reports a
  missing capability explicitly.

Old calls using `symptoms_restart` or `agp_restart` must be replaced with explicit
`custom` manifests. These import profiles were removed, not silently reinterpreted.
The reusable quadratic-surface benchmark format remains supported.

## Reproduction and recovery

Symptoms' `R/register_gflowui.R` takes the project root and taxonomy RDA file as
explicit arguments. `R/gflowui_project_config.R` records the existing collections
with project-relative asset paths. The script preserves a current registration's
collections and preferences; in an empty registry it builds a new registration
from that configuration. Generated matrices and source checksums remain in
`results/gflowui_manifest_assets/`, outside Git.

`dev/retire_project_registration.R` archives a project's registry row and manifest
before unregistering it with `delete_manifest = FALSE`. For legacy AGP, the
snapshot is under the gflowui user-data directory at
`projects/retired/agp.manifest.rds/20261001-183138/registration.rds`.
No assets were moved to Trash or deleted. The snapshot preserves all artifact
references; reactivation with the new application would require an explicit
custom manifest for any formerly implicit live data providers.

## Verification

The migrated Symptoms providers reproduce all 3,438 sample rows, 101 feature
columns, taxonomy mappings, and the 101 subjects from the previous providers.
Every graph collection uses the same preserved sample ordering. Generic subject
loading converts the integer visit-order field to numeric without changing its
values. Existing real-project endpoint and subject-overlay checks passed (35
assertions). Browser inspection confirmed Symptoms opens and retains endpoint
labels; the project picker omits legacy AGP and retains the newer Fermat project.

Synthetic regression tests exercise arbitrary project IDs, former special IDs,
explicit taxonomy and subject assets, identity-independent opening defaults,
folder-name independence, and retirement with asset preservation. Registry, graph selection, rendering,
Arms, provenance, Trash and project-order suites also passed. This is focused
regression validation, not a full CRAN check.
