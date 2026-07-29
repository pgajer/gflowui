# Basin Renderer Final-State QA

Date: 2026-07-28

- Project: `hmp_subject15_k03_heat_basin_path`
- Estimate: Subject 15 graph-heat occupation density, path index 4
- Source asset fingerprint: `c44680dd6a0e0f389dc294d2f7d2411309e872e3426c0ed6761263a42476ec3a`
- Selected field fingerprint: `32b46729ef18eb0b3f6f38a6fde1d67e914788d2614e4ac3bd84c669c8dc0ee5`
- Alignment evidence fingerprint: `dce5c9f91e855eeb2ebb9eedc60456516fb88f584649e201919a56173c0590ce`
- gflow build ID: `a740081d7dd9065ea23f1386a1799eb3`
- gflow runtime ID: `ad73a623d86e8825929530a2ae70e91a`
- Selected basin keys: `max|basin_max_v00001598`, `max|basin_max_v00001628`, `max|basin_max_v00001635`, `max|basin_max_v00001575`, `max|basin_max_v00001641`, `max|basin_max_v00001578`, `min|basin_min_v00000847`, `min|basin_min_v00002090`, `min|basin_min_v00000481`, `min|basin_min_v00003777`, `min|basin_min_v00003623`, `min|basin_min_v00004410`
- Layer specifications: 6 maximum fills, 6 minimum halos
- Plotly: passed; 13 total traces, 6 maximum fills, 6 minimum halos
- RGL: passed on null RGL device; 6 minimum marker layers
- Diagnostics: no construction, alignment, Plotly-build, or RGL-layer errors

## Reproduction

```sh
Rscript /Users/pgajer/current_projects/vaginal_community_trajectory_types/analysis/291_register_hmp_subject15_k03_gflowui_project.R
cd /Users/pgajer/current_projects/gflowui
Rscript dev/qa_basins_renderer_reference.R
Rscript -e 'pkgload::load_all(".", quiet=TRUE); gflowui::run_gflowui(host="127.0.0.1", port=3867, launch.browser=FALSE)'
```
