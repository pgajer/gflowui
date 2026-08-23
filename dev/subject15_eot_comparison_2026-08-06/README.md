# Subject 15 maximum-basin/EOT comparison

This directory contains a reproducible comparison of all 352 maximum basins for
the selected Subject 15 occupation-density estimate
`HMP_S15_K03_ETA_04` (`eta = 0.953`).

## Outputs

- `subject15_maximum_basin_eot_comparison.csv` is the complete basin-level
  comparison table.
- `subject15_visit_basin_assignments.csv` records the allocation of all 70
  observed Subject 15 visits.
- `subject15_eot_visit_threshold_sensitivity.csv` shows the descriptive
  consequences of requiring at least 1, 2, 3, 5, or 10 directly observed
  visits. These are sensitivity thresholds, not an accepted EOT definition.
- `subject15_m17_local_flow.csv` records the three neighbors of M17, each
  neighbor's selected CLOSEST ascent step, and its final root.
- `subject15_eot_comparison_summary.rds` records key checks, findings, and
  source provenance.
- `latex_report/subject15_eot_comparison_report.tex` is the auditable technical
  report source. Its PDF contains the definitions, four vector figures, the
  complete 17-basin observed table, the threshold analysis, and the exact M17
  local-flow case study.

## Metric definitions

- **Graph support** is the number of the 6,529 graph vertices whose deterministic
  CLOSEST ascent root is the basin maximum.
- **Density mass** is the selected normalized occupation-density mass summed
  over those graph-support vertices.
- **Subject visit count** is the number of the 70 observed Subject 15 sample
  vertices assigned to the basin by the same CLOSEST root map.
- **Observed run count** counts contiguous blocks of the same basin label in the
  ordered observed-sample sequence. A return is any run after the first.
- **Midpoint time weight** assigns an observation half the time to its preceding
  observed sample plus half the time to its following observed sample. Endpoint
  observations receive half of their single adjacent interval. The weights span
  the continuous 71-day interval from observed day 1 to observed day 72.

The midpoint time weight is included as a descriptive longitudinal sensitivity.
It is not treated as an EOT estimator or as evidence that the process was in
quasi-equilibrium.

## Reproduction

Run:

```sh
Rscript dev/subject15_eot_comparison_2026-08-06/derive_subject15_eot_comparison.R
```

The script validates row grains, raw-to-displayed basin identity, complete visit
assignment, graph-support conservation, density-mass conservation, visit-count
conservation, and midpoint-exposure conservation before writing the outputs.

To regenerate the analysis, vector figures, LaTeX report, and final PDF in one
step, run:

```sh
dev/subject15_eot_comparison_2026-08-06/latex_report/build_subject15_eot_comparison_report.sh
```

The release PDF is copied to
`output/pdf/subject15_eot_basin_comparison_report.pdf`.
