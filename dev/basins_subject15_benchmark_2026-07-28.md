# Subject15 Canonical Basin Benchmark

Date: 2026-07-28 17:44:27 EDT

Repository:
`/Users/pgajer/current_projects/gflowui`

Benchmark script:
`/Users/pgajer/current_projects/gflowui/dev/benchmark_basins_reference.R`

Reference project:
`hmp_subject15_k03_heat_basin_path`

## Input Validation

- Graph vertices: 6,529.
- Brier-selected conditional-expectation path index: 4.
- Field values: 6,529 finite values; the full graph domain was used.
- Occupation-density mass was passed through unchanged.
- Construction direction: `both`.
- Maximum assignment rows: 6,529.
- Minimum assignment rows: 6,529.
- Maximum basins: 352.
- Minimum basins: 841.
- `rank.by = "auto"` resolved to `primary.support.mass` independently for
  maximum and minimum summaries.

## Measurements

| Configuration | Elapsed | `Rprofmem` allocated bytes | Largest recorded allocation | Canonical object size |
|---|---:|---:|---:|---:|
| Trajectories omitted | 9.667 s | 15,005,582,960 | 5,261,816 | 56,401,880 |
| Cache hit | 0.119 s | 20,800,760 | not separately interpreted | reused object |
| Trajectories stored | 5.876 s | 14,986,750,128 | 5,261,816 | 61,890,832 |

Whole-process evidence reported by `/usr/bin/time -l`:

- real time: 19.41 seconds;
- maximum resident set size: 779,943,936 bytes;
- peak memory footprint: 613,827,616 bytes.

The stored-trajectory canonical object was 1.0973 times the size of the object
without trajectories, or approximately 9.7% larger.

## Disposition

The application uses `return.trajectories = FALSE`. The inspector and renderers
consume assignments, summaries, memberships, and extrema, but do not consume
the individual gradient paths. Omitting them therefore reduces retained object
size without removing a UI capability.

An uncached construction of about 9.7 seconds followed by a 0.12-second cache
hit is acceptable for the current desktop workflow. The roughly 780 MB
maximum resident-set measurement is not a release blocker for a single local
session, but it is material enough to monitor before supporting concurrent
sessions or substantially larger graphs.

## Measurement Limitations

- The trajectory-storing run followed the no-trajectory run in the same R
  process, so the elapsed times are warm-order measurements and are not a
  controlled speed comparison. They are sufficient for object-retention
  choice because the UI does not use the stored paths.
- `Rprofmem` reports cumulative allocation traffic, not peak live memory.
  `/usr/bin/time -l` supplies process-level peak evidence for the combined
  benchmark process rather than each configuration separately.
- The recorded gflow build identity was produced from a dirty source tree
  before the implementation commit. It identifies the benchmarked source
  content but is not the final installed post-commit Git description.
- No precomputed basin asset was treated as a canonical comparison target; the
  benchmark validates the newly reconstructed object and preserves the input
  occupation probability rather than asserting basin-ID equality with a
  legacy implementation.

## Reproduction

From `/Users/pgajer/current_projects/gflowui`:

```sh
/usr/bin/time -l Rscript dev/benchmark_basins_reference.R
```
