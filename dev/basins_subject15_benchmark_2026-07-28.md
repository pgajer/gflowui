# Subject15 Canonical Basin Benchmark

Date: 2026-07-28 21:47:44 EDT

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
- The source asset was reread after construction and its field was exactly
  identical to the initially loaded source vector.
- The UI adapter's probability normalization differed from the source vector
  by at most `5.20417042793042e-18`; exact identity is therefore false but the
  numerical difference is negligible.
- The constructed object's `field$input.values` and
  `field$vertex.mass.input` were each exactly identical to the normalized
  adapter field.
- The maximum absolute difference between the source vector and the
  constructed field was `5.20417042793042e-18`.
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
| Trajectories omitted | 10.680 s | 15,005,584,040 | 5,261,816 | 56,406,104 |
| Cache hit | 0.127 s | 20,886,848 | not separately interpreted | reused object |
| Trajectories stored | 6.417 s | 14,986,973,136 | 5,261,816 | 61,895,056 |

Whole-process evidence reported by `/usr/bin/time -l`:

- real time: 21.54 seconds;
- maximum resident set size: 798,720,000 bytes;
- peak memory footprint: 604,177,536 bytes.

The stored-trajectory canonical object was 1.0973 times the size of the object
without trajectories, or approximately 9.7% larger.

## Disposition

The application uses `return.trajectories = FALSE`. The inspector and renderers
consume assignments, summaries, memberships, and extrema, but do not consume
the individual gradient paths. Omitting them therefore reduces retained object
size without removing a UI capability.

An uncached construction of about 10.7 seconds followed by a 0.13-second cache
hit is acceptable for the current desktop workflow. The roughly 799 MB
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
- The recorded gflow build identity is from clean source revision
  `af1fc4e53365e421440c0d0ac71a01ad0f91fb52`, build ID
  `a740081d7dd9065ea23f1386a1799eb3`, and runtime ID
  `ad73a623d86e8825929530a2ae70e91a`.
- No precomputed basin asset was treated as a canonical comparison target; the
  benchmark validates the newly reconstructed object and preserves the input
  occupation probability rather than asserting basin-ID equality with a
  legacy implementation.

## Reproduction

From `/Users/pgajer/current_projects/gflowui`:

```sh
/usr/bin/time -l Rscript dev/benchmark_basins_reference.R
```
