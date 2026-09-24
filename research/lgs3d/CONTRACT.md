# Standalone JSON/CSV contract, version 1

This POSIX Python CLI runs `lgs-paper-union-v1`. It requires `ps`, Python 3.12,
and the task-local locked NumPy environment. It does not load the upstream native
reference kernel and does not depend on gflowui. Windows process supervision is
not implemented. The request and response JSON Schemas are in `schemas/`;
semantic checks below additionally validate graph and file contents.

From the repository root:

```sh
research/lgs3d/.venv/bin/python research/lgs3d/run.py research/lgs3d/fixtures/adapter_k4/request.json
```

The CLI prints exactly one response JSON object to stdout and writes it to
`<output_directory>/response.json`. Exit 0 means completed; 1 means rejected,
failed, limited, or cancelled; 2 means another invocation owns that output
directory (or argparse usage error). An output-directory lock prevents concurrent
publication. A busy response is printed without replacing the active run's file.
Use `--output-dir PATH` to override the request's output location. All relative
paths, including this override, resolve relative to the request file directory.
Default output is `lgs3d-output` beside the request. JSON duplicate keys and NaN
or Infinity literals are rejected. Requests are limited to 1 MiB.

## Inputs and identity

Required fields: `schema_version` (1), `graph_id`, `graph_sha256`, `vertex_file`,
`vertex_sha256`, `edge_file`, `edge_sha256`, `dimension` (2 or 3), `seed` (unsigned
64-bit integer), `locality_k` (1 through n-1), and `parameters`. Booleans do not
count as numbers. IDs are nonempty case-sensitive strings, compared by Unicode
code point for ties. No implicit ID or coordinate reordering is allowed.

* Vertex CSV: exact header `vertex_id`; unique nonempty IDs; declared order is
  retained in the output. Input file admission limit: 1 MiB.
* Edge CSV: exact header `source,target,length`; exactly one undirected edge per
  row, known endpoints, no self loops, duplicates, or disconnected vertices.
  Lengths must be finite, positive, and exactly 1 as parsed float64. Weighted
  graphs are unsupported. Input limit: 16 MiB and 100,000 edges.
* Optional initial coordinates: provide both `initial_coordinate_file` and
  `initial_coordinate_sha256`. Header is `vertex_id,x,y,z` in 3D or
  `vertex_id,x,y` in 2D, with IDs in exactly declared order and finite values.
  Limit: 2 MiB. Coincident or too-close points cause a reasoned numerical failure;
  there is no jitter or silent repair. Omit both fields for canonical seeded
  initialization described in METHOD.md.
* Exactly 2 through 2,000 vertices are admitted. Smaller inputs are unsupported;
  larger inputs exceed this experimental admission limit. Component placement
  belongs to the main project.

Checksums are lowercase hexadecimal SHA-256. File checksums cover raw bytes,
including line endings and row order. Graph SHA-256 is the hash of UTF-8 canonical
JSON with these fields: `{"edge_sha256":E,"format":"csv-graph-v1","vertex_sha256":V}`.
Canonical encoding sorts object keys, uses no whitespace or ASCII escaping, and
forbids nonfinite numbers. `E` and `V` are quoted checksum strings. Use
`lgs_paper.adapter.graph_hash(V,E)` to construct this explicitly versioned graph
identity. This deliberately binds both input files and the declared vertex order;
it is not a graph-isomorphism hash or a silently interchangeable upstream hash.

`parameters` must explicitly specify `walk_depth`, `walk_decay`, and
`repulsion_alpha`. The other named controls expand to the defaults below. Every
expanded parameter is returned and participates in cache identity.

| Parameter | Default / domain |
| --- | --- |
| walk_depth | required positive integer; fixture uses 10 |
| walk_decay | required, strictly between 0 and 1; fixture uses 0.1 |
| repulsion_alpha | required nonnegative finite real; fixture uses 0.2 |
| epochs | 60, positive integer |
| transition_epochs | 30, integer at least 2 |
| schedule_epsilon | 0.01, strictly between 0 and 1 |
| movement_tolerance | 1e-7, nonnegative finite |
| collision_distance | 1e-12, positive finite |
| max_pair_displacement | 1, positive finite |
| armijo | 1e-4, strictly between 0 and 1 |
| max_backtracks | 60, nonnegative integer |

METHOD.md defines the mathematics and stopping semantics. Changing locality
changes the objective; the objective values of different k are not directly
comparable quality scores.

## Result publication and resuming

Completed responses carry method/variant, graph and input hashes, dimension,
seed, all parameters, upstream commit, implementation commit and source hash,
Python/NumPy/platform identity, elapsed seconds, measured memory, termination,
objective history, quality diagnostics, warnings, and raw coordinate path/hash.
`completed` means a valid finite result was produced, not that an optimum was
proved. `epoch_budget`, `movement_tolerance`, and `floating_point_stagnation`
are distinct completion reasons. No display transform is applied. Floating-point
repeatability is checked within the recorded runtime; cross-platform bitwise
identity is not promised.

Each invocation works in its own `.attempt-<uuid>` directory. The numerical
worker writes coordinates there. After validation, the supervisor writes a
manifest plus its checksum receipt and atomically renames the whole directory
into `results/<cache_key>-<uuid>/`. It then atomically replaces the cache pointer
and response. These operations prevent partially written files being accepted;
they are not a multi-file transaction or a guarantee against filesystem or power
failure. A crash between publication steps can leave a valid unindexed result.
Such a directory is harmless and will not be discovered as a cache hit.

The cache key includes implementation commit, hashes of runtime source files and
requirements, runtime versions/platform, preparation version, graph ID and file
hashes, vertex order, dimension, expanded parameters, initial-coordinate checksum
(or canonical seeded initialization), seed, and variant. Cache reuse verifies
identity, completed status, manifest receipt/hash, coordinate checksum, exact ID
order/dimensionality, finite values, and result-root containment. Corrupt entries
are ignored and recalculated. Partial attempt directories never count as results.
These checks detect accidental damage; they do not authenticate files against a
hostile writer with access to the result directory.

Job limits and file locations do not alter numerical identity. Cache reads still
run under the invocation's limits and revalidate input checksums. On a cache hit,
`elapsed_seconds` and memory refer to lookup/validation; `source_run_*` retains
original numerical-run elapsed time and memory. Old cache entries are retained
when source commits change. No mid-optimization checkpoint resume is provided.

Failures omit coordinate path/hash claims. Status is `invalid_input`,
`unsupported`, `resource_limited`, `failed`, `cancelled`, or `busy`, with a reason
in `termination`. Failures before valid request preparation return only available
provenance, rather than inventing unknown graph or runtime diagnostics. The CLI's
response is authoritative for that invocation; an uncatchable SIGKILL cannot
write a new response, so consumers must also observe process exit. Do not infer
success from a response file left by an earlier invocation.

## Resource handling

Default and maximum limits are 600 wall seconds and 2,048 MiB. Supply lower limits
in `job_limits` as `wall_seconds` and `memory_mib`. Dense memory preflight is
`128 MiB + 128*n*n + 512*edge_count` bytes. It is a conservative admission estimate,
not a proof of actual peak usage. Only one heavy worker runs per CLI invocation;
callers must keep invocations serial for the current experimental workflow.

A supervisor samples summed supervisor/worker resident memory every 50 ms using
`ps`; shared pages may be counted twice. A post-exit sum of each process's peak
RSS conservatively detects short peaks between samples. Peak measurements exclude
unrelated jobs, include Python/NumPy overhead, and may include a small `ps` child
in the child high-water mark. Limits cover worker startup, input parsing,
preparation, optimization, diagnostics and cache validation; initial CLI parsing
and final publication are outside the numerical-job timer. `worker_elapsed_seconds`
includes worker preparation and diagnostics, not only optimizer steps.

Exceeding a limit terminates the owned process group (TERM, then KILL after
0.5 seconds if needed) and publishes a resource-limited failure. This is supervised
termination, not a kernel-enforced memory allocation ceiling: transient overshoot
is possible. Responses report observed maximum poll gap, memory overshoot and
time overshoot. A result exceeding the post-exit peak/time check is discarded.
SIGINT/SIGTERM cancellation removes the current partial attempt. A watchdog in
the worker exits if its supervisor disappears, including SIGKILL; abandoned
partial directories are never reused. Tests kill only their own child processes.

## Quality diagnostics

These are a basic subset of the shared project definitions dated 2026-09-24,
with formula ID `suitesparse-design-2026-09-24-basic-v1`. They are not upstream's
buggy diagnostic implementations and do not claim the full pilot evaluation.
All unordered within-component pairs are used, without sampling or exclusions.
For graph distances d and embedded distances r:

* Euclidean distance error: `sqrt(sum((s*r-d)^2)/sum(d^2))`,
  `s=sum(r*d)/sum(r^2)`.
* Relative-distance stress: `mean((t*r/d-1)^2)`,
  `t=sum(r/d)/sum((r/d)^2)`. Its independently fitted scale differs from s.
* Identity-scale edge error: `sqrt(mean((r_edge-1)^2))` for unit targets.
* Neighborhood error at hop radii 1 and 2: mean one minus Jaccard similarity
  between the complete graph hop ball and an equal-cardinality embedded nearest
  set. Self is excluded; exact embedded-distance ties use stable string IDs.

The scale fits belong to the diagnostics; raw coordinates remain unchanged.
Zero embedded pairs are counted. Completely collapsed or nonfinite-distance
layouts receive an unavailable status. Tests use straight/folded paths, explicit
scalar least squares, scale behavior and ties. Fixed-path error, rank diagnostics,
community scores and multi-component aggregation are not implemented here.
No alternative community partition or target is substituted.
