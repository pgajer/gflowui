# Consistent dCST colors in local views — 02_oct_2026

Local atlas fits now inherit the whole dataset's dCST annotations and palette.
Switching from a global embedding to a local fit therefore preserves sample
classification and color. The selected level remains active across views. These
are the global dCST classifications restricted to the region, not locally
recomputed dCSTs.

The Graphs panel provides **Color by: dCST** by default whenever both supported
levels are present. Relative-abundance colors and other existing sources remain
available through that selector. The dCST table still filters membership even
when another color source is selected. Its counts refer to the local graph.

Inheritance uses the parent view's declared metadata file and stable sample IDs;
it does not copy or rewrite the 610 imported layout assets. Metadata without a
unique sample-ID mapping is not aligned by position. The selected parent palette
is reused, including saved edits. Color changes made in a local view update the
same project palettes used globally. No project-name or local-path branch was
introduced.

## Verification

Tests cover reordered sample IDs, unknown/duplicate IDs, missing local metadata,
retention of numeric color sources, dCST defaults, global/local level continuity,
and palette editing in a local view followed by return to the global view.
Related atlas, dCST, manifest, and view-performance checks pass. Three browser
protocol regressions also pass when run with the bundled Node executable.

The actual Li and Lc 4,000-sample abundance-Euclidean Fermat power-2 layouts were
checked against the parent metadata. All 4,000 assignments at both levels match
in each region, and every displayed group has a parent palette color. Browser
checks cover default local dCST colors and switching to abundance coloring and
back. Targeted checks were used; no full CRAN check or geometry recomputation was
needed.

```r
testthat::test_local(
  filter = "atlas-colors|local-view-atlas|dcst|manifest-providers|view-performance",
  reporter = "summary"
)
```
