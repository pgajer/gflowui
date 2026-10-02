# Local region navigation fix — 02_oct_2026

Selecting a Li 4,000-sample region after enabling **Include retired regions**
was reported to alternate between a membership preview and the parent graph,
then appear stuck. The original session eventually settled during reproduction;
a permanent infinite loop was not established.

Two sources of control replacement were identified. Region changes were part of
the key used to rebuild the entire workflow accordion. Also, a change in the
number of controls in any panel replaced the accordion. Navigating to a saved
local fit exposed a reset to Whole dataset when the Graphs controls changed.
Recreated region/view selects could send default or previous selections back to
Shiny while the plot was updating.

The fix keeps the accordion scoped to the project, updates changed panel bodies
independently, and updates region/view choices without recreating their inputs.
Region and view events are validated against the available choices. A hidden
retired region or a view from a different region is rejected. Hiding retired
regions returns an active retired-region selection to Whole dataset.

## Verification

Browser checks on the actual comb-V3V4-tx project covered enabling the retired
checkbox, selecting Li / 4000 samples [revision 1], the parent preview, filtering
to exactly 4,000 members, opening its Jensen–Shannon complete-Fermat p=2 saved
layout, and returning to the parent layout with 25,042 vertices. The imported Li
region is revision 1 but is not itself retired; an isolated server fixture covers
an actually retired region without changing the user's atlas.

Focused tests cover stable navigation output, retired visibility, stale view
and region events, return to parent, unchanged saved assets, and keeping the
accordion and other panels mounted when a panel gains or loses controls.
The related R checks pass; 19 unavailable legacy/integration checks were skipped.
The Node check was also skipped by R's runtime lookup, so all three JavaScript
regressions (graph selection, scene updates, and lazy edges) were run separately
with the bundled Node executable and passed.
No distances, embeddings, memberships, or saved annotations are recomputed.

Run the related R checks from this checkout with:

```r
testthat::test_local(
  filter = "local-view-atlas|atlas-|view-performance|app-constructs",
  reporter = "summary"
)
```
