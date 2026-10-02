# Li–Lc arm snapshot checkbox: redraw correction

Checking and unchecking a saved arm snapshot could rebuild the full graph display
even when the same arm was already visible in the working set. In comb-V3V4-tx,
the saved Li–Lc corridor contains 7,086 vertices, on a 25,042-vertex graph. Showing
both sources added duplicate corridor traces; deselecting the snapshot removed
that duplicate and triggered another full plot update.

The checkbox also invalidated the combined sidebar through the arm-panel state.
Its saved-dataset table now updates independently. Identical visible arm records
are combined for display, and the renderer is notified only when the displayed
arms actually change. Saved sets remain separate, including their provenance.
A snapshot still shows and hides normally when working arms are hidden. Distinct
geometry and preview styling remain distinct.

A regression test failed on the old implementation: the checked snapshot produced
two displayed copies, and four checkbox changes triggered four display
invalidations. After the correction there is one copy and no display invalidation
for those redundant toggles. The test also checks ordinary snapshot visibility,
unchanged sidebar output, and unchanged snapshot-file checksums. Related arm,
endpoint-sharing and endpoint-layout tests pass.

In the live comb viewer, repeated selection and deselection of the original
snapshot retained the same Plotly scene and produced no browser errors. Checksums
confirmed that the working file, candidate and snapshot were unchanged.

The original reported browser crash was not reproduced in the fresh verification
session. The R server was still alive and its log contained no fatal exception;
the reported older browser tab did not respond to browser-control requests.
The correction removes a demonstrated redundant redraw path and duplicate traces,
but does not establish the exact low-level cause of that earlier tab failure.

The original working file, candidate and snapshot are retained. No arm geometry,
Fermat distance or embedding was recomputed.
