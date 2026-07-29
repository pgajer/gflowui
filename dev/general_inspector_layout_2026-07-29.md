# General Inspector Layout

Date: 2026-07-29

## Current Behavior

- `Compute & Open Basin Inspector` is the single Basins action.
- If the canonical Basin Complex is absent or stale, the action reconstructs
  it and opens the inspector.
- If the current construction identity already matches, the action opens the
  existing result without reconstruction.
- On wide windows, the graph is on the left and the General Inspector is on
  the right.
- The vertical separator can be dragged with a mouse or touch pointer. It also
  supports arrow keys, Shift+arrow for larger changes, Home/End, and
  double-click reset.
- Width is retained per project, graph set, and `k`.
- On narrower windows the layout stacks vertically and disables the horizontal
  drag handle.

## Extension Contract

`#gf_general_inspector` is the durable host. Its
`.gf-general-inspector-stack` child is a vertical flex container intended to
hold multiple visualization or diagnostic panels.

The Basin Inspector is currently the only child. Future panels should be added
as sibling Shiny outputs inside `.gf-general-inspector-stack`; they should not
introduce their own top-level split implementation.

The host is hidden when it contains no active inspector panel. Any future
panel that is responsible for making the host visible while initially hidden
must use `shiny::outputOptions(..., suspendWhenHidden = FALSE)`, as the Basin
Inspector does.

Splitter behavior and persistence are owned by:

- `inst/app/www/basin-inspector-state.js`
- `inst/app/www/styles.css`

Server-side width state and the General Inspector DOM host are owned by:

- `R/app_server.R`
