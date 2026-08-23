# Repository Instructions

## Scope
- This repository is the development home for the `gflowui` interface and its
  source, tests, documentation, and durable project-facing design notes.
- Keep generated run outputs, local scratch files, and transient render
  products out of source commits unless the task explicitly promotes them as
  durable artifacts.

## Private Agent Work Products
- Store internal agent-only work products under
  `~/.codex/private/gflowui/`, not in the repository. This includes internal
  audits, agent-to-agent handoffs, intermediate rewrites, working copies of
  reviewer reports used for agent tasks, historical prompts, and generated
  review diffs that are not intended as package, manuscript, reproducibility,
  or submission artifacts.
- Organize private material first by workstream and then, when useful, by
  artifact type. Use clearly named workstream directories with subdirectories
  such as `audits/`, `handoffs/`, `drafts/`, `prompts/`, and `diffs/`.
- Maintain a `README.md` in each workstream directory identifying every file's
  origin, former repository location, purpose, and possible future
  disposition.
- Keep formal and publication-facing assets in the repository. Do not move
  source code, tests, package documentation, manuscript source, bibliography,
  figures, rendering tools, citation-verification evidence, reproducibility
  inputs or scripts, checksums, provenance records, or formal submission files
  into the private tree.
- Treat draft responses, internal referee simulations, and agent working copies
  of received reports as private. If a response-to-reviewers document becomes
  part of an actual submission, copy its finalized version into the appropriate
  repository submission bundle.
- Do not make repository builds, tests, manuscript renders, or validation
  workflows depend on files under the private tree.
- When retiring a tracked internal file from the repository, preserve its
  existing Git history through the normal repository deletion and record its
  private destination in the workstream README.
- The private directory is not a credentials store. Never place passwords,
  access tokens, private keys, or other authentication secrets there.
