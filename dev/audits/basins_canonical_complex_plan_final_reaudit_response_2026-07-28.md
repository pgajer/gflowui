# Response to Canonical Basin Complex Plan Final Re-Audit

Date: 2026-07-28

Final re-audit:
`/Users/pgajer/current_projects/gflowui/dev/audits/basins_canonical_complex_plan_final_reaudit_2026-07-28.md`

Disposition: Accepted verdict recorded; F-01 and F-02 incorporated before
implementation

## F-01 — Accepted

The two new `create.basin.complex()` formals are appended after all eleven
existing positions, including `verbose`. A regression test will call the
complete legacy signature without argument names.

`vertex.id` accepts integer or character vectors and rejects factors, missing
values, duplicates, empty strings after encoding, and invalid character
encoding. Integer values use locale-independent decimal character encoding;
character values are stored in UTF-8.

Existing canonical tables retain internal integer vertex indices. Companion
external-ID columns and list columns expose the supplied IDs without changing
the meaning or type of existing fields.

## F-02 — Accepted

The plan metadata now labels `v0.2.0-1-g92a61c08-dirty` as Git-describe output.

## Implementation Status

The plan is accepted. Implementation begins after these documentation
corrections; all 22 acceptance gates remain implementation obligations.
