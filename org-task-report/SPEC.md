# org-task-report

Aggregate every org entry that shares a title into a single, dated,
time-summed report.

## Purpose

You log the same recurring task across many days as separate org headings
(same title, but different status/priority/tags and different clocks). This
package collects them and answers: *what did I do on this task, on which day,
and for how long?*

## Commands

`M-x org-task-report`, bound to `C-c t r` in every Org buffer.

Run it with point anywhere inside an org entry. It reads that entry's title,
finds all matching entries, and opens the `*Org Task Report*` report buffer.

`M-x org-task-report-toggle-presentation`, bound to `C-c t t` in every Org
buffer and to `t` inside a dashboard-presented report. It flips
`org-task-report-presentation` and re-renders the open report from its stored
rows — the source buffer is not rescanned, so the report keeps working even if
the source is no longer reachable.

## Matching rules

- Two entries match when their **titles are equal** after normalization.
- Normalization ignores TODO/DONE **status**, the **priority** cookie
  (`[#A]`), and **tags** — these are stripped by `org-get-heading`. Internal
  whitespace is collapsed and the title trimmed.
- **Scope is a single file**: only the buffer holding the entry under the
  cursor is scanned. (Extending to `org-agenda-files` later means swapping
  `org-task-report--map-entries`.)

## Per-entry data

For each matched entry the report captures:

| Field     | Source                                                              |
|-----------|---------------------------------------------------------------------|
| date      | Earliest `CLOCK:` start date in the entry (`nil` if no clocks).     |
| minutes   | Sum of all closed `CLOCK:` durations in the entry.                  |
| note      | The body text under the heading.                                    |

- **Clocks and notes exclude child subtrees** — only the entry's own body is
  read. Nested headings are analyzed on their own if their titles match.
- The **note** strips `PROPERTIES`/`LOGBOOK` drawers, `CLOCK:` lines, and
  `SCHEDULED:`/`DEADLINE:`/`CLOSED:` planning lines, leaving only written
  content.
- Open (unclosed) clocks contribute their date but **0 minutes**.

## Report layout

Rows are sorted by date (ascending; clockless rows sink to the end) in both
presentations. `org-task-report-presentation` picks the rendering; both are
built from the same collected rows, so switching changes only the display.

### `org` (default)

A plain **Org-mode buffer**. Each date is an Org heading whose text is a
`file:` link back to the source entry.

```org
#+TITLE: Task Title

3 entries · Total: 5h 30m

* [[file:/path/to/tasks.org::12][2026-05-13]] · 2h
First day of work.

* [[file:/path/to/tasks.org::40][2026-05-14]] · 1h 30m
Second day of work.

* [[file:/path/to/tasks.org::58][2026-05-16]] · 2h
Wrapped up.
```

The link carries a **line number** (`::12`), so it lands on the correct entry
even when several share a title. The line number is a snapshot taken when the
report is built; editing the source file afterwards may drift it. Visit a link
with `RET` or `C-c C-o`. When the source buffer has no file (e.g. a scratch
buffer), the date is rendered as plain text with no link.

### `dashboard`

A **read-only rendered buffer** (`special-mode`) in the style of the org-focus
dashboard: a summary block, then one dated block per entry. The date is a
clickable button carrying the same file/line target as the Org link (plain
text when the source has no file), followed by the duration and a bar scaled
to the longest entry in the report.

```
Org Task Report (Task Title)

Summary
Entries:           3
Days:              2
Total clocked:     3h 30m
Average per day:   1h 45m

Entries (3)

2026-05-13       2h       ████████████████
  First day of work.

2026-05-14       1h 30m   ████████████
  Second day of work.

(no clock info)  0m
  (no notes)
```

## Edge cases (current behaviour)

- **Two entries, same title, same date** → **not merged**. They appear as two
  separate dated headings. (Explicitly out of scope for now.)
- Entry with **no clocks** → shown under `(no clock info)` with `0m`.
- Entry with **no note** → shown as `/(no notes)/`.

## Tests

ERT tests in `org-task-report-tests.el`. Run in batch:

```sh
cd org-task-report
emacs --batch -l ert -l org-task-report.el -l org-task-report-tests.el \
      -f ert-run-tests-batch-and-exit
```

## Future extensions

- Multi-file scope via `org-agenda-files`.
- Merge same-title/same-date entries with combined notes.
- Fuzzy title matching (e.g. ignore trailing Jira keys).
