# org-task-report

Aggregate every org entry that shares a title into a single, dated,
time-summed report.

## Purpose

You log the same recurring task across many days as separate org headings
(same title, but different status/priority/tags and different clocks). This
package collects them and answers: *what did I do on this task, on which day,
and for how long?*

## Command

`M-x org-task-report`, bound to `C-c t r` in every Org buffer.

Run it with point anywhere inside an org entry. It reads that entry's title,
finds all matching entries, and opens the `*Org Task Report*` report buffer (an
Org-mode buffer).

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

The report is a plain **Org-mode buffer**. Rows are sorted by date
(ascending; clockless rows sink to the end); each date is an Org heading whose
text is a `file:` link back to the source entry.

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
