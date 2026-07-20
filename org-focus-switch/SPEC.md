# org-focus-switch Specification

## Overview

`org-focus-switch` reconstructs the chronological sequence of Org clock events
in a scope and extracts **task-switching behaviour**. Where `org-focus` answers
*how much time went where*, this package answers *how often you jumped between
tasks, and how the priority of your work moved when you did*.

It is a standalone package (feature `org-focus-switch`, file
`org-focus-switch.el`, in `org-focus-switch/`). It does not depend on
`org-focus`; instead `org-focus` optionally **consumes** it to render a compact
section in its dashboard.

## Two surfaces

The same analysis (`org-focus-switch-analyze`) drives two renderings:

| Element | Dedicated dashboard (`org-focus-switch`) | Embedded org-focus section (`org-focus-switch-render`) |
|---------|:--:|:--:|
| Switch-frequency summary | ✓ | ✓ |
| Per-day distribution | ✓ | ✓ |
| Direction tally (one line) | — (implied by group headers) | ✓ |
| Priority-transition matrix | ✓ | — |
| Direction-grouped edges | ✓ | — |
| Graph export | ✓ (only here) | — |

The dedicated dashboard (`org-focus-switch--render-dashboard-content`) is the
full view; the embedded section (`org-focus-switch-render`) is deliberately
trimmed to the headline numbers and per-day distribution, pointing to the
dashboard for the transition graph.

## The conclusions

### 1. Task-switch frequency

- **Task switches** — the number of boundaries between two consecutive clock
  events with different tasks.
- **Switches per focused hour** — `switches / total-clocked-hours`. A rate, not
  a raw count, so it is comparable across scopes of different size. High = a
  fragmented, context-switching period; low = long focus periods.
- **Focus blocks** — the number of uninterrupted stretches of the same task
  (`= switches + 1` when there is no session splitting). The **average block
  length** (`total-minutes / blocks`) is the natural inverse of the switch
  rate: "on average I focus N minutes before changing task".
- **Per-day distribution** (`:per-day`) — one row per day carrying that day's
  switch count and rate per focused hour, with a bar scaled to the busiest day.
  Each switch is attributed to the local calendar day of the task switched
  *to*; per-day counts sum to the overall switch count. Days are ascending.

### 2. Priority-transition graph

Every task switch is a directed edge from the priority you left to the priority
you moved to, read from the priority source (see [Priority
resolution](#priority-resolution)) (P0 → P1, P1 → P1, …). The edges form a
**directed weighted graph** presented two ways:

- an **adjacency matrix** (rows = from-priority, columns = to-priority, cell =
  count, `·` for zero);
- **three direction-grouped sections** — Lateral, Escalation and De-escalation
  — each headed by its total switch count, listing its edges by frequency.
  (This replaces the earlier flat "Edges (by frequency)" list.)

Each edge is classified relative to urgency rank (position in
`org-focus-switch-priorities`, most urgent first; unknown/absent priorities rank
last as `none`):

- **escalation** — moved toward a *more* urgent level (e.g. P2 → P0);
- **de-escalation** — moved toward a *less* urgent level (e.g. P0 → P1);
- **lateral** — same urgency (e.g. P1 → P1).

## Task identity

Two clock events belong to the same task when their **normalized headings**
match. `org-get-heading` strips the TODO/DONE state, the `[#A]` priority cookie
and tags; internal whitespace is collapsed and the title trimmed. Consequently:

- The same recurring task logged on many days as separate headings (the
  `org-task-report` pattern) is **one** task — clocking it back-to-back is not a
  switch.
- Leaving a task and returning to it later counts as **two** switches (out, and
  back).

## Priority resolution

Each clocked entry's priority is resolved by `org-focus-switch--priority-at-point`
in this order:

1. the `org-focus-switch-priority-property` property (default `FOCUS_PRIORITY`),
   read **with inheritance** when `org-focus-switch-priority-inherit` is non-nil
   (default) — so a priority set on an ancestor project/section heading applies
   to every leaf beneath it;
2. Org's native priority cookie (`[#A]`), mapped via
   `org-focus-switch-cookie-priority-alist` (default `?A→"P0" ?B→"P1" ?C→"P2"`),
   when `org-focus-switch-use-priority-cookie` is non-nil (default).

Only when neither source yields a value does the event fall into the
`org-focus-switch-none-label` node. Both fallbacks default on so that
priority-transition edges are detected whether priority is stored as the custom
property (on the entry or an ancestor) or as the built-in cookie; a graph of
only `none → none` edges therefore means no clocked entry carries a priority in
either form.

## Scope and eligibility

- **Leaf entries only.** Entries with child headings are pure structure and are
  skipped, so a parent's clocks are never double-counted with its children's.
- **Excluded subtrees.** Entries carrying any tag in
  `org-focus-switch-exclude-tags` (default `("private")`, inherited) are skipped
  entirely.
- **Closed clocks only.** A `CLOCK:` line participates only when it carries a
  `=> H:MM` duration. Open (running) clocks are ignored.
- The event **end time is derived** as `start + duration`, so only the leading
  timestamp is parsed and the trailing stamp's format is irrelevant.

## Session gap (optional)

`org-focus-switch-session-gap-minutes` (default `nil`): when set to a positive
number, a boundary whose idle gap exceeds it is treated as a **session break** —
it does *not* count as a task switch and produces *no* priority edge, but it
*does* start a new focus block. This prevents overnight/lunch gaps from
inflating the switch rate. With `nil`, every differently-titled consecutive pair
counts as a switch regardless of gap.

## Public API

```elisp
;; Pure analysis over a list of event plists — testable without buffers.
(org-focus-switch-analyze EVENTS) => DATA-PLIST

;; Scan a scope into events (via a MAP-FN callback) and analyze.
(org-focus-switch-collect MAP-FN) => DATA-PLIST

;; Insert the compact embedded section for DATA (summary + direction tally +
;; per-day distribution) at point in any text buffer.  Used by org-focus.
(org-focus-switch-render DATA)

;; Insert the full dashboard body for DATA (both conclusions + grouped edges).
(org-focus-switch--render-dashboard-content DATA)

;; Interactive dashboard (subtree; C-u for whole buffer).
(org-focus-switch &optional ARG)

;; Serialize the transition graph to a standard format (pure).
(org-focus-switch-to-format DATA FORMAT) => STRING

;; Interactive export — only inside the dashboard; acts on its buffer-local data.
(org-focus-switch-export)
```

`MAP-FN` receives a per-entry callback and maps it over the desired scope,
mirroring the scope callbacks used inside `org-focus` (e.g.
`(lambda (fn) (org-with-wide-buffer (org-map-entries fn nil 'tree)))`).

### Event plist (input to `org-focus-switch-analyze`)

```elisp
(:start FLOAT       ; seconds since epoch (clock start)
 :end   FLOAT       ; seconds since epoch (start + duration)
 :task  STRING      ; normalized heading = task identity
 :priority STRING|nil)
```

### Data plist (return of analyze / collect)

```elisp
(:events N               ; count of clock events
 :total-minutes FLOAT    ; total clocked minutes
 :switches N             ; task switches
 :blocks N               ; uninterrupted focus blocks
 :switches-per-hour FLOAT
 :avg-block-minutes FLOAT
 :edges HASH             ; (FROM . TO) label pair -> count
 :escalations N
 :de-escalations N
 :laterals N
 :per-day LIST           ; per-day distribution, date-ascending; each element
                         ;   (:date "YYYY-MM-DD" :switches N :minutes FLOAT
                         ;    :switches-per-hour FLOAT)
 :order LIST)            ; priority labels in rank order, none label last
```

## Graph export

The priority-transition graph is a **directed weighted multigraph**: nodes are
priority labels, each edge `FROM -> TO` carries the switch count as its weight
plus a `direction` attribute (escalation / de-escalation / lateral).

`org-focus-switch-to-format' (pure, `(DATA FORMAT) -> STRING') serializes it to
one of:

| FORMAT | Output | Notes |
|--------|--------|-------|
| `dot` | Graphviz DOT | `rankdir=LR`; edge `penwidth` = weight; `color` by direction (escalation firebrick, de-escalation seagreen, lateral gray50). |
| `mermaid` | Mermaid `flowchart LR` | `FROM -->|weight| TO`. |
| `graphml` | GraphML (XML) | `weight` (int) and `direction` (string) edge keys; labels XML-escaped. |
| `csv` | CSV edge list | Header `from,to,weight,direction`. |
| `json` | JSON | `{directed, nodes[], edges[{from,to,weight,direction}], metrics{…}}`; pretty-printed. |

Node set = the labels appearing in any edge, ordered by `:order` (rank order)
with unlisted labels last (`org-focus-switch--graph-nodes`); edges are emitted
most-frequent-first (`org-focus-switch--graph-edges`).

`org-focus-switch-export` **works only inside the dashboard**. The dashboard
stashes its analysis data in the buffer-local `org-focus-switch--dashboard-data`
(set by `org-focus-switch--render-dashboard`); export reads that variable and
errors with a hint elsewhere. It renders into
`org-focus-switch-export-buffer-name` and offers to write a file with a
format-appropriate extension (`dot`/`mmd`/`graphml`/`csv`/`json`). In the
dashboard, `e` (and `g`) are bound to it.

## Customization

```elisp
org-focus-switch-priorities            '("P0" "P1" "P2")  ; rank order, urgent first
org-focus-switch-priority-property     "FOCUS_PRIORITY"
org-focus-switch-priority-inherit      t                  ; inherit property from ancestors
org-focus-switch-use-priority-cookie   t                  ; fall back to [#A] cookie
org-focus-switch-cookie-priority-alist '((?A . "P0") (?B . "P1") (?C . "P2"))
org-focus-switch-none-label            "none"
org-focus-switch-exclude-tags          '("private")
org-focus-switch-session-gap-minutes   nil                ; or a positive integer
org-focus-switch-buffer-name           "*Org Focus Switch Dashboard*"
org-focus-switch-export-buffer-name    "*Org Task Switch Export*"
org-focus-switch-export-formats        '(("dot" . dot) ("mermaid" . mermaid) ...)
```

## Consumption by org-focus

`org-focus` requires this package with `(require 'org-focus-switch nil t)` and,
when present:

- attaches `:switch` data to its dashboard data plist in
  `org-focus--collect-subtree-data` / `org-focus--collect-global-data` (over the
  same subtree / global scope);
- renders the compact "Task Switching" section in `org-focus--render-dashboard`,
  after the Domain/Activity/Intentionality tables, via `org-focus-switch-render`
  (summary + direction tally + per-day distribution only — no matrix, no
  grouped edges, no export).

Both integration points are guarded by `fboundp`, so `org-focus` loads and runs
unchanged when the package is not on the `load-path`.

## Keybinding

`C-c t s` in Org buffers → `org-focus-switch` (dashboard; `C-u` for the whole
buffer). Set up on `org` load via `with-eval-after-load`. Inside the dashboard,
`e`/`g` → `org-focus-switch-export`.

## Tests

ERT tests in `org-focus-switch-tests.el`. Run in batch:

```sh
cd org-focus-switch
emacs --batch -l ert -l org-focus-switch.el -l org-focus-switch-tests.el \
      -f ert-run-tests-batch-and-exit
```

## Future extensions

- Multi-file standalone scope (via `org-agenda-files`), mirroring the future
  work planned for `org-task-report`.
- Time-of-day switch-rate decomposition (per-hour, complementing per-day).
- Distinguish "return" switches (A → B → A) from novel switches.
- Weight edges by time as well as count.
