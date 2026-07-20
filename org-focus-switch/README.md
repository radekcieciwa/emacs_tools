# org-focus-switch

Reconstruct the order in which you clocked tasks and answer two questions:

1. **How often did I switch tasks?** — as a rate (switches per focused hour)
   plus the average uninterrupted focus-block length. Frequent switching vs.
   long focus periods.
2. **How did priority move when I switched?** — a directed, weighted graph of
   `FOCUS_PRIORITY` transitions (P0 → P1, P1 → P1, …), classified into
   escalations, de-escalations and lateral moves.

Org clocks tell you *how long* you spent on each task. This package tells you
*how you moved between them*.

## Commands

- `M-x org-focus-switch` — bound to `C-c t s` in Org buffers. Analyzes the
  subtree at point; with a prefix (`C-u C-c t s`), the whole buffer. Opens the
  read-only `*Org Task Switch*` report.
- `M-x org-focus-switch-export` — export the transition graph in a standard
  format (see [Export](#exporting-the-graph)). Also bound to `e` inside the
  report buffer.
- Inside the **org-focus dashboard** (`C-c f d`) the same analysis appears as a
  "Task Switching" section — see [Integration](#integration-with-org-focus).

## Example report

```
Task Switching
Clock events:        4
Task switches:       3  (1.0 /focused-h)
Focus blocks:        4  (avg 45m each)
Clocked time:        3h

Priority transitions (from -> to)
         ->P0   ->P1   ->P2 ->none
P0          1      ·      1      ·
P1          ·      ·      ·      ·
P2          1      ·      ·      ·
none        ·      ·      ·      ·

Edges (by frequency)
  P0   -> P0      1  lateral
  P2   -> P0      1  escalation
  P0   -> P2      1  de-escalation

Direction:           1 escalation(s), 1 de-escalation(s), 1 lateral
```

## How it works

- **Task identity** is the normalized heading (TODO state, priority cookie and
  tags stripped). The same recurring task logged across many days is *one*
  task; clocking it back-to-back is not a switch, but leaving and returning
  counts as two.
- **Leaf entries only**, `:private:` subtrees skipped, **closed clocks only** —
  matching the `org-focus` taxonomy.
- A **task switch** is a boundary between two consecutive (time-ordered) clock
  events with different tasks. Each switch contributes one edge
  `from-priority → to-priority`.
- **Direction** is by urgency rank (`org-focus-switch-priorities`, most urgent
  first; missing priority ranks last as `none`): moving to a more urgent level
  is an escalation, less urgent is a de-escalation, equal is lateral.

### Where priority comes from

Priority is resolved per clocked entry, in this order:

1. the **`FOCUS_PRIORITY` property**, **inherited** from ancestor headings when
   `org-focus-switch-priority-inherit` is non-nil (default) — so setting it once
   on a project/section heading covers every leaf beneath it;
2. failing that, Org's **native priority cookie** `[#A]`/`[#B]`/`[#C]`, mapped
   through `org-focus-switch-cookie-priority-alist` (default `A→P0, B→P1,
   C→P2`), when `org-focus-switch-use-priority-cookie` is non-nil (default).

Only when neither source yields a value does an event fall into the `none`
node. (If you ever see *only* `none → none` edges, none of your clocked entries
carry a priority in either form.)

## Exporting the graph

`M-x org-focus-switch-export` (or press `e` in the report) renders the
priority-transition graph — a **directed weighted graph**, nodes = priorities,
edge weight = switch count, plus a `direction` attribute — in a standard format:

| Format | Consumers |
|--------|-----------|
| **DOT** (Graphviz) | `dot -Tsvg`, most graph tools; edges coloured by direction, thickness = weight. |
| **Mermaid** | GitHub/Markdown, `mermaid.js`, many docs tools. |
| **GraphML** | Gephi, yEd, Cytoscape, NetworkX. |
| **CSV** | Spreadsheets, pandas; `from,to,weight,direction` edge list. |
| **JSON** | Anything; includes `nodes`, `edges`, and the headline `metrics`. |

The command prompts for the format, shows the result in
`*Org Task Switch Export*`, and offers to write it to a file with the right
extension. `org-focus-switch-to-format` is the pure `(DATA FORMAT) → string`
function behind it.

Example (Mermaid), paste into any Markdown that renders Mermaid:

```mermaid
flowchart LR
  P0 -->|3| P1
  P1 -->|2| P0
  P0 -->|1| P0
```

### Session gap

Set `org-focus-switch-session-gap-minutes` to a positive integer to treat long
idle gaps (overnight, lunch) as session breaks: they start a new focus block
but are not counted as switches. Default `nil` counts every boundary.

## Integration with org-focus

`org-focus` requires this package optionally and, when it is on the
`load-path`, renders the "Task Switching" section in its dashboard over the same
scope (subtree or global). No configuration is needed beyond putting both
directories on the `load-path`:

```elisp
(add-to-list 'load-path "/path/to/development/org-focus-switch")
(add-to-list 'load-path "/path/to/development/org-focus")
(require 'org-focus)      ; picks up org-focus-switch automatically
(org-focus-mode 1)
```

Everything is guarded by `fboundp`, so `org-focus` works unchanged if this
package is absent.

## Customization

| Variable | Default | Meaning |
|----------|---------|---------|
| `org-focus-switch-priorities` | `("P0" "P1" "P2")` | Priority levels, most urgent first. |
| `org-focus-switch-priority-property` | `"FOCUS_PRIORITY"` | Property holding the priority. |
| `org-focus-switch-priority-inherit` | `t` | Inherit the property from ancestors. |
| `org-focus-switch-use-priority-cookie` | `t` | Fall back to the `[#A]` cookie. |
| `org-focus-switch-cookie-priority-alist` | `((?A . "P0") (?B . "P1") (?C . "P2"))` | Cookie → label map. |
| `org-focus-switch-none-label` | `"none"` | Label for entries with no priority. |
| `org-focus-switch-exclude-tags` | `("private")` | Excluded (inherited) tags. |
| `org-focus-switch-session-gap-minutes` | `nil` | Idle gap that splits sessions. |
| `org-focus-switch-buffer-name` | `"*Org Task Switch*"` | Standalone report buffer. |
| `org-focus-switch-export-formats` | DOT/Mermaid/GraphML/CSV/JSON | Formats offered by export. |

## Tests

```sh
cd org-focus-switch
emacs --batch -l ert -l org-focus-switch.el -l org-focus-switch-tests.el \
      -f ert-run-tests-batch-and-exit
```

See [`SPEC.md`](SPEC.md) for the full data-model and API contract.
