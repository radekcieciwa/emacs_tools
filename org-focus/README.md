# org-focus

A small Org-mode package for tracking where your time goes, linting entry
metadata, and rendering a focus dashboard with **dual-scope** (subtree /
global) modes.

```
org-focus/
├── org-focus.el          ← the package (provide 'org-focus)
├── org-focus-tests.el    ← ERT unit tests
├── README.md             ← this file
└── experimental/         ← diagnostic + ad-hoc scripts, not part of the package
    └── TESTING.md
```

## What it does

- **Dashboard** (`C-c f d`) — sums every clock in the measured scope and breaks
  it down by domain / activity / intentionality, lists clocked entries (merged
  by heading, sorted by duration), warns on metadata/ratio issues, and ends with
  a compact **By Child** table decomposing each direct child subtree.
- **Lint** (`C-c f l`) — lists entries with missing or conflicting metadata.
- **Fix** (`C-c f f`) — interactively set the metadata axes on the current entry.

There is **no week filtering**. The dashboard measures whatever scope you point
it at — put the cursor on a `Week` heading, a `Day` heading, or a project, and
it sums the clocks beneath it. Only leaf (childless) entries are measured;
parent headings are treated as pure structure.

## Metadata taxonomy

Required for clocked or active entries:

| Axis      | Tags (one each)             |
|-----------|-----------------------------|
| domain    | `prod` `team` `org`         |
| activity  | `build` `help` `ops` `sync` |
| intent    | `plan` `unplan`             |

Optional:

- `invest` tag — marks learning/exploration work (tracked against a target ratio).
- `FOCUS_PRIORITY` property — `P0` / `P1` / `P2`. `P0` is the "urgent" level used
  for the active-count and staleness warnings. (This is a custom property, *not*
  Org's built-in `[#A]` priority.)

Entries tagged `private` are excluded from everything.

Example:

```org
* TODO Investigate rendering regression :prod:build:plan:
  :PROPERTIES:
  :EFFORT:   2:00
  :FOCUS_PRIORITY: P0
  :END:
  CLOCK: [2026-06-08 Mon 09:00]--[2026-06-08 Mon 11:30] =>  2:30
```

## Install

```elisp
(add-to-list 'load-path "/path/to/org-focus")
(require 'org-focus)
(org-focus-mode 1)
```

In this repo it is wired up through `lisp/org-focus-config.el`, which adds the
directory to `load-path` and `(require 'org-focus)`.

Optionally install the recommended tag shortcuts:

```elisp
(org-focus-install-tag-groups)
```

## Keybindings (when `org-focus-mode` is on)

| Key            | Command                        | Scope                      |
|----------------|--------------------------------|----------------------------|
| `C-c f d`      | `org-focus-dashboard`          | subtree at point           |
| `C-u C-c f d`  | `org-focus-dashboard`          | global (configured files)  |
| `C-c f l`      | `org-focus-lint`               | subtree at point           |
| `C-u C-c f l`  | `org-focus-lint`               | global                     |
| `C-c f f`      | `org-focus-fix-current-entry`  | current entry              |

Both `dashboard` and `lint` also have explicit `-subtree` / `-global` commands.

## Configuration

| Variable                            | Default             | Meaning                                       |
|-------------------------------------|---------------------|-----------------------------------------------|
| `org-focus-domain-tags`             | prod team org       | Allowed domain tags                           |
| `org-focus-activity-tags`           | build help ops sync | Allowed activity tags                         |
| `org-focus-intent-tags`             | plan unplan         | Allowed intent tags                           |
| `org-focus-invest-tag`              | "invest"            | Investment tag                                |
| `org-focus-priorities`              | P0 P1 P2            | Priority levels (first = urgent)              |
| `org-focus-exclude-tags`            | private             | Tags that exclude an entry                    |
| `org-focus-max-active-p0`           | 3                   | Warn if more active P0 tasks than this        |
| `org-focus-stale-p0-days`           | 3                   | Warn if a P0 task isn't clocked within N days |
| `org-focus-investment-target-ratio` | 0.15                | Warn if invest ratio is below this            |
| `org-focus-unplanned-warning-ratio` | 0.30                | Warn if unplanned ratio exceeds this          |
| `org-focus-sync-warning-ratio`      | 0.25                | Warn if sync ratio exceeds this               |
| `org-focus-help-warning-ratio`      | 0.30                | Warn if help ratio exceeds this               |
| `org-focus-files`                   | nil                 | Files to scan (nil → `org-agenda-files`)      |
| `org-focus-current-file-only`       | nil                 | Scan only the current buffer's file           |
| `org-focus-enforce-on-clock-in`     | nil                 | Auto-fix metadata on clock-in                 |
| `org-focus-prompt-for-invest`       | nil                 | Ask about invest on clock-in                  |

## Running the tests

From this directory:

```sh
emacs -Q --batch -L . -l org-focus-tests.el -f ert-run-tests-batch-and-exit
```

The package is intentionally non-blocking: it warns and offers fixes, but never
prevents editing.
