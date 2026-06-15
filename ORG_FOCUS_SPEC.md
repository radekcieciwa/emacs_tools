# Org Focus Minor Mode Specification

## Overview
Org Focus is a minor mode for Org that enforces a metadata taxonomy on entries, renders a dual-scope focus dashboard, and lints entries for missing/conflicting tags.

The package is `org-focus.el` (feature `org-focus`), living in `org-focus/`. Diagnostic and exploratory scripts live under `org-focus/experimental/`.

**Scope, not weeks.** The dashboard does *not* filter by week. It sums every clock in the measured scope, which is chosen by cursor position:
- **Subtree scope**: the heading at point and its descendants.
- **Global scope**: all entries in the configured files.

Only leaf (childless) entries are measured; parent headings are pure structure.

## Metadata Taxonomy

Required for clocked/active entries:
- **domain** (one of): prod, team, org
- **activity** (one of): build, help, ops, sync
- **intentionality** (one of): plan, unplan

Optional:
- **invest** tag: marks learning/exploration work
- **priority** (FOCUS_PRIORITY property): one of P0, P1, P2

Note: Priority is stored in a custom FOCUS_PRIORITY property, not Org's built-in PRIORITY.

## Customizable Configuration

```elisp
org-focus-domain-tags        '("prod" "team" "org")
org-focus-activity-tags      '("build" "help" "ops" "sync")
org-focus-intent-tags        '("plan" "unplan")
org-focus-invest-tag         "invest"
org-focus-priorities         '("P0" "P1" "P2")    ; custom focus priority levels

org-focus-max-active-p0                3    ; warn if > N active P0 (urgent) tasks
org-focus-stale-p0-days                3    ; warn if P0 task not clocked in N days
org-focus-investment-target-ratio    0.15  ; warn if invest time ratio < N
org-focus-unplanned-warning-ratio    0.30  ; warn if unplan time ratio > N
org-focus-sync-warning-ratio         0.25  ; warn if sync time ratio > N
org-focus-help-warning-ratio         0.30  ; warn if help time ratio > N

org-focus-current-file-only          nil   ; if true, scan only current buffer's file
org-focus-enforce-on-clock-in        nil   ; if true, prompt to fix metadata on clock-in
org-focus-prompt-for-invest          nil   ; if true, ask for invest tag on clock-in
org-focus-files                      nil   ; files to scan (default: org-agenda-files)
org-focus-dashboard-buffer-name      "*Org Focus Dashboard*"
org-focus-lint-buffer-name           "*Org Focus Lint*"
```

## Keybindings (when mode enabled)

```
C-c f d    org-focus-dashboard          ; dashboard (subtree; C-u for global)
C-c f l    org-focus-lint               ; lint (subtree; C-u for global)
C-c f f    org-focus-fix-current-entry  ; fix metadata on current entry
```

## Core Features

### 1. Dashboard (`org-focus-dashboard`)

Aggregates every clock in the measured scope (no week filtering):

**Summary section**:
- Total clocked hours
- Hours with complete metadata (domain + activity + intent)
- Hours with invest tag

**Breakdown tables** (hours and %):
- Domain (prod, team, org)
- Activity (build, help, ops, sync)
- Intentionality (plan, unplan)

**Warnings**:
- Too many active P0 (urgent) tasks (> threshold)
- Stale P0 tasks (not clocked in N days)
- Metadata issues found
- Investment ratio below target
- Unplanned work exceeds threshold
- Sync/meeting time exceeds threshold
- Help/support time exceeds threshold

**Metadata issues section**:
- List entries with detected issues
- Show all issues per entry
- Clickable links to navigate to entry

### 2. Linting (`org-focus-lint`)

Scans configured files for entries with metadata issues:
- Display heading + all issues
- Clickable links to navigate to entry

### 3. Helper: Install Tag Groups

`org-focus-install-tag-groups`: Sets `org-tag-alist` with recommended taxonomy including shortcuts:
```
Domain:  p=prod, t=team, o=org
Activity: b=build, h=help, a=ops, m=sync
Intent:   l=plan, u=unplan
Other:    i=invest
```

## Data Structures

### Metadata per entry (collected during scans)

```elisp
(:minutes N
 :tags [list]
 :priority "P0"|"P1"|"P2"|nil
 :todo state|nil
 :heading string
 :marker marker
 :file path
 :params plist)
```

### Dashboard data (return value of `org-focus--collect-subtree-data` / `org-focus--collect-global-data`)

Both collectors funnel through `org-focus--collect` and `org-focus--aggregate`,
so the plist shape is identical regardless of scope:

```elisp
(:rows [list of metadata plists]        ; leaf entries with clock time > 0
 :lint [list]                           ; leaf entries with issues
 :total N                               ; total clocked minutes
 :known N                               ; minutes with complete metadata (domain+activity+intent)
 :invest N                              ; minutes tagged invest
 :active-a N                            ; count of active urgent (P0) tasks
 :stale-a [list]                        ; stale urgent tasks (:heading :marker :file :last-clock)
 :by-domain {hash: tag → minutes}
 :by-activity {hash: tag → minutes}
 :by-intent {hash: tag → minutes}
 :by-priority {hash: level → minutes})
```

### Lint issue entry

```elisp
(:heading string
 :issues [list of issue strings]
 :marker marker
 :file path)
```

## Core Algorithms

### Clock time collection
- For each leaf entry, sum `CLOCK: ... => H:MM` durations on that entry only
  (`org-focus--sum-clocks-for-entry`); child headings are not double-counted.
- No week filtering: every matching clock in the entry contributes.

### Week boundaries (helper only)
- `org-focus--week-start-end` returns Monday midnight → next Monday midnight.
- Retained as a building block for possible future week-scoped views; the
  current dashboard does not use it.

### Last clock detection
- Scan CLOCK lines in entry's subtree
- Extract end time from each line
- Return latest time (or now for open clocks)
- Used to detect stale P0 tasks

### Metadata completeness
- An entry has "known metadata" if it has: domain + activity + intent
- Entries can have invest tag separately; only counts for ratio calculations
- Priority is optional; does not block "known metadata" status

### Files scanned
- If `org-focus-current-file-only` is true: current buffer file only
- Otherwise, use `org-focus-files` if set
- Fallback: `org-agenda-files`
- Final fallback: current buffer file if no agenda files

## Metadata Validation Issues

Detected for clocked/active entries:
- Missing domain tag
- Multiple domain tags
- Missing activity tag
- Multiple activity tags
- Missing intentionality tag
- Multiple intentionality tags

## Mode Lifecycle

```elisp
(org-focus-mode 1)  ; enable
  → adds org-focus--ensure-on-clock-in to org-clock-in-hook

(org-focus-mode 0)  ; disable
  → removes hook
```

Hook behavior: on clock-in, if `org-focus-enforce-on-clock-in` is true and in Org buffer, call `org-focus-fix-current-entry`.

## UI / Display

All interactive buffers:
- Use `special-mode` (read-only)
- Sections: bold headers
- Values: optional faces (success/warning)
- Buttons: clickable text linking to source entries
  - Action: find-file, goto-char marker, org-fold-show-context, org-fold-show-entry

## Notes

- Mode is intentionally non-blocking: lints and warns but does not prevent editing
- Dashboard works best with consistent use of org-clock
- Taxonomy is opinionated but fully customizable
- Entry linting only applies to clocked or TODO entries
- Priority is stored in custom FOCUS_PRIORITY property; does not use Org's PRIORITY

---

## Interactive Metadata Fixing (implemented)

**Function**: `org-focus-fix-current-entry` (bound to `C-c f f`)
- Prompts for each metadata axis (domain, activity, intentionality) via single-key
  selection (`org-focus--choose`).
- When `org-focus-prompt-for-invest` is non-nil, also asks whether to add the invest tag.
- Removes all existing tags on an axis before adding the chosen one.
- Saves the buffer after applying fixes.

**Helper functions**:
- `org-focus--fix-axis(axis choices)`: drop conflicting tags on an axis, prompt for one.
- `org-focus--ensure-on-clock-in()`: hook that calls the fixer on clock-in when
  `org-focus-enforce-on-clock-in` is non-nil.
- `org-focus--get-priority()` / `org-focus--set-priority(priority)`: read/write the
  FOCUS_PRIORITY property.

**Related customization**:
- `org-focus-enforce-on-clock-in`: automatic (on clock-in) vs. manual (`C-c f f`).
- `org-focus-prompt-for-invest`: whether the invest tag is prompted.

## Possible future work

- Optional week/date-range scoping (building on `org-focus--week-start-end`).
- Promoting useful pieces of `org-focus/experimental/` (e.g. clock diagnostics)
  into the core package once stabilized.
