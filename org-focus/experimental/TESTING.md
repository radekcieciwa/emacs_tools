# Experimental scripts

These files are **not** part of the `org-focus` package. They are ad-hoc
diagnostics, exploratory scripts, and historical tests kept for reference while
new features are developed. They are not loaded by `org-focus.el` or by
`org-focus-config.el`.

> ⚠️ Some of these were written against an earlier "weekly" version of the
> package and reference functions that no longer exist (e.g.
> `org-focus--sum-clocks-in-range`). They are kept as historical artifacts and
> may not run against the current `org-focus.el`. The supported, passing test
> suite is `../org-focus-tests.el`.

## Contents

| File                              | What it is                                                        | Runs against current core? |
|-----------------------------------|-------------------------------------------------------------------|----------------------------|
| `diagnose-clocks.el`              | Interactive clock-entry diagnostic (`M-x org-focus-diagnose-clocks`) | Yes                     |
| `test-clock-fixtures.org`         | Sample Org file with assorted clock lines                         | n/a (data)                 |
| `test-clock-simple.el`            | Quick manual clock-sum checks                                     | No — uses removed API      |
| `org-focus-clock-parsing-tests.el`| ERT tests for the old week-range clock parser                     | No — uses removed API      |
| `test-comprehensive.el`           | Broad manual exercise of dashboard/lint                           | Partial                    |
| `test-dual-scope.el`              | Manual subtree-vs-global comparison                               | Partial                    |
| `test-parent-fix.el`              | Manual check that parent headings are skipped                     | Partial                    |
| `test-real-structure.el`          | Manual run over a realistic Week/Day tree                         | Partial                    |
| `test-fix.el`                     | Manual exercise of `org-focus-fix-current-entry`                  | Partial                    |
| `debug-fix.el`                    | Scratch debugging for the fixer                                   | Scratch                    |
| `debug-has-children.el`           | Scratch debugging for child-heading detection                     | Scratch                    |
| `debug-has-children-detailed.el`  | Verbose variant of the above                                      | Scratch                    |

## How to load the core first

Everything here assumes `org-focus` is on `load-path`. From this directory:

```sh
emacs -Q --batch -L .. -L . -l <script>.el
```

`-L ..` puts `org-focus.el` (the parent dir) on `load-path`; `-L .` adds this
directory so cross-references between experimental files resolve.

## Clock diagnostics

`diagnose-clocks.el` is the most useful piece here. Interactively:

```elisp
(add-to-list 'load-path "/path/to/org-focus")
(add-to-list 'load-path "/path/to/org-focus/experimental")
(require 'org-focus)
(require 'diagnose-clocks)
;; then, in an Org buffer:
M-x org-focus-diagnose-clocks
```

It walks clock lines in the current buffer and reports parsing problems
(malformed durations, open clocks, etc.).

## Running a historical ERT script

```sh
emacs -Q --batch -L .. -L . -l org-focus-clock-parsing-tests.el \
      -f ert-run-tests-batch-and-exit
```

Expect failures where the script references the removed week-range API — that is
known and the reason these live under `experimental/` rather than next to the
package. Port any check you still want into `../org-focus-tests.el`.
