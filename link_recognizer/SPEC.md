# Link Recognizer

`link_recognizer.el` turns a raw URL from the clipboard into a prettified
Org-mode link with a short, human-readable description. Each URL shape has its
own *recognizer* function; `org-insert-link-interceptor` walks them in order and
uses the first one that matches.

## How a recognizer works

A recognizer is a function that takes a single string (the URL) and returns
either:

- `nil` — the URL does not match this recognizer, or
- a cons cell `(URL . DESCRIPTION)` — where `URL` is the original link and
  `DESCRIPTION` is the short label to show.

Example — the GitHub Enterprise PR recognizer maps

```
https://github.bumble.dev/ios/bumble/pull/22891/files
```

to

```
("https://github.bumble.dev/ios/bumble/pull/22891/files" . "ghe/ios/bumble/PR#22891")
```

where `ghe` and `PR#` are hardcoded, and `ios` (org), `bumble` (repo) and
`22891` (PR index) are extracted from the URL.

## GitHub links

GitHub is handled by one recognizer, `github-link-recognizer`, covering both
`github.com` (prefix `gh`) and `github.bumble.dev` (prefix `ghe`) — see
`github-host-prefixes` to add another host. It splits the URL with
`github-url-parts` into `(PREFIX OWNER REPO PATH ANCHOR)` and labels `PATH` via
`github-path-label`. Descriptions always start with `PREFIX/OWNER/REPO`:

| URL after `OWNER/REPO/`                        | Description suffix          |
| ---------------------------------------------- | --------------------------- |
| `pull/12345` (also `/files`, `/commits`, `/checks`, `#discussion_r…`) | `/PR#12345` |
| `actions/runs/161234`                          | `/run#161234`               |
| `actions/runs/161234/job/455667`               | `/run#161234 job#455667`    |
| `actions/runs/161234/attempts/2`               | `/run#161234 (attempt 2)`   |
| `actions/jobs/455667`                          | `/job#455667`               |
| `actions/workflows/ci.yml`                     | `/workflow ci.yml`          |
| `actions`                                      | `/actions`                  |
| `issues/777`                                   | `/issue#777`                |
| `discussions/42`                               | `/discussion#42`            |
| `commit/1a2b3c4d5e6f…`                         | `/commit 1a2b3c4`           |
| `compare/main...feature-branch`                | `/compare main...feature-branch` |
| `releases/tag/swift-6.0-RELEASE`               | `/release swift-6.0-RELEASE` |
| `blob/main/Sources/Core/Engine.swift#L10-L24`  | `/Engine.swift:10-24`       |
| `tree/release/6.0`                             | `/tree release/6.0`         |
| `wiki/Getting-Started`                         | `/wiki Getting Started`     |
| `projects/3` / `milestone/3`                   | `/project#3` / `/milestone#3` |
| anything else, or repository root              | *(none)* — plain `gh/apple/swift` |

Query strings and anchors are stripped before matching, so `?diff=split`,
`?check_run_id=…` and `#step:5:120` do not change the description.

Three sibling recognizers handle the rest of GitHub:

- `github-org-link-recognizer` — `/orgs/apple/projects/7` → `gh/apple/project#7`
- `github-owner-link-recognizer` — `/apple` → `gh/apple`
- `github-gist-link-recognizer` — `gist.github.com/radek/1a2b3c4d…` → `gist/radek/1a2b3c4`

`github-enterprise-pull-link-recognizer` still exists as a GHE-pull-request-only
entry point, but it is not registered in `org-link-recognizers` —
`github-link-recognizer` returns the same description for those URLs.

To add a new GitHub shape, add a `cond` clause to `github-path-label` (before
the broader ones) instead of writing a new top-level recognizer.

## How to add a new entry

1. **Write the recognizer function** in `link_recognizer.el`. Match the URL with
   `string-match`, pull out the parts you need with `match-string`, and return a
   cons of the original string and the label:

   ```elisp
   (defun my-link-recognizer (string)
     (if (string-match "https?://example\\.com/\\([0-9]+\\)" string)
         (if-let ((id (match-string 1 string)))
             (cons string (concat "Example-" id)))))
   ```

   Notes:
   - Wrap capture groups in `\\( ... \\)`.
   - Escape literal dots as `\\.` so `.` is not treated as "any char".
   - Return `nil` (falls through automatically) when nothing matches.

2. **Register it** by adding the function symbol to the `org-link-recognizers`
   list. Order matters — the first match wins, so put more specific recognizers
   before more general ones (e.g. the GitHub Enterprise recognizer is listed
   before the generic GitHub ones).

3. **Add test cases** in `link_recognizer_tests.el` (see below).

## How to run the tests

The tests live in `link_recognizer_tests.el` and use `assert` (an alias for
`cl-assert`). They `load-file` the implementation and assert on each
recognizer's return value.

Run them from this directory in batch mode:

```sh
cd link_recognizer
emacs --batch --eval "(progn (require 'cl) (load-file \"link_recognizer_tests.el\") (message \"ALL TESTS PASSED\"))"
```

- If every assertion passes, the run prints `ALL TESTS PASSED` and exits 0.
- A failing assertion raises `cl-assertion-failed`, prints the failing form, and
  exits non-zero.

You can also evaluate individual test forms interactively in Emacs with
`C-M-x` while editing `link_recognizer_tests.el`.

### Adding a test case

Append a `let` block that binds the recognizer's output and asserts on it. Cover
both a matching URL and a non-matching one:

```elisp
;; my-link-recognizer
(let (
      (t1 (my-link-recognizer "https://example.com/42"))
      (t2 (my-link-recognizer "https://other.com/42"))
      )
      (assert (equal '("https://example.com/42" . "Example-42") t1))
      (assert (equal nil t2))
      )
```

It is also worth asserting the recognizer is reachable through the dispatcher:

```elisp
(let ((t1 (insert-link-interceptor-for-value "https://example.com/42")))
      (assert (equal '("https://example.com/42" . "Example-42") t1)))
```
