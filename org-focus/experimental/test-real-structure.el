#!/usr/bin/env emacs --script
;; Test with structure similar to user's real file

(require 'org)
(require 'org-clock)

(require (quote org-focus))

(message "\n=== Testing Real-World Structure ===\n")

(let ((test-file "/tmp/test-real.org"))
  (with-temp-file test-file
    (insert "* Thursday <2026-05-14 Thu>
** DONE P1 Fill in Snippets :team:ops:plan:
:LOGBOOK:
CLOCK: [2026-05-14 Thu 06:29]--[2026-05-14 Thu 06:45] =>  0:16
:END:
** Break :private:
:LOGBOOK:
CLOCK: [2026-05-14 Thu 09:22]--[2026-05-14 Thu 09:33] =>  0:11
CLOCK: [2026-05-14 Thu 07:32]--[2026-05-14 Thu 07:41] =>  0:09
:END:
** Life :private:
:LOGBOOK:
CLOCK: [2026-05-14 Thu 09:59]--[2026-05-14 Thu 11:43] =>  1:44
:END:
** DONE Review Stuff :team:ops:unplan:
:LOGBOOK:
CLOCK: [2026-05-14 Thu 05:10]--[2026-05-14 Thu 05:27] =>  0:17
:END:
** ITERATED_WIP [#A] Important Task :prod:build:plan:
:LOGBOOK:
CLOCK: [2026-05-14 Thu 13:54]--[2026-05-14 Thu 13:59] =>  0:05
CLOCK: [2026-05-14 Thu 05:34]--[2026-05-14 Thu 05:43] =>  0:09
:END:

* Friday <2026-05-15 Fri>
** TODO Fill Snippets :team:ops:plan:
** Break :private:
:LOGBOOK:
CLOCK: [2026-05-15 Fri 05:53]--[2026-05-15 Fri 06:02] =>  0:09
:END:
"))

  (with-current-buffer (find-file-noselect test-file t)
    (setq org-agenda-files (list test-file))

    ;; Position cursor on Thursday
    (org-back-to-heading t)
    (let ((heading (org-focus--heading)))
      (message "Measuring: %s\n" heading)

      (let ((data (org-focus--collect-subtree-data)))
        (let ((total (plist-get data :total))
              (rows (plist-get data :rows))
              (lint (plist-get data :lint)))

          (message "Results:")
          (message "  Total clocked: %.2fh" (/ total 60.0))
          (message "  Clocked entries: %d" (length rows))
          (message "  Lint issues: %d" (length lint))

          (message "\nEntries found:")
          (dolist (row rows)
            (let ((entry-heading (plist-get row :heading))
                  (minutes (plist-get row :minutes)))
              (message "    • %s: %.2fh" entry-heading (/ minutes 60.0))))

          (if (> (length lint) 0)
              (progn
                (message "\nLint issues found:")
                (dolist (issue lint)
                  (let ((issue-heading (plist-get issue :heading)))
                    (message "    • %s" issue-heading))))
            (message "\nNo lint issues found."))

          ;; Verify no parent in results
          (let ((has-parent nil))
            (dolist (row rows)
              (when (string-match "Thursday\\|Friday" (plist-get row :heading))
                (setq has-parent t)))
            (if has-parent
                (message "\n✗ FAIL: Parent headings found in results!")
              (message "\n✓ PASS: No parent headings in results")))

          ;; Verify no private in results
          (let ((has-private nil))
            (dolist (row rows)
              (when (string-match "Break\\|Life" (plist-get row :heading))
                (setq has-private t)))
            (if has-private
                (message "✗ FAIL: Private entries found in results!")
              (message "✓ PASS: Private entries excluded")))

          ;; Expected:
          ;; - P1 Fill in: 0:16
          ;; - Review Stuff: 0:17
          ;; - Important Task: 0:05 + 0:09 = 0:14
          ;; Total: 0:47 (without private break:1:20 and life:1:44)
          (let ((expected (+ 16 17 14)))  ; minutes
            (if (<= (abs (- total expected)) 5)
                (message "✓ PASS: Total time correct (expected ~%d min, got %d min)" expected total)
              (message "✗ FAIL: Total time wrong (expected ~%d min, got %d min)" expected total))))))

    (kill-buffer))

  (delete-file test-file))

(message "\n=== Done ===\n")
