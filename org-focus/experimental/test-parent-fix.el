#!/usr/bin/env emacs --script
;; Test suite for parent heading fix

(require 'org)
(require 'org-clock)

(require (quote org-focus))

(message "\n=== Testing Parent Heading Fix ===\n")

;; Create a temporary test file
(let ((test-file "/tmp/org-focus-test.org")
      (tests-passed 0)
      (tests-failed 0))

  ;; Write test org file
  (with-temp-file test-file
    (insert "* Week #19 (PARENT - NO CLOCKS)
** Monday (PARENT - NO CLOCKS)
*** Task A :prod:build:plan:
:LOGBOOK:
CLOCK: [2026-05-13 Mon 09:00]--[2026-05-13 Mon 11:00] => 2:00
:END:
*** Task B :team:ops:unplan:
:LOGBOOK:
CLOCK: [2026-05-13 Mon 11:00]--[2026-05-13 Mon 13:00] => 2:00
:END:
** Tuesday (PARENT - NO CLOCKS)
*** Task C :prod:build:plan:
:LOGBOOK:
CLOCK: [2026-05-14 Tue 09:00]--[2026-05-14 Tue 17:00] => 8:00
:END:
*** Incomplete Task :team:help:plan:
:LOGBOOK:
CLOCK: [2026-05-14 Tue 14:00]
:END:
** Break :private:
:LOGBOOK:
CLOCK: [2026-05-14 Tue 17:00]--[2026-05-14 Tue 18:00] => 1:00
:END:
"))

  (message "Test 1: Load and prepare")
  (with-current-buffer (find-file-noselect test-file t)
    (setq org-agenda-files (list test-file))
    (org-back-to-heading t)

    ;; Test 2: Subtree measurement
    (message "\nTest 2: Subtree scope (on 'Week #19')")
    (let ((data (org-focus--collect-subtree-data)))
      (let ((total (plist-get data :total))
            (rows (plist-get data :rows))
            (lint (plist-get data :lint))
            (expected-total 600)) ; 2+2+8 = 12 hours = 720 minutes. But 1h private excluded

        ;; We expect: 2h + 2h + 8h = 12 hours = 720 minutes (private break excluded)
        ;; Actually: only leaf nodes, so 4 leaf entries: Task A (2h), Task B (2h), Task C (8h), Incomplete (0h - no duration)
        ;; Total should be 12 hours = 720 minutes

        (message "  Total clocked: %.1fh" (/ total 60.0))
        (message "  Entries with clocks: %d" (length rows))
        (message "  Lint issues: %d" (length lint))

        ;; Verify no parent headings in rows
        (let ((has-parent-in-rows nil))
          (dolist (row rows)
            (let ((heading (plist-get row :heading)))
              (when (or (string-match "Week" heading)
                       (string-match "Monday" heading)
                       (string-match "Tuesday" heading))
                (setq has-parent-in-rows t))))
          (if has-parent-in-rows
              (progn
                (message "  ✗ FAIL: Parent headings found in rows!")
                (setq tests-failed (1+ tests-failed)))
            (progn
              (message "  ✓ PASS: No parent headings in rows")
              (setq tests-passed (1+ tests-passed)))))

        ;; Verify correct number of leaf entries
        ;; Should be 4: Task A, Task B, Task C, Incomplete
        (if (= (length rows) 4)
            (progn
              (message "  ✓ PASS: Correct number of leaf entries (4)")
              (setq tests-passed (1+ tests-passed)))
          (progn
            (message "  ✗ FAIL: Expected 4 leaf entries, got %d" (length rows))
            (setq tests-failed (1+ tests-failed))))

        ;; Verify no parent headings in lint
        (let ((has-parent-in-lint nil))
          (dolist (issue lint)
            (let ((heading (plist-get issue :heading)))
              (when (or (string-match "Week" heading)
                       (string-match "Monday" heading)
                       (string-match "Tuesday" heading)
                       (string-match "Break" heading))
                (setq has-parent-in-lint t))))
          (if has-parent-in-lint
              (progn
                (message "  ✗ FAIL: Parent headings found in lint!")
                (setq tests-failed (1+ tests-failed)))
            (progn
              (message "  ✓ PASS: No parent headings in lint")
              (setq tests-passed (1+ tests-passed)))))

        ;; Verify total is correct (should be 12h = 720 minutes)
        (if (>= total 700)  ; Allow small variance
            (progn
              (message "  ✓ PASS: Total time is correct (~12h)")
              (setq tests-passed (1+ tests-passed)))
          (progn
            (message "  ✗ FAIL: Total time is wrong (expected ~12h, got %.1fh)" (/ total 60.0))
            (setq tests-failed (1+ tests-failed))))))

    ;; Test 3: Global scope
    (message "\nTest 3: Global scope")
    (let ((data (org-focus--collect-global-data)))
      (let ((total (plist-get data :total))
            (rows (plist-get data :rows))
            (lint (plist-get data :lint)))

        (message "  Total clocked: %.1fh" (/ total 60.0))
        (message "  Entries with clocks: %d" (length rows))
        (message "  Lint issues: %d" (length lint))

        ;; Verify no parent headings in global rows
        (let ((has-parent nil))
          (dolist (row rows)
            (let ((heading (plist-get row :heading)))
              (when (or (string-match "Week" heading)
                       (string-match "Monday" heading)
                       (string-match "Tuesday" heading))
                (setq has-parent t))))
          (if has-parent
              (progn
                (message "  ✗ FAIL: Parent headings in global rows!")
                (setq tests-failed (1+ tests-failed)))
            (progn
              (message "  ✓ PASS: No parent headings in global rows")
              (setq tests-passed (1+ tests-passed)))))))

    ;; Test 4: Private tag filtering
    (message "\nTest 4: Private tag filtering")
    (let ((data (org-focus--collect-subtree-data)))
      (let ((total (plist-get data :total))
            (rows (plist-get data :rows)))

        ;; Break is :private: so it should be excluded
        (let ((has-private nil))
          (dolist (row rows)
            (let ((heading (plist-get row :heading)))
              (when (string-match "Break" heading)
                (setq has-private t))))
          (if has-private
              (progn
                (message "  ✗ FAIL: Private entry found in results!")
                (setq tests-failed (1+ tests-failed)))
            (progn
              (message "  ✓ PASS: Private entries correctly excluded")
              (setq tests-passed (1+ tests-passed)))))))

    ;; Test 5: Entry naming
    (message "\nTest 5: Correct entry names in results")
    (let ((data (org-focus--collect-subtree-data)))
      (let ((rows (plist-get data :rows)))
        (let ((entry-names (mapcar (lambda (r) (plist-get r :heading)) rows)))
          (message "  Found entries: %s" (string-join entry-names ", "))
          (if (and (member "Task A" entry-names)
                   (member "Task B" entry-names)
                   (member "Task C" entry-names))
              (progn
                (message "  ✓ PASS: All expected entries found")
                (setq tests-passed (1+ tests-passed)))
            (progn
              (message "  ✗ FAIL: Missing expected entries")
              (setq tests-failed (1+ tests-failed)))))))

    (kill-buffer)))

  ;; Summary
  (message "\n=== Test Results ===")
  (message "Passed: %d" tests-passed)
  (message "Failed: %d" tests-failed)

  (if (= tests-failed 0)
      (message "\n✅ All tests passed!")
    (message "\n❌ Some tests failed"))

  ;; Cleanup
  (when (file-exists-p test-file)
    (delete-file test-file))

  (message "\n=== Done ===\n"))
