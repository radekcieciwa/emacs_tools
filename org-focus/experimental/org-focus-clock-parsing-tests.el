;;; org-focus-clock-parsing-tests.el --- Tests for clock parsing logic -*- lexical-binding: t; -*-

;;; Commentary:
;; Comprehensive tests for org-focus--sum-clocks-in-range function
;; Tests validate that clock entries are parsed correctly and filtered by date range

;;; Code:

(require 'ert)
(require 'org)
(require 'org-clock)

;;; Helper function to create a test buffer with org content and return week range
(defun org-focus-test--create-buffer (content)
  "Create a temp buffer with CONTENT in org-mode."
  (with-temp-buffer
    (insert content)
    (org-mode)
    (list (current-buffer))))

;;; Test: Single clock entry in week range
(ert-deftest org-focus-test-clock-single-entry-in-range ()
  "Test summing a single clock entry within the week."
  (with-temp-buffer
    (insert "* TODO Task :prod:build:plan:
CLOCK: [2026-05-15 Fri 09:00]--[2026-05-15 Fri 17:00] =>  8:00
Some content here
")
    (org-mode)
    (goto-char (point-min))
    (org-next-visible-heading 1)

    (let* ((week-start (encode-time 0 0 0 11 5 2026))  ; 2026-05-11 (Monday)
           (week-end (encode-time 0 0 0 18 5 2026))    ; 2026-05-18 (Monday)
           (minutes (org-focus--sum-clocks-in-range week-start week-end)))
      (should (= minutes 480)))))  ; 8 hours = 480 minutes

;;; Test: Multiple clock entries in same task
(ert-deftest org-focus-test-clock-multiple-entries-same-task ()
  "Test summing multiple clock entries in the same task."
  (with-temp-buffer
    (insert "* TODO Task :prod:build:plan:
CLOCK: [2026-05-15 Fri 09:00]--[2026-05-15 Fri 12:00] =>  3:00
CLOCK: [2026-05-15 Fri 13:00]--[2026-05-15 Fri 17:00] =>  4:00
Content here
")
    (org-mode)
    (goto-char (point-min))
    (org-next-visible-heading 1)

    (let* ((week-start (encode-time 0 0 0 11 5 2026))
           (week-end (encode-time 0 0 0 18 5 2026))
           (minutes (org-focus--sum-clocks-in-range week-start week-end)))
      (should (= minutes 420)))))  ; 7 hours = 420 minutes

;;; Test: Clock entry outside week range (before)
(ert-deftest org-focus-test-clock-entry-before-week ()
  "Test that clock entries before the week are not counted."
  (with-temp-buffer
    (insert "* TODO Task :prod:build:plan:
CLOCK: [2026-05-10 Thu 09:00]--[2026-05-10 Thu 17:00] =>  8:00
Content here
")
    (org-mode)
    (goto-char (point-min))
    (org-next-visible-heading 1)

    (let* ((week-start (encode-time 0 0 0 11 5 2026))  ; Week starts Monday
           (week-end (encode-time 0 0 0 18 5 2026))
           (minutes (org-focus--sum-clocks-in-range week-start week-end)))
      (should (= minutes 0)))))

;;; Test: Clock entry outside week range (after)
(ert-deftest org-focus-test-clock-entry-after-week ()
  "Test that clock entries after the week are not counted."
  (with-temp-buffer
    (insert "* TODO Task :prod:build:plan:
CLOCK: [2026-05-18 Mon 09:00]--[2026-05-18 Mon 17:00] =>  8:00
Content here
")
    (org-mode)
    (goto-char (point-min))
    (org-next-visible-heading 1)

    (let* ((week-start (encode-time 0 0 0 11 5 2026))
           (week-end (encode-time 0 0 0 18 5 2026))  ; Exclusive end
           (minutes (org-focus--sum-clocks-in-range week-start week-end)))
      (should (= minutes 0)))))

;;; Test: Parent entry should not include child clocks
(ert-deftest org-focus-test-clock-parent-excludes-children ()
  "Test that parent entry doesn't include child entry clocks."
  (with-temp-buffer
    (insert "* TODO Parent :prod:build:plan:
CLOCK: [2026-05-15 Fri 09:00]--[2026-05-15 Fri 10:00] =>  1:00
** TODO Child :prod:build:plan:
CLOCK: [2026-05-15 Fri 10:00]--[2026-05-15 Fri 12:00] =>  2:00
More content
")
    (org-mode)
    (goto-char (point-min))
    (org-next-visible-heading 1)  ; Position at Parent

    (let* ((week-start (encode-time 0 0 0 11 5 2026))
           (week-end (encode-time 0 0 0 18 5 2026))
           (minutes (org-focus--sum-clocks-in-range week-start week-end)))
      (should (= minutes 60)))))  ; Only 1 hour from parent, not 3

;;; Test: Child entry should only count its own clocks
(ert-deftest org-focus-test-clock-child-excludes-parent ()
  "Test that child entry doesn't include parent entry clocks."
  (with-temp-buffer
    (insert "* TODO Parent :prod:build:plan:
CLOCK: [2026-05-15 Fri 09:00]--[2026-05-15 Fri 10:00] =>  1:00
** TODO Child :prod:build:plan:
CLOCK: [2026-05-15 Fri 10:00]--[2026-05-15 Fri 12:00] =>  2:00
More content
")
    (org-mode)
    (goto-char (point-min))
    (org-next-visible-heading 2)  ; Position at Child

    (let* ((week-start (encode-time 0 0 0 11 5 2026))
           (week-end (encode-time 0 0 0 18 5 2026))
           (minutes (org-focus--sum-clocks-in-range week-start week-end)))
      (should (= minutes 120)))))  ; Only 2 hours from child

;;; Test: Clock entries on boundary dates
(ert-deftest org-focus-test-clock-on-week-boundaries ()
  "Test clock entries on the exact week boundaries."
  (with-temp-buffer
    (insert "* TODO Monday :prod:build:plan:
CLOCK: [2026-05-11 Mon 00:00]--[2026-05-11 Mon 08:00] =>  8:00
** TODO Sunday :prod:build:plan:
CLOCK: [2026-05-17 Sun 16:00]--[2026-05-17 Sun 23:59] =>  7:59
")
    (org-mode)
    (goto-char (point-min))
    (org-next-visible-heading 1)  ; Monday entry

    (let* ((week-start (encode-time 0 0 0 11 5 2026))
           (week-end (encode-time 0 0 0 18 5 2026))
           (minutes (org-focus--sum-clocks-in-range week-start week-end)))
      (should (= minutes 480)))))

;;; Test: Entry with no clock lines
(ert-deftest org-focus-test-clock-no-entries ()
  "Test entry with no clock lines."
  (with-temp-buffer
    (insert "* TODO Task :prod:build:plan:
Some content but no clocks
")
    (org-mode)
    (goto-char (point-min))
    (org-next-visible-heading 1)

    (let* ((week-start (encode-time 0 0 0 11 5 2026))
           (week-end (encode-time 0 0 0 18 5 2026))
           (minutes (org-focus--sum-clocks-in-range week-start week-end)))
      (should (= minutes 0)))))

;;; Test: Mixed - some clocks in range, some outside
(ert-deftest org-focus-test-clock-mixed-in-and-out-of-range ()
  "Test entry with some clocks in range and some outside."
  (with-temp-buffer
    (insert "* TODO Task :prod:build:plan:
CLOCK: [2026-05-10 Thu 09:00]--[2026-05-10 Thu 17:00] =>  8:00
CLOCK: [2026-05-15 Fri 09:00]--[2026-05-15 Fri 17:00] =>  8:00
CLOCK: [2026-05-20 Wed 09:00]--[2026-05-20 Wed 17:00] =>  8:00
")
    (org-mode)
    (goto-char (point-min))
    (org-next-visible-heading 1)

    (let* ((week-start (encode-time 0 0 0 11 5 2026))
           (week-end (encode-time 0 0 0 18 5 2026))
           (minutes (org-focus--sum-clocks-in-range week-start week-end)))
      (should (= minutes 480)))))  ; Only Friday's 8 hours

;;; Test: Fractional hours (e.g., 2:30)
(ert-deftest org-focus-test-clock-fractional-hours ()
  "Test parsing clock entries with fractional hours."
  (with-temp-buffer
    (insert "* TODO Task :prod:build:plan:
CLOCK: [2026-05-15 Fri 09:00]--[2026-05-15 Fri 11:30] =>  2:30
CLOCK: [2026-05-16 Sat 14:15]--[2026-05-16 Sat 16:45] =>  2:30
")
    (org-mode)
    (goto-char (point-min))
    (org-next-visible-heading 1)

    (let* ((week-start (encode-time 0 0 0 11 5 2026))
           (week-end (encode-time 0 0 0 18 5 2026))
           (minutes (org-focus--sum-clocks-in-range week-start week-end)))
      (should (= minutes 300)))))  ; 2:30 + 2:30 = 5:00 = 300 minutes

;;; Test: Clock with seconds in duration
(ert-deftest org-focus-test-clock-with-seconds ()
  "Test parsing clock entries with seconds (should be ignored)."
  (with-temp-buffer
    (insert "* TODO Task :prod:build:plan:
CLOCK: [2026-05-15 Fri 09:00]--[2026-05-15 Fri 10:30] =>  1:30
")
    (org-mode)
    (goto-char (point-min))
    (org-next-visible-heading 1)

    (let* ((week-start (encode-time 0 0 0 11 5 2026))
           (week-end (encode-time 0 0 0 18 5 2026))
           (minutes (org-focus--sum-clocks-in-range week-start week-end)))
      (should (= minutes 90)))))  ; 1:30 = 90 minutes

;;; Test: Entire week of work
(ert-deftest org-focus-test-clock-full-week ()
  "Test typical week with multiple tasks and entries."
  (with-temp-buffer
    (insert "* TODO Monday task :prod:build:plan:
CLOCK: [2026-05-11 Mon 09:00]--[2026-05-11 Mon 17:00] =>  8:00
* TODO Wednesday task :prod:build:plan:
CLOCK: [2026-05-13 Wed 09:00]--[2026-05-13 Wed 12:00] =>  3:00
CLOCK: [2026-05-13 Wed 13:00]--[2026-05-13 Wed 17:00] =>  4:00
* TODO Friday task :prod:build:plan:
CLOCK: [2026-05-15 Fri 09:00]--[2026-05-15 Fri 16:00] =>  7:00
")
    (org-mode)
    (goto-char (point-min))

    ;; Test Monday task
    (org-next-visible-heading 1)
    (let* ((week-start (encode-time 0 0 0 11 5 2026))
           (week-end (encode-time 0 0 0 18 5 2026))
           (minutes-mon (org-focus--sum-clocks-in-range week-start week-end)))
      (should (= minutes-mon 480)))

    ;; Test Wednesday task
    (org-next-visible-heading 1)
    (let* ((week-start (encode-time 0 0 0 11 5 2026))
           (week-end (encode-time 0 0 0 18 5 2026))
           (minutes-wed (org-focus--sum-clocks-in-range week-start week-end)))
      (should (= minutes-wed 420)))

    ;; Test Friday task
    (org-next-visible-heading 1)
    (let* ((week-start (encode-time 0 0 0 11 5 2026))
           (week-end (encode-time 0 0 0 18 5 2026))
           (minutes-fri (org-focus--sum-clocks-in-range week-start week-end)))
      (should (= minutes-fri 420)))))

(provide 'org-focus-clock-parsing-tests)

;;; org-focus-clock-parsing-tests.el ends here
