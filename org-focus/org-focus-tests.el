;;; org-focus-tests.el --- Tests for org-focus -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for the org-focus package.
;;
;; Run from this directory with:
;;   emacs -Q --batch -L . -l org-focus-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'org-focus)
(require 'ert)

;;; Pure utility functions

(ert-deftest org-focus-test-member-count ()
  "Test counting tag membership."
  (let ((tags '("prod" "build" "plan")))
    (should (= 0 (org-focus--member-count '("team" "org") tags)))
    (should (= 1 (org-focus--member-count '("prod" "team") tags)))
    (should (= 2 (org-focus--member-count '("prod" "build" "ops") tags)))
    (should (= 3 (org-focus--member-count '("prod" "build" "plan") tags)))))

(ert-deftest org-focus-test-present-tags ()
  "Test filtering tags by candidates."
  (let ((tags '("prod" "build" "plan" "invest")))
    (should (equal '() (org-focus--present-tags '("team" "org") tags)))
    (should (equal '("prod") (org-focus--present-tags '("prod" "team") tags)))
    (should (equal '("build" "plan") (org-focus--present-tags '("build" "plan" "help") tags)))))

(ert-deftest org-focus-test-format-hours ()
  "Test formatting minutes as decimal hours."
  (should (string= "0.0h" (org-focus--format-hours 0)))
  (should (string= "1.0h" (org-focus--format-hours 60)))
  (should (string= "1.5h" (org-focus--format-hours 90)))
  (should (string= "2.5h" (org-focus--format-hours 150)))
  (should (string= "8.0h" (org-focus--format-hours 480))))

(ert-deftest org-focus-test-format-ratio ()
  "Test formatting ratio as percentage."
  (should (string= "0%" (org-focus--format-ratio 0 0)))
  (should (string= "0%" (org-focus--format-ratio 0 100)))
  (should (string= "50%" (org-focus--format-ratio 50 100)))
  (should (string= "33%" (org-focus--format-ratio 33 100)))
  (should (string= "100%" (org-focus--format-ratio 100 100))))

(ert-deftest org-focus-test-timestamp-within-days-p ()
  "Test checking if a timestamp is within N days."
  (let* ((now (current-time))
         (one-day-ago (time-subtract now (days-to-time 1)))
         (two-days-ago (time-subtract now (days-to-time 2)))
         (five-days-ago (time-subtract now (days-to-time 5))))
    (should (org-focus--timestamp-within-days-p one-day-ago 3))
    (should (org-focus--timestamp-within-days-p two-days-ago 3))
    (should (not (org-focus--timestamp-within-days-p five-days-ago 3)))))

(ert-deftest org-focus-test-urgent-priority ()
  "Test that the urgent priority is the first configured level."
  (should (string= "P0" (org-focus--urgent-priority)))
  (let ((org-focus-priorities '("A" "B" "C")))
    (should (string= "A" (org-focus--urgent-priority)))))

;;; Week boundary calculation

(ert-deftest org-focus-test-week-start-end ()
  "Test that week boundaries are computed correctly."
  (let* ((result (org-focus--week-start-end))
         (start (car result))
         (end (cadr result)))
    (should start)
    (should end)
    (should (> (float-time end) (float-time start)))
    (let ((delta-seconds (float-time (time-subtract end start))))
      (should (< (abs (- delta-seconds (* 7 24 60 60))) 2)))))

(ert-deftest org-focus-test-week-start-end-is-list ()
  "Test that week boundaries return a two-element list."
  (let ((result (org-focus--week-start-end)))
    (should (listp result))
    (should (= 2 (length result)))))

;;; Entry linting: org-focus--entry-issues

(ert-deftest org-focus-test-entry-issues-empty-tags ()
  "Test entry-issues with no tags on an active entry."
  (with-temp-buffer
    (insert "* TODO Entry with no tags\n")
    (org-mode)
    (goto-char (point-min))
    (org-next-visible-heading 0)
    (let ((issues (org-focus--entry-issues t)))
      (should (>= (length issues) 3))
      (should (seq-find (lambda (i) (string-match-p "Missing domain" i)) issues))
      (should (seq-find (lambda (i) (string-match-p "Missing activity" i)) issues))
      (should (seq-find (lambda (i) (string-match-p "Missing.*intent" i)) issues)))))

(ert-deftest org-focus-test-entry-issues-with-all-tags ()
  "Test a valid entry with all required tags."
  (with-temp-buffer
    (insert "* TODO Valid entry :prod:build:plan:\n")
    (org-mode)
    (goto-char (point-min))
    (org-next-visible-heading 0)
    (should (null (org-focus--entry-issues t)))))

(ert-deftest org-focus-test-entry-issues-done-no-check ()
  "Test that DONE entries are not checked for metadata."
  (with-temp-buffer
    (insert "* DONE Completed entry\n")
    (org-mode)
    (goto-char (point-min))
    (org-next-visible-heading 0)
    (should (null (org-focus--entry-issues t)))))

(ert-deftest org-focus-test-entry-issues-multiple-domain-tags ()
  "Test detection of multiple domain tags."
  (with-temp-buffer
    (insert "* TODO Entry :prod:team:build:plan:\n")
    (org-mode)
    (goto-char (point-min))
    (org-next-visible-heading 0)
    (let ((issues (org-focus--entry-issues t)))
      (should (seq-find (lambda (i) (string-match-p "Multiple domain" i)) issues)))))

(ert-deftest org-focus-test-entry-issues-multiple-activity-tags ()
  "Test detection of multiple activity tags."
  (with-temp-buffer
    (insert "* TODO Entry :prod:build:help:plan:\n")
    (org-mode)
    (goto-char (point-min))
    (org-next-visible-heading 0)
    (let ((issues (org-focus--entry-issues t)))
      (should (seq-find (lambda (i) (string-match-p "Multiple activity" i)) issues)))))

;;; Urgent (P0) detection: org-focus--active-p0-p

(ert-deftest org-focus-test-active-p0-p-true ()
  "Test that an active TODO with FOCUS_PRIORITY P0 is detected as urgent."
  (with-temp-buffer
    (insert "* TODO Urgent task\n:PROPERTIES:\n:FOCUS_PRIORITY: P0\n:END:\n")
    (org-mode)
    (goto-char (point-min))
    (org-next-visible-heading 0)
    (should (org-focus--active-p0-p))))

(ert-deftest org-focus-test-active-p0-p-done-is-not-active ()
  "Test that a DONE P0 task is not counted as active."
  (with-temp-buffer
    (insert "* DONE Urgent task\n:PROPERTIES:\n:FOCUS_PRIORITY: P0\n:END:\n")
    (org-mode)
    (goto-char (point-min))
    (org-next-visible-heading 0)
    (should (not (org-focus--active-p0-p)))))

(ert-deftest org-focus-test-active-p0-p-lower-priority ()
  "Test that a P1 task is not counted as urgent."
  (with-temp-buffer
    (insert "* TODO Task\n:PROPERTIES:\n:FOCUS_PRIORITY: P1\n:END:\n")
    (org-mode)
    (goto-char (point-min))
    (org-next-visible-heading 0)
    (should (not (org-focus--active-p0-p)))))

;;; Row grouping: org-focus--group-rows

(ert-deftest org-focus-test-group-rows-sums-and-counts ()
  "Same-heading rows are merged: minutes summed, occurrences counted."
  (let* ((rows (list (list :heading "Build" :minutes 60 :priority "P0" :marker 'm1 :file "a")
                     (list :heading "Help"  :minutes 30 :priority nil  :marker 'm2 :file "a")
                     (list :heading "Build" :minutes 90 :priority "P0" :marker 'm3 :file "b")))
         (groups (org-focus--group-rows rows))
         (build (seq-find (lambda (g) (string= (plist-get g :heading) "Build")) groups))
         (help (seq-find (lambda (g) (string= (plist-get g :heading) "Help")) groups)))
    (should (= 2 (length groups)))
    (should (= 150 (plist-get build :minutes)))
    (should (= 2 (plist-get build :count)))
    (should (= 30 (plist-get help :minutes)))
    (should (= 1 (plist-get help :count)))
    ;; Metadata comes from the first-seen occurrence.
    (should (eq 'm1 (plist-get build :marker)))))

(ert-deftest org-focus-test-group-rows-sorted-descending ()
  "Groups are returned sorted by total minutes, largest first."
  (let* ((rows (list (list :heading "Small" :minutes 10 :priority nil :marker nil :file nil)
                     (list :heading "Big"   :minutes 120 :priority nil :marker nil :file nil)
                     (list :heading "Mid"   :minutes 60 :priority nil :marker nil :file nil)
                     (list :heading "Small" :minutes 5 :priority nil :marker nil :file nil)))
         (groups (org-focus--group-rows rows)))
    (should (equal '("Big" "Mid" "Small")
                   (mapcar (lambda (g) (plist-get g :heading)) groups)))
    (should (= 15 (plist-get (car (last groups)) :minutes)))))

;;; Per-child decomposition: org-focus--collect-children-data

(ert-deftest org-focus-test-collect-children-data ()
  "Direct children of the subtree are decomposed individually."
  (with-temp-buffer
    (insert "* Week\n"
            "** Day Mon\n"
            "*** TODO A :prod:build:plan:\n"
            "CLOCK: [2026-06-08 Mon 09:00]--[2026-06-08 Mon 11:00] =>  2:00\n"
            "** Day Tue\n"
            "*** TODO B :team:help:unplan:\n"
            "CLOCK: [2026-06-09 Tue 09:00]--[2026-06-09 Tue 10:00] =>  1:00\n")
    (org-mode)
    (goto-char (point-min)) ; on the Week heading
    (let* ((children (org-focus--collect-children-data))
           (mon (nth 0 children))
           (tue (nth 1 children)))
      (should (= 2 (length children)))
      (should (string= "Day Mon" (plist-get mon :heading)))
      (should (string= "Day Tue" (plist-get tue :heading)))
      (should (= 120 (plist-get (plist-get mon :data) :total)))
      (should (= 60 (plist-get (plist-get tue :data) :total)))
      (should (= 120 (gethash "prod" (plist-get (plist-get mon :data) :by-domain) 0)))
      (should (= 60 (gethash "help" (plist-get (plist-get tue :data) :by-activity) 0))))))

(ert-deftest org-focus-test-collect-children-data-leaf ()
  "A subtree with no child headings yields no children."
  (with-temp-buffer
    (insert "* Lonely :prod:build:plan:\n"
            "CLOCK: [2026-06-08 Mon 09:00]--[2026-06-08 Mon 10:00] =>  1:00\n")
    (org-mode)
    (goto-char (point-min))
    (should (null (org-focus--collect-children-data)))))

;;; Warning generation: org-focus--warning-lines

(defun org-focus-tests--data (&rest overrides)
  "Build a baseline dashboard data plist, merging OVERRIDES on top.
Hash-table sub-tables are always fresh and empty unless filled by caller."
  (let ((base (list :total 480
                    :invest 100
                    :active-a 1
                    :stale-a '()
                    :lint '()
                    :by-activity (make-hash-table :test #'equal)
                    :by-intent (make-hash-table :test #'equal))))
    (while overrides
      (setq base (plist-put base (pop overrides) (pop overrides))))
    base))

(ert-deftest org-focus-test-warning-lines-clean ()
  "Test that no warnings are generated for clean data."
  (let ((data (org-focus-tests--data :invest 100)))
    (puthash "plan" 480 (plist-get data :by-intent))
    (puthash "unplan" 0 (plist-get data :by-intent))
    (puthash "sync" 0 (plist-get data :by-activity))
    (puthash "help" 0 (plist-get data :by-activity))
    (should (null (org-focus--warning-lines data)))))

(ert-deftest org-focus-test-warning-lines-investment-low ()
  "Test warning when investment time is below target."
  (let ((data (org-focus-tests--data :invest 50)))
    (puthash "plan" 430 (plist-get data :by-intent))
    (should (seq-find (lambda (w) (string-match-p "Investment below target" w))
                      (org-focus--warning-lines data)))))

(ert-deftest org-focus-test-warning-lines-unplanned-high ()
  "Test warning when unplanned work exceeds the threshold."
  (let ((data (org-focus-tests--data)))
    (puthash "plan" 250 (plist-get data :by-intent))
    (puthash "unplan" 230 (plist-get data :by-intent))
    (should (seq-find (lambda (w) (string-match-p "Unplanned work high" w))
                      (org-focus--warning-lines data)))))

(ert-deftest org-focus-test-warning-lines-sync-high ()
  "Test warning when sync/meeting time exceeds the threshold."
  (let ((data (org-focus-tests--data)))
    (puthash "sync" 150 (plist-get data :by-activity))
    (puthash "help" 50 (plist-get data :by-activity))
    (puthash "build" 280 (plist-get data :by-activity))
    (puthash "plan" 480 (plist-get data :by-intent))
    (should (seq-find (lambda (w) (string-match-p "Sync/meeting time high" w))
                      (org-focus--warning-lines data)))))

(ert-deftest org-focus-test-warning-lines-help-high ()
  "Test warning when help/support time exceeds the threshold."
  (let ((data (org-focus-tests--data)))
    (puthash "help" 160 (plist-get data :by-activity))
    (puthash "build" 320 (plist-get data :by-activity))
    (puthash "plan" 480 (plist-get data :by-intent))
    (should (seq-find (lambda (w) (string-match-p "Help/support time high" w))
                      (org-focus--warning-lines data)))))

(ert-deftest org-focus-test-warning-lines-too-many-active-p0 ()
  "Test warning when too many active urgent tasks exist."
  (let ((data (org-focus-tests--data :active-a 4)))
    (puthash "plan" 480 (plist-get data :by-intent))
    (should (seq-find (lambda (w) (string-match-p "Too many active P0" w))
                      (org-focus--warning-lines data)))))

(ert-deftest org-focus-test-warning-lines-stale-p0-tasks ()
  "Test warning for stale urgent tasks."
  (let ((data (org-focus-tests--data
               :stale-a '((:heading "Old task" :marker nil :file nil :last-clock nil)
                          (:heading "Older task" :marker nil :file nil :last-clock nil)))))
    (puthash "plan" 480 (plist-get data :by-intent))
    (should (seq-find (lambda (w) (string-match-p "Stale P0" w))
                      (org-focus--warning-lines data)))))

(ert-deftest org-focus-test-warning-lines-lint-issues ()
  "Test warning for metadata lint issues."
  (let ((data (org-focus-tests--data
               :lint '((:heading "Bad entry 1" :issues ("Missing domain") :marker nil :file nil)
                       (:heading "Bad entry 2" :issues ("Missing priority") :marker nil :file nil)))))
    (puthash "plan" 480 (plist-get data :by-intent))
    (should (seq-find (lambda (w) (string-match-p "Entries with metadata issues" w))
                      (org-focus--warning-lines data)))))

(ert-deftest org-focus-test-warning-lines-multiple ()
  "Test multiple warnings at once."
  (let ((data (org-focus-tests--data
               :invest 50
               :active-a 3
               :stale-a '((:heading "Stale" :marker nil :file nil :last-clock nil))
               :lint '((:heading "Bad" :issues ("Missing domain") :marker nil :file nil)))))
    (puthash "plan" 300 (plist-get data :by-intent))
    (puthash "unplan" 180 (plist-get data :by-intent))
    (puthash "sync" 0 (plist-get data :by-activity))
    (puthash "help" 0 (plist-get data :by-activity))
    (should (>= (length (org-focus--warning-lines data)) 4))))

(provide 'org-focus-tests)

;;; org-focus-tests.el ends here
