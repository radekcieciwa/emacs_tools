;;; org-task-report-tests.el --- ERT tests for org-task-report -*- lexical-binding: t; -*-

;; Run in batch:
;;   cd org-task-report
;;   emacs --batch -l ert -l org-task-report.el -l org-task-report-tests.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; Or evaluate individual forms interactively with C-M-x while editing.

(require 'ert)
(load-file "org-task-report.el")

(defmacro org-task-report-test--with-org (text &rest body)
  "Insert TEXT into a temp Org buffer, run BODY with point at min."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,text)
     (org-mode)
     (goto-char (point-min))
     ,@body))

;;;; Title normalization

(ert-deftest org-task-report-test-normalize-title ()
  (should (equal "Fix login" (org-task-report--normalize-title "Fix   login")))
  (should (equal "Fix login" (org-task-report--normalize-title "  Fix login  ")))
  (should (equal nil (org-task-report--normalize-title nil))))

(ert-deftest org-task-report-test-title-ignores-status-priority-tags ()
  (org-task-report-test--with-org "* TODO [#A] Fix login :bug:urgent:\n"
    (should (equal "Fix login" (org-task-report--title-at-point)))))

;;;; Clock parsing

(ert-deftest org-task-report-test-parse-clock-line ()
  (should (equal '("2026-05-15" . 480)
                 (org-task-report--parse-clock-line
                  "CLOCK: [2026-05-15 Fri 09:00]--[2026-05-15 Fri 17:00] =>  8:00")))
  ;; Open clock: date present, zero minutes.
  (should (equal '("2026-05-15" . 0)
                 (org-task-report--parse-clock-line
                  "CLOCK: [2026-05-15 Fri 09:00]")))
  (should (equal nil (org-task-report--parse-clock-line "not a clock line"))))

(ert-deftest org-task-report-test-entry-clocks-sums-and-excludes-children ()
  (org-task-report-test--with-org
      (concat "* TODO Parent\n"
              "CLOCK: [2026-05-13 Wed 09:00]--[2026-05-13 Wed 12:00] =>  3:00\n"
              "CLOCK: [2026-05-13 Wed 13:00]--[2026-05-13 Wed 17:00] =>  4:00\n"
              "** TODO Child\n"
              "CLOCK: [2026-05-13 Wed 17:00]--[2026-05-13 Wed 19:00] =>  2:00\n")
    (let ((clocks (org-task-report--entry-clocks)))
      (should (= 2 (length clocks)))
      (should (= 420 (org-task-report--entry-minutes clocks)))
      (should (equal "2026-05-13" (org-task-report--entry-date clocks))))))

(ert-deftest org-task-report-test-entry-date-is-earliest ()
  (should (equal "2026-05-10"
                 (org-task-report--entry-date
                  '(("2026-05-15" . 60) ("2026-05-10" . 30) ("2026-05-12" . 15))))))

;;;; Note extraction

(ert-deftest org-task-report-test-entry-note-strips-noise ()
  (org-task-report-test--with-org
      (concat "* TODO Work\n"
              ":PROPERTIES:\n:ID: abc\n:END:\n"
              "SCHEDULED: <2026-05-13 Wed>\n"
              "CLOCK: [2026-05-13 Wed 09:00]--[2026-05-13 Wed 10:00] =>  1:00\n"
              "Investigated the crash.\n"
              "Root cause found.\n")
    (should (equal "Investigated the crash.\nRoot cause found."
                   (org-task-report--entry-note)))))

(ert-deftest org-task-report-test-entry-note-empty ()
  (org-task-report-test--with-org
      (concat "* TODO Work\n"
              "CLOCK: [2026-05-13 Wed 09:00]--[2026-05-13 Wed 10:00] =>  1:00\n")
    (should (equal nil (org-task-report--entry-note)))))

;;;; Collection + sorting

(ert-deftest org-task-report-test-collect-matching-titles ()
  (org-task-report-test--with-org
      (concat "* TODO [#A] Deploy service :ops:\n"
              "CLOCK: [2026-05-14 Thu 09:00]--[2026-05-14 Thu 10:30] =>  1:30\n"
              "Second day of work.\n"
              "* DONE Deploy service\n"
              "CLOCK: [2026-05-13 Wed 09:00]--[2026-05-13 Wed 11:00] =>  2:00\n"
              "First day of work.\n"
              "* TODO Unrelated task\n"
              "CLOCK: [2026-05-13 Wed 11:00]--[2026-05-13 Wed 12:00] =>  1:00\n")
    (let ((rows (org-task-report--collect "Deploy service")))
      (should (= 2 (length rows)))
      ;; Sorted by date ascending.
      (should (equal "2026-05-13" (plist-get (nth 0 rows) :date)))
      (should (equal "2026-05-14" (plist-get (nth 1 rows) :date)))
      (should (equal "First day of work." (plist-get (nth 0 rows) :note)))
      (should (= 120 (plist-get (nth 0 rows) :minutes)))
      (should (= 90 (plist-get (nth 1 rows) :minutes))))))

(ert-deftest org-task-report-test-collect-same-date-separate-entries ()
  ;; Edge case: two entries, same title, same date -> kept separate.
  (org-task-report-test--with-org
      (concat "* TODO Standup\n"
              "CLOCK: [2026-05-13 Wed 09:00]--[2026-05-13 Wed 09:15] =>  0:15\n"
              "Morning.\n"
              "* TODO Standup\n"
              "CLOCK: [2026-05-13 Wed 16:00]--[2026-05-13 Wed 16:10] =>  0:10\n"
              "Afternoon.\n")
    (let ((rows (org-task-report--collect "Standup")))
      (should (= 2 (length rows)))
      (should (equal "2026-05-13" (plist-get (nth 0 rows) :date)))
      (should (equal "2026-05-13" (plist-get (nth 1 rows) :date))))))

;;;; Duration formatting

(ert-deftest org-task-report-test-format-hm ()
  (should (equal "0m" (org-task-report--format-hm 0)))
  (should (equal "45m" (org-task-report--format-hm 45)))
  (should (equal "2h" (org-task-report--format-hm 120)))
  (should (equal "1h 30m" (org-task-report--format-hm 90))))

;;; org-task-report-tests.el ends here
