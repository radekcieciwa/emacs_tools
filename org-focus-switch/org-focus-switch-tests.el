;;; org-focus-switch-tests.el --- ERT tests for org-focus-switch -*- lexical-binding: t; -*-

;; Run in batch:
;;   cd org-focus-switch
;;   emacs --batch -l ert -l org-focus-switch.el -l org-focus-switch-tests.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; Or evaluate individual forms interactively with C-M-x while editing.

(require 'ert)
(load-file "org-focus-switch.el")

(defmacro org-focus-switch-test--with-org (text &rest body)
  "Insert TEXT into a temp Org buffer, run BODY with point at min."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,text)
     (org-mode)
     (goto-char (point-min))
     ,@body))

(defun org-focus-switch-test--event (start-min task priority duration-min)
  "Build a synthetic event: START-MIN and DURATION-MIN are minutes.
TASK and PRIORITY tag it; times are stored as seconds."
  (list :start (* 60.0 start-min)
        :end (* 60.0 (+ start-min duration-min))
        :task task
        :priority priority))

;;;; Title / priority helpers

(ert-deftest org-focus-switch-test-normalize-title ()
  (should (equal "Fix login" (org-focus-switch--normalize-title "Fix   login")))
  (should (equal "Fix login" (org-focus-switch--normalize-title "  Fix login  ")))
  (should (equal nil (org-focus-switch--normalize-title nil))))

(ert-deftest org-focus-switch-test-title-ignores-status-priority-tags ()
  (org-focus-switch-test--with-org "* TODO [#A] Fix login :prod:build:\n"
    (should (equal "Fix login" (org-focus-switch--title-at-point)))))

(ert-deftest org-focus-switch-test-prio-label-none ()
  (should (equal "none" (org-focus-switch--prio-label nil)))
  (should (equal "none" (org-focus-switch--prio-label "")))
  (should (equal "P1" (org-focus-switch--prio-label "P1"))))

;;;; Ranking and direction

(ert-deftest org-focus-switch-test-rank-order ()
  ;; P0 most urgent (rank 0); unknown/none rank below all listed levels.
  (should (< (org-focus-switch--rank "P0") (org-focus-switch--rank "P1")))
  (should (< (org-focus-switch--rank "P2") (org-focus-switch--rank "none")))
  (should (= (org-focus-switch--rank "none") (org-focus-switch--rank "P?"))))

(ert-deftest org-focus-switch-test-direction ()
  (should (eq 'escalation (org-focus-switch--direction "P1" "P0")))
  (should (eq 'de-escalation (org-focus-switch--direction "P0" "P1")))
  (should (eq 'lateral (org-focus-switch--direction "P1" "P1")))
  ;; none is the least urgent, so leaving it is an escalation.
  (should (eq 'escalation (org-focus-switch--direction "none" "P2")))
  (should (eq 'de-escalation (org-focus-switch--direction "P0" "none"))))

;;;; Clock line parsing

(ert-deftest org-focus-switch-test-parse-clock-line ()
  (let ((event (org-focus-switch--parse-clock-line
                "CLOCK: [2026-05-13 Wed 10:00]--[2026-05-13 Wed 11:30] =>  1:30"
                "Task A" "P0")))
    (should event)
    (should (equal "Task A" (plist-get event :task)))
    (should (equal "P0" (plist-get event :priority)))
    ;; End is start + 90 minutes.
    (should (= (* 60.0 90) (- (plist-get event :end) (plist-get event :start))))))

(ert-deftest org-focus-switch-test-parse-clock-line-open-and-garbage ()
  ;; Open clock (no duration) and non-clock lines yield nil.
  (should (null (org-focus-switch--parse-clock-line
                 "CLOCK: [2026-05-13 Wed 10:00]" "T" nil)))
  (should (null (org-focus-switch--parse-clock-line "not a clock line" "T" nil))))

;;;; Analysis (pure)

(ert-deftest org-focus-switch-test-analyze-switches-and-edges ()
  ;; A A B C C A : three switches, four focus blocks.
  (let* ((events
          (list (org-focus-switch-test--event 0   "A" "P0" 30)
                (org-focus-switch-test--event 30  "A" "P0" 30)   ; same task, no switch
                (org-focus-switch-test--event 60  "B" "P1" 30)   ; A->B  P0->P1 de-escalation
                (org-focus-switch-test--event 90  "C" "P0" 30)   ; B->C  P1->P0 escalation
                (org-focus-switch-test--event 120 "C" "P0" 15)   ; same task, no switch
                (org-focus-switch-test--event 135 "A" "P0" 30))) ; C->A  P0->P0 lateral
         (data (org-focus-switch-analyze events)))
    (should (= 6 (plist-get data :events)))
    (should (= 3 (plist-get data :switches)))
    (should (= 4 (plist-get data :blocks)))
    (should (= 165 (round (plist-get data :total-minutes))))
    (should (= 1 (plist-get data :escalations)))
    (should (= 1 (plist-get data :de-escalations)))
    (should (= 1 (plist-get data :laterals)))
    (let ((edges (plist-get data :edges)))
      (should (= 1 (gethash '("P0" . "P1") edges 0)))
      (should (= 1 (gethash '("P1" . "P0") edges 0)))
      (should (= 1 (gethash '("P0" . "P0") edges 0))))
    ;; 3 switches over 2.75 focused hours.
    (should (< (abs (- (/ 3 2.75) (plist-get data :switches-per-hour))) 0.001))
    ;; 165 minutes across 4 blocks.
    (should (< (abs (- (/ 165.0 4) (plist-get data :avg-block-minutes))) 0.001))))

(ert-deftest org-focus-switch-test-analyze-orders-by-time ()
  ;; Events out of order still analyze by start time: B(0) then A(30) = 1 switch.
  (let* ((events (list (org-focus-switch-test--event 30 "A" "P1" 30)
                       (org-focus-switch-test--event 0  "B" "P0" 30)))
         (data (org-focus-switch-analyze events)))
    (should (= 1 (plist-get data :switches)))
    (should (= 1 (gethash '("P0" . "P1") (plist-get data :edges) 0)))))

(ert-deftest org-focus-switch-test-analyze-empty ()
  (let ((data (org-focus-switch-analyze '())))
    (should (= 0 (plist-get data :events)))
    (should (= 0 (plist-get data :switches)))
    (should (= 0 (plist-get data :blocks)))
    (should (= 0.0 (plist-get data :switches-per-hour)))))

(ert-deftest org-focus-switch-test-analyze-single-block ()
  ;; One task, no switches, but still one focus block.
  (let ((data (org-focus-switch-analyze
               (list (org-focus-switch-test--event 0  "A" "P0" 30)
                     (org-focus-switch-test--event 30 "A" "P0" 30)))))
    (should (= 0 (plist-get data :switches)))
    (should (= 1 (plist-get data :blocks)))))

(ert-deftest org-focus-switch-test-none-priority-edge ()
  ;; A switch involving an entry with no priority is labelled "none".
  (let* ((data (org-focus-switch-analyze
                (list (org-focus-switch-test--event 0  "A" "P0" 30)
                      (org-focus-switch-test--event 30 "B" nil  30)))))
    (should (= 1 (gethash '("P0" . "none") (plist-get data :edges) 0)))
    (should (= 1 (plist-get data :de-escalations)))))

;;;; Per-day distribution

(ert-deftest org-focus-switch-test-per-day-distribution ()
  ;; Two days of work.  Day 1: A->B->A = two switches.  Day 2: the overnight
  ;; boundary A->C counts (session gap is nil) and is attributed to day 2, plus
  ;; C->D, so day 2 = two switches.  A switch is attributed to the day of the
  ;; task switched *to*.
  (org-focus-switch-test--with-org
      (concat "* A\n:PROPERTIES:\n:FOCUS_PRIORITY: P0\n:END:\n"
              "CLOCK: [2026-05-13 Wed 09:00]--[2026-05-13 Wed 10:00] =>  1:00\n"
              "* B\n:PROPERTIES:\n:FOCUS_PRIORITY: P1\n:END:\n"
              "CLOCK: [2026-05-13 Wed 10:00]--[2026-05-13 Wed 10:30] =>  0:30\n"
              "* A\n:PROPERTIES:\n:FOCUS_PRIORITY: P0\n:END:\n"
              "CLOCK: [2026-05-13 Wed 10:30]--[2026-05-13 Wed 11:00] =>  0:30\n"
              "* C\n:PROPERTIES:\n:FOCUS_PRIORITY: P0\n:END:\n"
              "CLOCK: [2026-05-14 Thu 09:00]--[2026-05-14 Thu 10:00] =>  1:00\n"
              "* D\n:PROPERTIES:\n:FOCUS_PRIORITY: P2\n:END:\n"
              "CLOCK: [2026-05-14 Thu 10:00]--[2026-05-14 Thu 11:00] =>  1:00\n")
    (let* ((data (org-focus-switch-collect
                  (lambda (fn) (org-with-wide-buffer (org-map-entries fn nil nil)))))
           (per-day (plist-get data :per-day)))
      (should (= 2 (length per-day)))
      (should (equal "2026-05-13" (plist-get (nth 0 per-day) :date)))
      (should (equal "2026-05-14" (plist-get (nth 1 per-day) :date)))
      (should (= 2 (plist-get (nth 0 per-day) :switches)))
      (should (= 2 (plist-get (nth 1 per-day) :switches)))
      ;; Per-day switch counts sum to the overall switch count.
      (should (= (plist-get data :switches)
                 (apply #'+ (mapcar (lambda (d) (plist-get d :switches)) per-day)))))))

;;;; Session gap

(ert-deftest org-focus-switch-test-session-gap-suppresses-switch ()
  ;; With a session gap, a long idle boundary is a break, not a switch.
  (let ((org-focus-switch-session-gap-minutes 60))
    (let ((data (org-focus-switch-analyze
                 ;; A ends at 30; B starts at 120 -> 90-minute gap > 60.
                 (list (org-focus-switch-test--event 0   "A" "P0" 30)
                       (org-focus-switch-test--event 120 "B" "P1" 30)))))
      (should (= 0 (plist-get data :switches)))
      ;; But it still opens a new focus block.
      (should (= 2 (plist-get data :blocks)))
      (should (= 0 (hash-table-count (plist-get data :edges)))))))

(ert-deftest org-focus-switch-test-session-gap-nil-counts-all ()
  ;; Default (nil): the same long gap still counts as a switch.
  (let ((org-focus-switch-session-gap-minutes nil))
    (let ((data (org-focus-switch-analyze
                 (list (org-focus-switch-test--event 0   "A" "P0" 30)
                       (org-focus-switch-test--event 120 "B" "P1" 30)))))
      (should (= 1 (plist-get data :switches))))))

;;;; Collection over a buffer

(ert-deftest org-focus-switch-test-collect-buffer ()
  (org-focus-switch-test--with-org
      (concat "* Task A\n"
              ":PROPERTIES:\n:FOCUS_PRIORITY: P0\n:END:\n"
              "CLOCK: [2026-05-13 Wed 10:00]--[2026-05-13 Wed 10:30] =>  0:30\n"
              "* Task B\n"
              ":PROPERTIES:\n:FOCUS_PRIORITY: P1\n:END:\n"
              "CLOCK: [2026-05-13 Wed 10:30]--[2026-05-13 Wed 11:30] =>  1:00\n")
    (let ((data (org-focus-switch-collect
                 (lambda (fn) (org-with-wide-buffer (org-map-entries fn nil nil))))))
      (should (= 2 (plist-get data :events)))
      (should (= 1 (plist-get data :switches)))
      (should (= 1 (gethash '("P0" . "P1") (plist-get data :edges) 0)))
      (should (= 1 (plist-get data :de-escalations))))))

(ert-deftest org-focus-switch-test-collect-excludes-private-and-parents ()
  ;; Parent headings are structure only; :private: subtrees are skipped.
  (org-focus-switch-test--with-org
      (concat "* Project\n"                          ; parent: has children, skipped
              "** Task A\n"
              ":PROPERTIES:\n:FOCUS_PRIORITY: P0\n:END:\n"
              "CLOCK: [2026-05-13 Wed 09:00]--[2026-05-13 Wed 09:30] =>  0:30\n"
              "** Task B\n"
              ":PROPERTIES:\n:FOCUS_PRIORITY: P1\n:END:\n"
              "CLOCK: [2026-05-13 Wed 09:30]--[2026-05-13 Wed 10:00] =>  0:30\n"
              "* Secret :private:\n"
              "CLOCK: [2026-05-13 Wed 10:00]--[2026-05-13 Wed 12:00] =>  2:00\n")
    (let ((data (org-focus-switch-collect
                 (lambda (fn) (org-with-wide-buffer (org-map-entries fn nil nil))))))
      ;; Only the two leaf tasks contribute; the private clock is excluded.
      (should (= 2 (plist-get data :events)))
      (should (= 1 (plist-get data :switches)))
      (should (= 60 (round (plist-get data :total-minutes)))))))

;;;; Same-titled entries across days = same task

(ert-deftest org-focus-switch-test-same-title-not-a-switch ()
  ;; "Standup" logged twice around a different task: back-to-back same titles
  ;; are not a switch; the interleaved task produces two switches.
  (org-focus-switch-test--with-org
      (concat "* Standup\n"
              ":PROPERTIES:\n:FOCUS_PRIORITY: P1\n:END:\n"
              "CLOCK: [2026-05-13 Wed 09:00]--[2026-05-13 Wed 09:15] =>  0:15\n"
              "* Build feature\n"
              ":PROPERTIES:\n:FOCUS_PRIORITY: P0\n:END:\n"
              "CLOCK: [2026-05-13 Wed 09:15]--[2026-05-13 Wed 10:15] =>  1:00\n"
              "* Standup\n"
              ":PROPERTIES:\n:FOCUS_PRIORITY: P1\n:END:\n"
              "CLOCK: [2026-05-13 Wed 10:15]--[2026-05-13 Wed 10:20] =>  0:05\n")
    (let ((data (org-focus-switch-collect
                 (lambda (fn) (org-with-wide-buffer (org-map-entries fn nil nil))))))
      (should (= 3 (plist-get data :events)))
      (should (= 2 (plist-get data :switches)))
      (should (= 1 (gethash '("P1" . "P0") (plist-get data :edges) 0)))   ; escalation
      (should (= 1 (gethash '("P0" . "P1") (plist-get data :edges) 0))))))  ; de-escalation

;;;; Priority resolution

(ert-deftest org-focus-switch-test-priority-from-property ()
  (org-focus-switch-test--with-org
      (concat "* Task\n:PROPERTIES:\n:FOCUS_PRIORITY: P0\n:END:\n")
    (org-back-to-heading t)
    (should (equal "P0" (org-focus-switch--priority-at-point)))))

(ert-deftest org-focus-switch-test-priority-inherited-from-parent ()
  ;; Priority set on the project heading is inherited by the clocked leaf.
  (org-focus-switch-test--with-org
      (concat "* Project\n:PROPERTIES:\n:FOCUS_PRIORITY: P0\n:END:\n"
              "** Leaf task\n"
              "CLOCK: [2026-05-13 Wed 09:00]--[2026-05-13 Wed 09:30] =>  0:30\n")
    (goto-char (point-min))
    (re-search-forward "Leaf task")
    (let ((org-focus-switch-priority-inherit t))
      (should (equal "P0" (org-focus-switch--priority-at-point))))
    (let ((org-focus-switch-priority-inherit nil)
          (org-focus-switch-use-priority-cookie nil))
      (should (null (org-focus-switch--priority-at-point))))))

(ert-deftest org-focus-switch-test-priority-from-cookie ()
  ;; No FOCUS_PRIORITY, but a native [#A] cookie -> mapped to P0.
  (org-focus-switch-test--with-org "* TODO [#A] Task\n"
    (org-back-to-heading t)
    (let ((org-focus-switch-use-priority-cookie t))
      (should (equal "P0" (org-focus-switch--priority-at-point))))
    (let ((org-focus-switch-use-priority-cookie nil))
      (should (null (org-focus-switch--priority-at-point))))))

(ert-deftest org-focus-switch-test-cookie-edges-end-to-end ()
  ;; Real regression: cookie-only priorities must produce non-none edges.
  (org-focus-switch-test--with-org
      (concat "* [#A] Build feature\n"
              "CLOCK: [2026-05-13 Wed 09:00]--[2026-05-13 Wed 10:00] =>  1:00\n"
              "* [#B] Review PR\n"
              "CLOCK: [2026-05-13 Wed 10:00]--[2026-05-13 Wed 10:30] =>  0:30\n")
    (let ((data (org-focus-switch-collect
                 (lambda (fn) (org-with-wide-buffer (org-map-entries fn nil nil))))))
      (should (= 1 (gethash '("P0" . "P1") (plist-get data :edges) 0)))
      (should (= 0 (gethash '("none" . "none") (plist-get data :edges) 0))))))

;;;; Graph export

(defun org-focus-switch-test--sample-data ()
  "Build analysis data with a couple of distinct edges for export tests."
  (org-focus-switch-analyze
   (list (org-focus-switch-test--event 0  "A" "P0" 30)
         (org-focus-switch-test--event 30 "B" "P1" 30)     ; P0 -> P1 de-escalation
         (org-focus-switch-test--event 60 "C" "P0" 30))))  ; P1 -> P0 escalation

(ert-deftest org-focus-switch-test-export-dot ()
  (let ((dot (org-focus-switch-to-format (org-focus-switch-test--sample-data) 'dot)))
    (should (string-prefix-p "digraph priority_transitions {" dot))
    (should (string-match-p "\"P0\" -> \"P1\"" dot))
    (should (string-match-p "\"P1\" -> \"P0\"" dot))
    (should (string-suffix-p "}\n" dot))))

(ert-deftest org-focus-switch-test-export-mermaid ()
  (let ((mmd (org-focus-switch-to-format (org-focus-switch-test--sample-data) 'mermaid)))
    (should (string-prefix-p "flowchart LR" mmd))
    (should (string-match-p "P0 -->|1| P1" mmd))))

(ert-deftest org-focus-switch-test-export-graphml ()
  (let ((xml (org-focus-switch-to-format (org-focus-switch-test--sample-data) 'graphml)))
    (should (string-match-p "<graphml" xml))
    (should (string-match-p "<node id=\"P0\"/>" xml))
    (should (string-match-p "<edge source=\"P0\" target=\"P1\">" xml))
    (should (string-match-p "de-escalation" xml))))

(ert-deftest org-focus-switch-test-export-csv ()
  (let ((csv (org-focus-switch-to-format (org-focus-switch-test--sample-data) 'csv)))
    (should (string-prefix-p "from,to,weight,direction" csv))
    (should (string-match-p "P0,P1,1,de-escalation" csv))
    (should (string-match-p "P1,P0,1,escalation" csv))))

(ert-deftest org-focus-switch-test-export-json ()
  (let* ((json (org-focus-switch-to-format (org-focus-switch-test--sample-data) 'json))
         (parsed (let ((json-object-type 'alist)
                       (json-array-type 'list))
                   (json-read-from-string json))))
    (should (eq t (cdr (assq 'directed parsed))))
    (should (member "P0" (cdr (assq 'nodes parsed))))
    (should (= 2 (cdr (assq 'switches (cdr (assq 'metrics parsed))))))
    (let ((edges (cdr (assq 'edges parsed))))
      (should (cl-some (lambda (e) (and (equal "P0" (cdr (assq 'from e)))
                                        (equal "P1" (cdr (assq 'to e)))))
                       edges)))))

(ert-deftest org-focus-switch-test-export-unknown-format ()
  (should-error (org-focus-switch-to-format (org-focus-switch-test--sample-data) 'svg)))

;;;; Duration formatting

(ert-deftest org-focus-switch-test-format-duration ()
  (should (equal "0m" (org-focus-switch--format-duration 0)))
  (should (equal "45m" (org-focus-switch--format-duration 45)))
  (should (equal "2h" (org-focus-switch--format-duration 120)))
  (should (equal "1h 30m" (org-focus-switch--format-duration 90))))

;;;; Rendering smoke tests

(ert-deftest org-focus-switch-test-embedded-render-is-trimmed ()
  ;; The embedded (org-focus) render shows the summary + direction tally +
  ;; per-day distribution, but NOT the matrix or per-edge listing.
  (let* ((data (org-focus-switch-analyze
                (list (org-focus-switch-test--event 0  "A" "P0" 30)
                      (org-focus-switch-test--event 30 "B" "P1" 30))))
         (text (with-temp-buffer
                 (org-focus-switch-render data)
                 (buffer-string))))
    (should (string-match-p "Task Switching" text))
    (should (string-match-p "Task switches:" text))
    (should (string-match-p "Switches per day" text))
    (should (string-match-p "Direction:" text))
    ;; Trimmed: no matrix and no grouped edge sections.
    (should-not (string-match-p "Priority transitions" text))
    (should-not (string-match-p "P0 +-> +P1" text))))

(ert-deftest org-focus-switch-test-dashboard-render-has-graph-and-groups ()
  ;; The dashboard body carries both conclusions plus the reworked
  ;; three-section grouping (Lateral / Escalation / De-escalation).
  (let* ((data (org-focus-switch-analyze
                (list (org-focus-switch-test--event 0  "A" "P0" 30)
                      (org-focus-switch-test--event 30 "B" "P1" 30)
                      (org-focus-switch-test--event 60 "C" "P0" 30))))
         (text (with-temp-buffer
                 (org-focus-switch--render-dashboard-content data)
                 (buffer-string))))
    (should (string-match-p "Switch frequency" text))
    (should (string-match-p "Switches per day" text))
    (should (string-match-p "Priority transitions" text))
    (should (string-match-p "Lateral (0)" text))
    (should (string-match-p "Escalation (1)" text))
    (should (string-match-p "De-escalation (1)" text))
    (should (string-match-p "P0 +-> +P1" text))
    ;; The flat "Edges (by frequency)" section is gone.
    (should-not (string-match-p "Edges (by frequency)" text))))

;;; org-focus-switch-tests.el ends here
