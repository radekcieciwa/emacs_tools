;;; org-focus-switch.el --- Task-switch and priority-transition analysis for Org clocks -*- lexical-binding: t; -*-

;; Author: Radoslaw Cieciwa
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (org "9.6"))
;; Keywords: outlines, time, convenience
;; URL: https://github.com/radekcieciwa/emacs_tools

;;; Commentary:
;;
;; Org clocks record *how long* you spent on each task, but not *how often you
;; jumped between them*.  This package reconstructs the chronological sequence
;; of clock events in a scope and extracts:
;;
;;   1. Task-switch frequency.  How often did you change tasks?  Reported as
;;      switches per focused hour, the average uninterrupted focus block
;;      length, and a per-day distribution.  A high rate means fragmented
;;      attention; a low rate means long focus periods.
;;
;;   2. The priority-transition graph.  Every task switch is an edge from the
;;      priority you left to the priority you moved to (P0 -> P1, P1 -> P1,
;;      etc.), read from the FOCUS_PRIORITY property.  The edges form a
;;      directed weighted graph, rendered as an adjacency matrix and grouped
;;      into three sections -- lateral, escalation and de-escalation -- each
;;      carrying its switch count.
;;
;; The heart of the package is `org-focus-switch-analyze', a *pure* function
;; over a list of event plists.  `org-focus-switch-collect' scans an Org scope
;; into such events and hands them to it, so the analysis is testable without
;; any buffers.
;;
;; Consumption:
;; - Dashboard: `M-x org-focus-switch' (bound to C-c t s in Org buffers) opens
;;   the Org Focus Switch dashboard for the subtree at point (whole buffer with
;;   a prefix argument).  It shows the frequency summary, the per-day
;;   distribution and the full priority-transition graph, and exports the graph
;;   via `e' (DOT/Mermaid/GraphML/CSV/JSON).  Export works only here.
;; - Embedded: `org-focus' requires this package and renders a compact "Task
;;   Switching" section in its dashboard via `org-focus-switch-collect' /
;;   `org-focus-switch-render' -- the frequency summary, direction tally and
;;   per-day distribution, but not the transition matrix or per-edge graph
;;   (those live in the dedicated dashboard).
;;
;; Design notes:
;; - Task identity is the normalized heading (TODO state, priority cookie and
;;   tags stripped), so two same-titled entries logged on different days are the
;;   *same* task; clocking them back-to-back is not a switch, while jumping to a
;;   different title and back counts as two switches.
;; - Only leaf (childless) entries are measured and `:private:'-tagged subtrees
;;   are skipped, mirroring the org-focus taxonomy.  Only closed clocks (those
;;   with a `=> H:MM' duration) participate.
;; - The event end time is derived as start + duration, so the trailing
;;   timestamp's exact format is irrelevant.

;;; Code:

(require 'org)
(require 'org-clock)
(require 'subr-x)
(require 'cl-lib)
(require 'json)

(defgroup org-focus-switch nil
  "Task-switch and priority-transition analysis for Org clocks."
  :group 'org)

(defcustom org-focus-switch-priorities '("P0" "P1" "P2")
  "Priority levels in rank order, most urgent first.
Read from the property named by `org-focus-switch-priority-property'.
The rank drives escalation/de-escalation classification: the earlier a
level appears here, the more urgent it is.  Levels not present in this
list (including entries with no priority) rank below every listed level."
  :type '(repeat string)
  :group 'org-focus-switch)

(defcustom org-focus-switch-priority-property "FOCUS_PRIORITY"
  "Org property that stores an entry's focus priority."
  :type 'string
  :group 'org-focus-switch)

(defcustom org-focus-switch-none-label "none"
  "Label used for clock events whose entry has no priority set."
  :type 'string
  :group 'org-focus-switch)

(defcustom org-focus-switch-exclude-tags '("private")
  "Tags whose subtrees are excluded from switch analysis."
  :type '(repeat string)
  :group 'org-focus-switch)

(defcustom org-focus-switch-session-gap-minutes nil
  "Idle gap, in minutes, that separates work sessions.
When a positive number, a boundary between two consecutive clock events
whose gap exceeds it is treated as a session break: it is not counted as
a task switch and produces no priority edge, but it does start a new
focus block (so overnight or lunch gaps do not inflate the switch rate).
When nil, every boundary between differently-titled consecutive events
counts as a switch, regardless of the gap."
  :type '(choice (const :tag "No session splitting" nil) integer)
  :group 'org-focus-switch)

(defcustom org-focus-switch-priority-inherit t
  "When non-nil, read the priority property with inheritance.
An entry with no priority of its own then borrows the nearest ancestor's
`org-focus-switch-priority-property' value.  This is the common case when
priority is set on a project/section heading rather than on each leaf."
  :type 'boolean
  :group 'org-focus-switch)

(defcustom org-focus-switch-use-priority-cookie t
  "When non-nil, fall back to Org's native priority cookie ([#A]).
Used only when the entry (and, with inheritance, its ancestors) has no
`org-focus-switch-priority-property'.  The cookie character is mapped to a
focus priority label via `org-focus-switch-cookie-priority-alist'."
  :type 'boolean
  :group 'org-focus-switch)

(defcustom org-focus-switch-cookie-priority-alist '((?A . "P0") (?B . "P1") (?C . "P2"))
  "Map Org priority-cookie characters to focus priority labels.
Consulted only when `org-focus-switch-use-priority-cookie' is non-nil."
  :type '(alist :key-type character :value-type string)
  :group 'org-focus-switch)

(defcustom org-focus-switch-buffer-name "*Org Focus Switch Dashboard*"
  "Name of the Org Focus Switch dashboard buffer."
  :type 'string
  :group 'org-focus-switch)

(defcustom org-focus-switch-export-buffer-name "*Org Task Switch Export*"
  "Name of the graph-export buffer."
  :type 'string
  :group 'org-focus-switch)

(defcustom org-focus-switch-export-formats
  '(("dot"     . dot)
    ("mermaid" . mermaid)
    ("graphml" . graphml)
    ("csv"     . csv)
    ("json"    . json))
  "Alist of (NAME . SYMBOL) graph-export formats.
Offered for completion by `org-focus-switch-export'."
  :type '(alist :key-type string :value-type symbol)
  :group 'org-focus-switch)

;;;; Entry predicates and accessors

(defun org-focus-switch--normalize-title (heading)
  "Normalize HEADING for task identity: collapse whitespace and trim."
  (when heading
    (string-trim (replace-regexp-in-string "[ \t]+" " " heading))))

(defun org-focus-switch--title-at-point ()
  "Return the normalized task title of the entry at point.
`org-get-heading' already drops TODO state, the priority cookie and tags."
  (org-focus-switch--normalize-title (org-get-heading t t t t)))

(defun org-focus-switch--excluded-p ()
  "Return non-nil when the entry at point is excluded from analysis."
  (let ((tags (org-get-tags)))
    (cl-some (lambda (tag) (member tag tags)) org-focus-switch-exclude-tags)))

(defun org-focus-switch--priority-at-point ()
  "Return the priority label for the entry at point, or nil.
Resolution order:
1. the `org-focus-switch-priority-property' property, inherited from
   ancestors when `org-focus-switch-priority-inherit' is non-nil;
2. Org's native priority cookie ([#A]) mapped through
   `org-focus-switch-cookie-priority-alist', when
   `org-focus-switch-use-priority-cookie' is non-nil.
Returns nil when neither source yields a value, which the analysis then
renders as the `org-focus-switch-none-label' node."
  (or (let ((value (org-entry-get (point)
                                  org-focus-switch-priority-property
                                  (and org-focus-switch-priority-inherit t))))
        (and value (not (string-empty-p value)) value))
      (and org-focus-switch-use-priority-cookie
           (let ((cookie (nth 3 (org-heading-components))))
             (and cookie
                  (cdr (assq cookie org-focus-switch-cookie-priority-alist)))))))

(defun org-focus-switch--has-children-p ()
  "Return non-nil when the entry at point has any child heading."
  (let ((level (org-current-level)))
    (save-excursion
      (outline-next-heading)
      (and (not (eobp))
           (org-current-level)
           (> (org-current-level) level)))))

;;;; Clock parsing

(defun org-focus-switch--parse-clock-line (line task priority)
  "Parse a closed CLOCK LINE into an event plist, or return nil.
TASK and PRIORITY tag the resulting event.  Only clocks with a `=> H:MM'
duration are parsed; open clocks return nil.  The event end time is the
start time plus the clocked duration.

The returned plist has the shape:
  (:start FLOAT :end FLOAT :task STRING :priority STRING-or-nil)
where the times are seconds since the epoch."
  (when (and (string-match
              "CLOCK:[ \t]*\\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}[^]]*\\)\\]"
              line)
             (let ((stamp (match-string 1 line)))
               (when (string-match "=>[ \t]+\\([0-9]+\\):\\([0-9]+\\)" line)
                 (let* ((minutes (+ (* 60 (string-to-number (match-string 1 line)))
                                    (string-to-number (match-string 2 line))))
                        (start (float-time (encode-time (org-parse-time-string stamp)))))
                   (setq line (list :start start
                                    :end (+ start (* 60.0 minutes))
                                    :task task
                                    :priority priority))))))
    line))

(defun org-focus-switch--entry-events ()
  "Return the list of clock event plists for the entry at point.
Child subtrees are excluded (leaf entries have none anyway)."
  (save-excursion
    (org-back-to-heading t)
    (let* ((task (org-focus-switch--title-at-point))
           (priority (org-focus-switch--priority-at-point))
           (end (save-excursion (org-end-of-subtree t t)))
           events)
      (forward-line 1)
      (while (re-search-forward "^[ \t]*CLOCK:" end t)
        (let ((event (org-focus-switch--parse-clock-line
                      (buffer-substring-no-properties (line-beginning-position)
                                                      (line-end-position))
                      task priority)))
          (when event (push event events))))
      (nreverse events))))

;;;; Priority ranking and edge classification

(defun org-focus-switch--prio-label (priority)
  "Return the display label for PRIORITY, mapping nil/empty to the none label."
  (if (and priority (not (string-empty-p priority)))
      priority
    org-focus-switch-none-label))

(defun org-focus-switch--rank (label)
  "Return the urgency rank of priority LABEL (lower is more urgent).
Listed priorities rank by position; unknown labels and the none label
rank below every listed priority."
  (or (cl-position label org-focus-switch-priorities :test #'equal)
      (length org-focus-switch-priorities)))

(defun org-focus-switch--direction (from to)
  "Classify a switch from priority label FROM to TO.
Returns `escalation' (moved to a more urgent level), `de-escalation'
\(moved to a less urgent level), or `lateral' (same urgency)."
  (let ((fr (org-focus-switch--rank from))
        (tr (org-focus-switch--rank to)))
    (cond ((< tr fr) 'escalation)
          ((> tr fr) 'de-escalation)
          (t 'lateral))))

;;;; Analysis (pure)

(defun org-focus-switch--event-date (event)
  "Return the local calendar date (\"YYYY-MM-DD\") of EVENT's start time."
  (format-time-string "%Y-%m-%d"
                      (seconds-to-time (plist-get event :start))))

(defun org-focus-switch--per-day (day-minutes day-switches)
  "Build the per-day distribution list from DAY-MINUTES and DAY-SWITCHES hashes.
Returns a list of plists (:date :switches :minutes :switches-per-hour),
one per day that has clocked time or switches, sorted by date ascending."
  (let ((dates (make-hash-table :test #'equal))
        keys)
    (maphash (lambda (date _) (puthash date t dates)) day-minutes)
    (maphash (lambda (date _) (puthash date t dates)) day-switches)
    (maphash (lambda (date _) (push date keys)) dates)
    (mapcar (lambda (date)
              (let ((minutes (gethash date day-minutes 0.0))
                    (switches (gethash date day-switches 0)))
                (list :date date
                      :switches switches
                      :minutes minutes
                      :switches-per-hour (if (> minutes 0)
                                             (/ switches (/ minutes 60.0))
                                           0.0))))
            (sort keys #'string<))))

(defun org-focus-switch-analyze (events)
  "Analyze EVENTS, a list of clock event plists, into a metrics plist.

Each event is (:start FLOAT :end FLOAT :task STRING :priority STRING-or-nil).
Events are sorted by start time; a task switch is a boundary between two
consecutive events with different tasks (subject to
`org-focus-switch-session-gap-minutes').

Returns a plist:
  (:events N                 count of clock events
   :total-minutes F          total clocked minutes across events
   :switches N               count of task switches
   :blocks N                 count of uninterrupted focus blocks
   :switches-per-hour F      switches divided by focused hours
   :avg-block-minutes F      total minutes divided by blocks
   :edges HASH               (FROM . TO) label pair -> switch count
   :escalations N            switches toward a more urgent priority
   :de-escalations N         switches toward a less urgent priority
   :laterals N               switches between equal priorities
   :per-day LIST             per-day distribution: plists (:date :switches
                             :minutes :switches-per-hour), date-ascending
   :order LIST)              priority labels in rank order, none last"
  (let* ((sorted (sort (copy-sequence events)
                       (lambda (a b) (< (plist-get a :start) (plist-get b :start)))))
         (edges (make-hash-table :test #'equal))
         (day-minutes (make-hash-table :test #'equal))
         (day-switches (make-hash-table :test #'equal))
         (gap-secs (and (numberp org-focus-switch-session-gap-minutes)
                        (> org-focus-switch-session-gap-minutes 0)
                        (* 60 org-focus-switch-session-gap-minutes)))
         (total 0.0)
         (switches 0)
         (blocks 0)
         (escalations 0)
         (de-escalations 0)
         (laterals 0)
         prev)
    (dolist (event sorted)
      (let ((date (org-focus-switch--event-date event))
            (minutes (/ (- (plist-get event :end) (plist-get event :start)) 60.0)))
        (cl-incf total minutes)
        (puthash date (+ minutes (gethash date day-minutes 0.0)) day-minutes)
        (if (null prev)
            (setq blocks 1)
          (let* ((gap (- (plist-get event :start) (plist-get prev :end)))
                 (session-break (and gap-secs (> gap gap-secs)))
                 (changed (not (equal (plist-get event :task) (plist-get prev :task)))))
            (cond
             (session-break
              ;; A break, not a deliberate switch, but a fresh focus block.
              (cl-incf blocks))
             (changed
              (cl-incf switches)
              (cl-incf blocks)
              ;; Attribute the switch to the day of the task switched *to*.
              (puthash date (1+ (gethash date day-switches 0)) day-switches)
              (let* ((from (org-focus-switch--prio-label (plist-get prev :priority)))
                     (to (org-focus-switch--prio-label (plist-get event :priority)))
                     (key (cons from to)))
                (puthash key (1+ (gethash key edges 0)) edges)
                (pcase (org-focus-switch--direction from to)
                  ('escalation (cl-incf escalations))
                  ('de-escalation (cl-incf de-escalations))
                  ('lateral (cl-incf laterals)))))))))
      (setq prev event))
    (list :events (length sorted)
          :total-minutes total
          :switches switches
          :blocks blocks
          :switches-per-hour (if (> total 0) (/ switches (/ total 60.0)) 0.0)
          :avg-block-minutes (if (> blocks 0) (/ total blocks) 0.0)
          :edges edges
          :escalations escalations
          :de-escalations de-escalations
          :laterals laterals
          :per-day (org-focus-switch--per-day day-minutes day-switches)
          :order (append org-focus-switch-priorities
                         (list org-focus-switch-none-label)))))

;;;; Collection over a scope

(defun org-focus-switch-collect (map-fn)
  "Collect clock events over a scope and return the analysis plist.
MAP-FN receives a per-entry callback and is responsible for mapping it
over the desired scope (a subtree, the buffer, or configured files),
exactly like the scope callbacks used by `org-focus'.  Leaf entries that
are not excluded contribute their clock events; the pooled events are
handed to `org-focus-switch-analyze'."
  (let (events)
    (funcall
     map-fn
     (lambda ()
       (unless (or (org-focus-switch--excluded-p)
                   (org-focus-switch--has-children-p))
         (setq events (nconc events (org-focus-switch--entry-events))))))
    (org-focus-switch-analyze events)))

;;;; Formatting helpers

(defun org-focus-switch--format-duration (minutes)
  "Format MINUTES as a compact \"Hh MMm\" duration string."
  (let* ((m (round minutes))
         (h (/ m 60))
         (mm (% m 60)))
    (cond ((<= m 0) "0m")
          ((= h 0) (format "%dm" mm))
          ((= mm 0) (format "%dh" h))
          (t (format "%dh %02dm" h mm)))))

(defun org-focus-switch--edge-items (edges)
  "Return EDGES as a list of ((FROM . TO) . COUNT), most frequent first."
  (let (items)
    (maphash (lambda (key count) (push (cons key count) items)) edges)
    (sort items (lambda (a b) (> (cdr a) (cdr b))))))

;;;; Rendering (embeddable)

(defun org-focus-switch--insert-matrix (order edges)
  "Insert the adjacency matrix for EDGES over priority labels ORDER."
  (insert (format "%-6s" ""))
  (dolist (to order)
    (insert (format "%7s" (concat "->" to))))
  (insert "\n")
  (dolist (from order)
    (insert (format "%-6s" from))
    (dolist (to order)
      (let ((count (gethash (cons from to) edges 0)))
        (insert (format "%7s" (if (> count 0) (number-to-string count) "·")))))
    (insert "\n")))

(defun org-focus-switch--render-summary (data)
  "Insert the switch-frequency conclusion for DATA (Conclusion 1)."
  (insert (format "%-20s %d\n" "Clock events:" (plist-get data :events)))
  (insert (format "%-20s %d  (%.1f /focused-h)\n"
                  "Task switches:" (plist-get data :switches)
                  (plist-get data :switches-per-hour)))
  (insert (format "%-20s %d  (avg %s each)\n"
                  "Focus blocks:" (plist-get data :blocks)
                  (org-focus-switch--format-duration
                   (plist-get data :avg-block-minutes))))
  (insert (format "%-20s %s\n"
                  "Clocked time:"
                  (org-focus-switch--format-duration
                   (plist-get data :total-minutes)))))

(defun org-focus-switch--render-per-day (data)
  "Insert the per-day switch-frequency distribution for DATA.
One row per day: the switch count, the rate per focused hour, and a bar
scaled to the busiest day so the shape of the distribution is visible."
  (insert (propertize "Switches per day\n" 'face 'bold))
  (let ((per-day (plist-get data :per-day)))
    (if (null per-day)
        (insert "  —\n")
      (let ((max-switches (apply #'max 1 (mapcar (lambda (d) (plist-get d :switches))
                                                 per-day))))
        (insert (format "  %-12s %8s %11s  %s\n"
                        "Date" "Switches" "/focused-h" "Distribution"))
        (dolist (day per-day)
          (let* ((switches (plist-get day :switches))
                 (bar-width (round (* 20.0 (/ (float switches) max-switches)))))
            (insert (format "  %-12s %8d %11.1f  %s\n"
                            (plist-get day :date)
                            switches
                            (plist-get day :switches-per-hour)
                            (make-string bar-width ?█))))))))
  (insert "\n"))

(defun org-focus-switch--render-direction-summary (data)
  "Insert the one-line direction tally for DATA."
  (insert (format "%-20s %d escalation(s), %d de-escalation(s), %d lateral\n"
                  "Direction:"
                  (plist-get data :escalations)
                  (plist-get data :de-escalations)
                  (plist-get data :laterals))))

(defun org-focus-switch--render-matrix-section (data)
  "Insert the \"Priority transitions\" adjacency matrix for DATA."
  (insert (propertize "Priority transitions (from -> to)\n" 'face 'bold))
  (org-focus-switch--insert-matrix (plist-get data :order) (plist-get data :edges)))

(defun org-focus-switch--render-groups (data)
  "Insert the switches grouped into three direction sections with counters.
Each of Lateral, Escalation and De-escalation is a header carrying its
switch count, followed by the edges in that group ordered by frequency."
  (let ((edges (org-focus-switch--graph-edges data)))
    (dolist (group (list (list 'lateral "Lateral" (plist-get data :laterals))
                         (list 'escalation "Escalation" (plist-get data :escalations))
                         (list 'de-escalation "De-escalation" (plist-get data :de-escalations))))
      (let* ((direction (nth 0 group))
             (label (nth 1 group))
             (count (nth 2 group))
             (members (cl-remove-if-not
                       (lambda (e) (eq (plist-get e :direction) direction))
                       edges)))
        (insert (propertize (format "%s (%d)\n" label count) 'face 'bold))
        (if (null members)
            (insert "  —\n")
          (dolist (e members)
            (insert (format "  %-4s -> %-4s  %3d\n"
                            (plist-get e :from)
                            (plist-get e :to)
                            (plist-get e :weight)))))
        (insert "\n")))))

(defun org-focus-switch-render (data)
  "Insert a compact \"Task Switching\" section for DATA at point.
Used to embed switch metrics in the `org-focus' dashboard: it shows the
frequency conclusion and the direction tally only.  The full
priority-transition graph (matrix and per-direction edges) lives in the
dedicated dashboard opened by \\[org-focus-switch]."
  (insert (propertize "Task Switching\n" 'face 'bold))
  (let ((events (plist-get data :events)))
    (if (or (null events) (= events 0))
        (insert "No clock events in scope.\n\n")
      (progn
        (org-focus-switch--render-summary data)
        (if (= (plist-get data :switches) 0)
            (insert "\nNo task switches: a single unbroken focus block.\n")
          (progn
            (insert "\n")
            (org-focus-switch--render-direction-summary data)
            (insert "\n")
            (org-focus-switch--render-per-day data)))
        (insert (propertize "Full transition graph: M-x org-focus-switch\n\n"
                            'face 'shadow)))))
  data)

(defun org-focus-switch--render-dashboard-content (data)
  "Insert the full dashboard body for DATA at point.
Both conclusions: the frequency summary, then the priority-transition
graph as an adjacency matrix and three direction-grouped sections."
  (let ((events (plist-get data :events)))
    (if (or (null events) (= events 0))
        (insert "No clock events in scope.\n\n")
      (progn
        (insert (propertize "Switch frequency\n" 'face 'bold))
        (org-focus-switch--render-summary data)
        (insert "\n")
        (org-focus-switch--render-per-day data)
        (if (= (plist-get data :switches) 0)
            (insert "No task switches: a single unbroken focus block.\n\n")
          (progn
            (org-focus-switch--render-matrix-section data)
            (insert "\n")
            (org-focus-switch--render-groups data)))))))

;;;; Graph export

;; The priority-transition graph is a directed weighted multigraph:
;; nodes are priority labels, each edge FROM -> TO carries the switch count as
;; its weight plus a direction attribute.  These serializers turn the analysis
;; data into standard, tool-consumable graph formats.

(defun org-focus-switch--graph-nodes (data)
  "Return the priority-label nodes that participate in DATA's edges.
Ordered by DATA's `:order' (rank order), with any unlisted labels last."
  (let ((present (make-hash-table :test #'equal))
        nodes)
    (maphash (lambda (key _)
               (puthash (car key) t present)
               (puthash (cdr key) t present))
             (plist-get data :edges))
    (dolist (label (plist-get data :order))
      (when (gethash label present)
        (push label nodes)
        (remhash label present)))
    (maphash (lambda (label _) (push label nodes)) present)
    (nreverse nodes)))

(defun org-focus-switch--graph-edges (data)
  "Return DATA's edges as plists (:from :to :weight :direction), by frequency."
  (mapcar (lambda (item)
            (let ((from (car (car item)))
                  (to (cdr (car item))))
              (list :from from :to to
                    :weight (cdr item)
                    :direction (org-focus-switch--direction from to))))
          (org-focus-switch--edge-items (plist-get data :edges))))

(defun org-focus-switch--dir-color (direction)
  "Return a Graphviz colour name for a switch DIRECTION symbol."
  (pcase direction
    ('escalation "firebrick")
    ('de-escalation "seagreen")
    (_ "gray50")))

(defun org-focus-switch--xml-escape (string)
  "Escape STRING for inclusion in XML text/attributes."
  (let ((s string))
    (setq s (replace-regexp-in-string "&" "&amp;" s t t))
    (setq s (replace-regexp-in-string "<" "&lt;" s t t))
    (setq s (replace-regexp-in-string ">" "&gt;" s t t))
    (setq s (replace-regexp-in-string "\"" "&quot;" s t t))
    s))

(defun org-focus-switch--to-dot (data)
  "Serialize DATA's transition graph as Graphviz DOT."
  (let ((nodes (org-focus-switch--graph-nodes data))
        (edges (org-focus-switch--graph-edges data)))
    (concat
     "digraph priority_transitions {\n"
     "  rankdir=LR;\n"
     "  node [shape=box, style=rounded];\n"
     (mapconcat (lambda (n) (format "  %S;\n" n)) nodes "")
     (mapconcat (lambda (e)
                  (format "  %S -> %S [label=%S, penwidth=%d, color=%S];\n"
                          (plist-get e :from)
                          (plist-get e :to)
                          (number-to-string (plist-get e :weight))
                          (plist-get e :weight)
                          (org-focus-switch--dir-color (plist-get e :direction))))
                edges "")
     "}\n")))

(defun org-focus-switch--to-mermaid (data)
  "Serialize DATA's transition graph as a Mermaid flowchart."
  (let ((edges (org-focus-switch--graph-edges data)))
    (concat
     "flowchart LR\n"
     (mapconcat (lambda (e)
                  (format "  %s -->|%d| %s"
                          (plist-get e :from)
                          (plist-get e :weight)
                          (plist-get e :to)))
                edges "\n")
     "\n")))

(defun org-focus-switch--to-graphml (data)
  "Serialize DATA's transition graph as GraphML."
  (let ((nodes (org-focus-switch--graph-nodes data))
        (edges (org-focus-switch--graph-edges data)))
    (concat
     "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
     "<graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\">\n"
     "  <key id=\"weight\" for=\"edge\" attr.name=\"weight\" attr.type=\"int\"/>\n"
     "  <key id=\"direction\" for=\"edge\" attr.name=\"direction\" attr.type=\"string\"/>\n"
     "  <graph id=\"priority_transitions\" edgedefault=\"directed\">\n"
     (mapconcat (lambda (n)
                  (format "    <node id=\"%s\"/>\n"
                          (org-focus-switch--xml-escape n)))
                nodes "")
     (mapconcat (lambda (e)
                  (format (concat "    <edge source=\"%s\" target=\"%s\">\n"
                                  "      <data key=\"weight\">%d</data>\n"
                                  "      <data key=\"direction\">%s</data>\n"
                                  "    </edge>\n")
                          (org-focus-switch--xml-escape (plist-get e :from))
                          (org-focus-switch--xml-escape (plist-get e :to))
                          (plist-get e :weight)
                          (symbol-name (plist-get e :direction))))
                edges "")
     "  </graph>\n"
     "</graphml>\n")))

(defun org-focus-switch--to-csv (data)
  "Serialize DATA's transition graph as a CSV edge list."
  (concat
   "from,to,weight,direction\n"
   (mapconcat (lambda (e)
                (format "%s,%s,%d,%s"
                        (plist-get e :from)
                        (plist-get e :to)
                        (plist-get e :weight)
                        (symbol-name (plist-get e :direction))))
              (org-focus-switch--graph-edges data)
              "\n")
   "\n"))

(defun org-focus-switch--to-json (data)
  "Serialize DATA's transition graph and headline metrics as JSON."
  (let* ((json-encoding-pretty-print t)
         (nodes (org-focus-switch--graph-nodes data))
         (edges (mapcar (lambda (e)
                          `(("from" . ,(plist-get e :from))
                            ("to" . ,(plist-get e :to))
                            ("weight" . ,(plist-get e :weight))
                            ("direction" . ,(symbol-name (plist-get e :direction)))))
                        (org-focus-switch--graph-edges data)))
         (payload
          `(("directed" . t)
            ("nodes" . ,(vconcat nodes))
            ("edges" . ,(vconcat edges))
            ("metrics"
             . (("events" . ,(plist-get data :events))
                ("switches" . ,(plist-get data :switches))
                ("blocks" . ,(plist-get data :blocks))
                ("switchesPerHour" . ,(plist-get data :switches-per-hour))
                ("avgBlockMinutes" . ,(plist-get data :avg-block-minutes))
                ("totalMinutes" . ,(plist-get data :total-minutes))
                ("escalations" . ,(plist-get data :escalations))
                ("deEscalations" . ,(plist-get data :de-escalations))
                ("laterals" . ,(plist-get data :laterals)))))))
    (concat (json-encode payload) "\n")))

(defun org-focus-switch-to-format (data format)
  "Return DATA's transition graph serialized to FORMAT.
FORMAT is one of the symbols in `org-focus-switch-export-formats'
\(`dot', `mermaid', `graphml', `csv', `json')."
  (pcase format
    ('dot (org-focus-switch--to-dot data))
    ('mermaid (org-focus-switch--to-mermaid data))
    ('graphml (org-focus-switch--to-graphml data))
    ('csv (org-focus-switch--to-csv data))
    ('json (org-focus-switch--to-json data))
    (_ (user-error "Unknown export format: %s" format))))

(defun org-focus-switch--format-extension (format)
  "Return a file extension string for graph export FORMAT."
  (pcase format
    ('dot "dot")
    ('mermaid "mmd")
    ('graphml "graphml")
    ('csv "csv")
    ('json "json")
    (_ "txt")))

(defvar-local org-focus-switch--dashboard-data nil
  "Analysis data backing the current Org Focus Switch dashboard buffer.
Set by `org-focus-switch--render-dashboard'; the sole data source for
`org-focus-switch-export', which is why export works only in the dashboard.")

(defun org-focus-switch-export ()
  "Export the dashboard's priority-transition graph in a chosen format.
Only works inside the Org Focus Switch dashboard (\\[org-focus-switch]),
operating on the graph it is currently showing.  Prompts for one of
`org-focus-switch-export-formats' (DOT, Mermaid, GraphML, CSV, JSON),
renders it into `org-focus-switch-export-buffer-name', and offers to
write it to a file."
  (interactive)
  (let ((data org-focus-switch--dashboard-data))
    (unless data
      (user-error "Export is only available in the Org Focus Switch dashboard (M-x org-focus-switch)"))
    (let* ((name (completing-read "Export graph as: "
                                  (mapcar #'car org-focus-switch-export-formats)
                                  nil t))
           (format (cdr (assoc name org-focus-switch-export-formats)))
           (text (org-focus-switch-to-format data format))
           (buf (get-buffer-create org-focus-switch-export-buffer-name)))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (fundamental-mode)
          (insert text)
          (goto-char (point-min))))
      (pop-to-buffer buf)
      (when (y-or-n-p (format "Write %s export to a file? " name))
        (let ((file (read-file-name
                     "Write to: " nil nil nil
                     (format "priority-transitions.%s"
                             (org-focus-switch--format-extension format)))))
          (with-temp-file file (insert text))
          (message "Wrote %s graph to %s" name file))))))

;;;; Dashboard command

(defvar org-focus-switch-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "e") #'org-focus-switch-export)
    (define-key map (kbd "g") #'org-focus-switch-export)
    map)
  "Keymap for the Org Focus Switch dashboard buffer.")

(defun org-focus-switch--render-dashboard (data scope-label)
  "Render DATA into the dashboard buffer, noting SCOPE-LABEL.
Stashes DATA buffer-locally so `org-focus-switch-export' can act on it."
  (let ((buf (get-buffer-create org-focus-switch-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (special-mode)
        (use-local-map (make-composed-keymap org-focus-switch-dashboard-mode-map
                                             (current-local-map)))
        (setq org-focus-switch--dashboard-data data)
        (insert (propertize "Org Focus Switch Dashboard"
                            'face '(:height 1.3 :weight bold)))
        (insert (format " (%s)\n\n" scope-label))
        (org-focus-switch--render-dashboard-content data)
        (insert (propertize
                 "Press e to export the transition graph (DOT/Mermaid/GraphML/CSV/JSON).\n"
                 'face 'shadow))
        (goto-char (point-min))))
    (pop-to-buffer buf)))

;;;###autoload
(defun org-focus-switch (&optional arg)
  "Open the Org Focus Switch dashboard for the subtree at point.
With a prefix ARG, analyze the whole current buffer instead.  The
read-only dashboard shows the switch frequency, the per-day switch
distribution, and the priority-transition graph, and offers graph export
via `e'."
  (interactive "P")
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (let* ((scope (if arg nil 'tree))
         (label (if arg "buffer" "subtree"))
         (data (save-excursion
                 (unless arg (org-back-to-heading t))
                 (org-focus-switch-collect
                  (lambda (fn)
                    (org-with-wide-buffer (org-map-entries fn nil scope)))))))
    (org-focus-switch--render-dashboard data label)))

;;;###autoload
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c t s") #'org-focus-switch))

(provide 'org-focus-switch)

;;; org-focus-switch.el ends here
