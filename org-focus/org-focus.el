;;; org-focus.el --- Focus tracking, linting, and dashboard for Org mode -*- lexical-binding: t; -*-

;; Author: Radoslaw Cieciwa
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1") (org "9.6"))
;; Keywords: outlines, time, convenience
;; URL: https://github.com/radekcieciwa/emacs_tools

;;; Commentary:
;;
;; A small Org-mode helper package for:
;; - enforcing a lightweight metadata taxonomy on clock-in
;; - linting entries for missing/conflicting metadata
;; - rendering a focus dashboard with dual-scope (subtree / global) modes
;;
;; Dual-scope operation:
;; - Subtree scope: measure only the heading at point and its descendants.
;; - Global scope:  measure all entries in the configured files.
;;
;; There is no week filtering: the dashboard sums every clock in the measured
;; scope.  You control which slice to measure by where you put the cursor
;; (e.g. a Week heading, a Day heading, a Project heading).
;;
;; Recommended taxonomy:
;;
;; Required axes for clocked/active work
;; - domain:   prod | team | org
;; - activity: build | help | ops | sync
;; - intent:   plan | unplan
;;
;; Optional axes
;; - invest   (tags investment/learning/exploration work)
;; - priority (FOCUS_PRIORITY property: P0, P1, P2; P0 is "urgent")
;;
;; Example entry:
;;
;; * TODO Investigate rendering regression :prod:build:plan:
;;   :PROPERTIES:
;;   :EFFORT:   2:00
;;   :FOCUS_PRIORITY: P0
;;   :END:
;;
;; Install:
;;   1. Put this file's directory on `load-path'.
;;   2. (require 'org-focus)
;;   3. (org-focus-mode 1)
;;
;; Usage:
;;   Position the cursor on a heading (Week, Day, Project, etc.) and run:
;;   - C-c f d: dashboard for that subtree
;;   - C-u C-c f d: dashboard for all entries
;;   - C-c f l: lint for that subtree
;;   - C-u C-c f l: lint for all entries
;;   - C-c f f: fix metadata on the current entry
;;
;; Notes:
;; - This package is intentionally conservative: it warns and offers fixes,
;;   but it does not hard-block editing.
;; - Entries with the :private: tag are excluded from all operations.
;; - Experimental/diagnostic tooling lives under experimental/; see
;;   experimental/TESTING.md.

;;; Code:

(require 'org)
(require 'org-clock)
(require 'cl-lib)
(require 'subr-x)
(require 'button)
(require 'time-date)

(defgroup org-focus nil
  "Focus tracking and review helpers for Org mode."
  :group 'org)

;;;; Taxonomy

(defcustom org-focus-domain-tags '("prod" "team" "org")
  "Allowed domain tags.  Exactly one is recommended for clocked work."
  :type '(repeat string)
  :group 'org-focus)

(defcustom org-focus-activity-tags '("build" "help" "ops" "sync")
  "Allowed activity tags.  Exactly one is recommended for clocked work."
  :type '(repeat string)
  :group 'org-focus)

(defcustom org-focus-intent-tags '("plan" "unplan")
  "Allowed intentionality tags.  Exactly one is recommended for clocked work."
  :type '(repeat string)
  :group 'org-focus)

(defcustom org-focus-invest-tag "invest"
  "Tag used to indicate investment work."
  :type 'string
  :group 'org-focus)

(defcustom org-focus-exclude-tags '("private")
  "Tags that exclude entries from focus tracking."
  :type '(repeat string)
  :group 'org-focus)

(defcustom org-focus-priorities '("P0" "P1" "P2")
  "Allowed custom focus priority levels.  Stored in FOCUS_PRIORITY property.
The first element is treated as the \"urgent\" level for warnings."
  :type '(repeat string)
  :group 'org-focus)

;;;; Thresholds

(defcustom org-focus-max-active-p0 3
  "Warn when more than this many active urgent (P0) tasks exist."
  :type 'integer
  :group 'org-focus)

(defcustom org-focus-stale-p0-days 3
  "Warn when an urgent (P0) task has not been clocked in this many days."
  :type 'integer
  :group 'org-focus)

(defcustom org-focus-investment-target-ratio 0.15
  "Desired lower bound for the investment time ratio."
  :type 'float
  :group 'org-focus)

(defcustom org-focus-unplanned-warning-ratio 0.30
  "Warn if the unplanned time ratio exceeds this threshold."
  :type 'float
  :group 'org-focus)

(defcustom org-focus-sync-warning-ratio 0.25
  "Warn if the sync/meeting time ratio exceeds this threshold."
  :type 'float
  :group 'org-focus)

(defcustom org-focus-help-warning-ratio 0.30
  "Warn if the help/support time ratio exceeds this threshold."
  :type 'float
  :group 'org-focus)

;;;; Scanning and behaviour

(defcustom org-focus-files nil
  "Files to scan for dashboard/linting.
When nil, use `org-agenda-files'."
  :type '(repeat file)
  :group 'org-focus)

(defcustom org-focus-current-file-only nil
  "If non-nil, scan only the current buffer's file for dashboard/linting."
  :type 'boolean
  :group 'org-focus)

(defcustom org-focus-enforce-on-clock-in nil
  "If non-nil, automatically run `org-focus-fix-current-entry' on clock-in.
If nil, metadata fixing is available only via the manual `C-c f f' command."
  :type 'boolean
  :group 'org-focus)

(defcustom org-focus-prompt-for-invest nil
  "If non-nil, ask on clock-in whether the work is investment.
If nil, investment stays sparse and is only recorded when explicitly tagged."
  :type 'boolean
  :group 'org-focus)

(defcustom org-focus-dashboard-buffer-name "*Org Focus Dashboard*"
  "Dashboard buffer name."
  :type 'string
  :group 'org-focus)

(defcustom org-focus-lint-buffer-name "*Org Focus Lint*"
  "Lint buffer name."
  :type 'string
  :group 'org-focus)

(defvar org-focus-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c f d") #'org-focus-dashboard)
    (define-key map (kbd "C-c f l") #'org-focus-lint)
    (define-key map (kbd "C-c f f") #'org-focus-fix-current-entry)
    map)
  "Keymap for `org-focus-mode'.")

;;;; Small predicates and accessors

(defun org-focus--files ()
  "Return the list of files to scan."
  (cond
   (org-focus-current-file-only
    (when (buffer-file-name) (list (buffer-file-name))))
   (org-focus-files
    org-focus-files)
   ((org-agenda-files t))
   ((buffer-file-name)
    (list (buffer-file-name)))
   (t nil)))

(defun org-focus--in-org-buffer-p ()
  "Return non-nil when the current buffer is an Org buffer."
  (derived-mode-p 'org-mode))

(defun org-focus--all-tags-at-point ()
  "Return all inherited tags at point."
  (org-get-tags nil t))

(defun org-focus--member-count (candidates tags)
  "Count how many of CANDIDATES exist in TAGS."
  (cl-count-if (lambda (tag) (member tag candidates)) tags))

(defun org-focus--present-tags (candidates tags)
  "Return all CANDIDATES present in TAGS."
  (cl-remove-if-not (lambda (tag) (member tag tags)) candidates))

(defun org-focus--has-child-headings-p ()
  "Return non-nil if the current entry has any child headings."
  (let ((current-level (org-current-level)))
    (save-excursion
      (outline-next-heading)
      (and (not (eobp))
           (> (org-current-level) current-level)))))

(defun org-focus--excluded-p ()
  "Return non-nil if the current entry should be excluded from tracking."
  (let ((tags (org-focus--all-tags-at-point)))
    (cl-some (lambda (tag) (member tag tags)) org-focus-exclude-tags)))

(defun org-focus--todo-done-p ()
  "Return non-nil if the current heading is in a done state."
  (member (org-get-todo-state) org-done-keywords))

(defun org-focus--get-priority ()
  "Return the FOCUS_PRIORITY property value at point, or nil."
  (org-entry-get (point) "FOCUS_PRIORITY"))

(defun org-focus--set-priority (priority)
  "Set the FOCUS_PRIORITY property at point to PRIORITY string."
  (org-set-property "FOCUS_PRIORITY" priority))

(defun org-focus--heading ()
  "Return the current heading text without tags/todo/priority."
  (org-get-heading t t t t))

(defun org-focus--urgent-priority ()
  "Return the urgent priority level (the first of `org-focus-priorities')."
  (car org-focus-priorities))

(defun org-focus--active-p0-p ()
  "Return non-nil if the current entry is an active urgent (P0) task."
  (and (equal (org-focus--get-priority) (org-focus--urgent-priority))
       (org-get-todo-state)
       (not (org-focus--todo-done-p))))

(defun org-focus--choose (prompt choices)
  "Prompt with PROMPT for one of CHOICES.
CHOICES is a list of (LABEL . KEY).  Returns the selected label."
  (let* ((msg (concat prompt " "
                      (mapconcat (lambda (c)
                                   (format "[%c]%s" (cdr c) (car c)))
                                 choices
                                 " ")))
         (keys (mapcar #'cdr choices))
         (key (read-key msg)))
    (while (not (memq key keys))
      (setq key (read-key (format "%s (invalid, try again)" msg))))
    (car (rassoc key choices))))

;;;; Clock parsing

(defun org-focus--clocked-entry-p ()
  "Return non-nil if the current entry has any CLOCK line."
  (save-excursion
    (org-back-to-heading t)
    (let ((end (save-excursion (org-end-of-subtree t t))))
      (re-search-forward org-clock-line-re end t))))

(defun org-focus--timestamp-within-days-p (timestamp days)
  "Return non-nil if TIMESTAMP is within DAYS of now.
TIMESTAMP should be an Emacs time value."
  (let* ((now (current-time))
         (delta (float-time (time-subtract now timestamp))))
    (<= delta (* days 24 60 60))))

(defun org-focus--week-start-end ()
  "Return (START END) time values for the current week, Monday to next Monday.
Times are local midnight boundaries.  Retained as a building block for
future week-scoped views; the dashboard itself does not filter by week."
  (let* ((now (decode-time (current-time)))
         (day (nth 3 now))
         (mon (nth 4 now))
         (year (nth 5 now))
         (dow (nth 6 now)) ; 0 Sun .. 6 Sat
         (days-since-monday (if (= dow 0) 6 (1- dow)))
         (today-midnight (encode-time 0 0 0 day mon year))
         (week-start (time-subtract today-midnight (days-to-time days-since-monday)))
         (week-end (time-add week-start (days-to-time 7))))
    (list week-start week-end)))

(defun org-focus--format-hours (minutes)
  "Return MINUTES formatted as a decimal-hours string."
  (format "%.1fh" (/ minutes 60.0)))

(defun org-focus--format-ratio (part total)
  "Format PART/TOTAL as a percentage string."
  (if (<= total 0)
      "0%"
    (format "%.0f%%" (* 100.0 (/ (float part) total)))))

(defun org-focus--org-files-map-entries (fn &optional match)
  "Run FN for each entry in the configured files matching MATCH.
FN is called with point on each heading."
  (dolist (file (org-focus--files))
    (when (and file (file-exists-p file))
      (with-current-buffer (find-file-noselect file t)
        (org-with-wide-buffer
         (org-map-entries fn match 'file))))))

(defun org-focus--last-clock-time ()
  "Return the latest clock end time for the current entry, or nil.
Open clocks count as now."
  (save-excursion
    (org-back-to-heading t)
    (let ((end (save-excursion (org-end-of-subtree t t)))
          latest)
      (while (re-search-forward org-clock-line-re end t)
        (let* ((line (match-string 0))
               (closed (string-match "=>" line))
               (times (ignore-errors (org-clock-special-range line))))
          (cond
           ((and (consp times) (nth 1 times))
            (let ((t2 (nth 1 times)))
              (when (or (null latest) (time-less-p latest t2))
                (setq latest t2))))
           ((and (consp times) (nth 0 times) (not closed))
            (setq latest (current-time))))))
      latest)))

(defun org-focus--sum-clocks-for-entry ()
  "Sum all clock minutes in the current entry (excluding child headings)."
  (let ((total 0))
    (save-excursion
      (org-back-to-heading t)
      (let* ((current-level (org-current-level))
             (heading-end (progn (end-of-line) (point)))
             (child-start (save-excursion
                            (forward-line 1)
                            (and (re-search-forward "^\\*+ " nil t)
                                 (let ((line-level (1- (length (match-string 0)))))
                                   (when (<= line-level current-level)
                                     (match-beginning 0)))))))
        (goto-char heading-end)
        (forward-line 1)
        (while (and (< (point) (point-max))
                    (or (not child-start) (< (point) child-start)))
          (when (looking-at "^[ \t]*CLOCK:")
            (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
              (when (string-match "=>[ \t]+\\([0-9]+\\):\\([0-9]+\\)" line)
                (let ((hours (string-to-number (match-string 1 line)))
                      (mins (string-to-number (match-string 2 line))))
                  (setq total (+ total (* 60 hours) mins))))))
          (forward-line 1))))
    total))

(defun org-focus--collect-clocks-for-entry ()
  "Collect clock minutes and metadata snapshot for the current entry.
Returns a plist with :minutes and metadata fields."
  (list :minutes (org-focus--sum-clocks-for-entry)
        :tags (org-focus--all-tags-at-point)
        :priority (org-focus--get-priority)
        :todo (org-get-todo-state)
        :heading (org-focus--heading)
        :marker (copy-marker (point))
        :file (buffer-file-name)))

;;;; Linting

(defun org-focus--entry-issues (&optional require-for-clocked-only)
  "Return a list of metadata issues for the current entry.
If REQUIRE-FOR-CLOCKED-ONLY is non-nil, only require full metadata for
clocked or active entries."
  (let* ((tags (org-focus--all-tags-at-point))
         (domain-present (org-focus--present-tags org-focus-domain-tags tags))
         (activity-present (org-focus--present-tags org-focus-activity-tags tags))
         (intent-present (org-focus--present-tags org-focus-intent-tags tags))
         (clocked (org-focus--clocked-entry-p))
         (active (and (org-get-todo-state) (not (org-focus--todo-done-p))))
         (must-check (or (not require-for-clocked-only) clocked active))
         issues)
    (when (> (length domain-present) 1)
      (push (format "Multiple domain tags: %s" (string-join domain-present ", ")) issues))
    (when (> (length activity-present) 1)
      (push (format "Multiple activity tags: %s" (string-join activity-present ", ")) issues))
    (when (> (length intent-present) 1)
      (push (format "Multiple intent tags: %s" (string-join intent-present ", ")) issues))
    (when must-check
      (when (= (length domain-present) 0)
        (push "Missing domain tag" issues))
      (when (= (length activity-present) 0)
        (push "Missing activity tag" issues))
      (when (= (length intent-present) 0)
        (push "Missing intentionality tag" issues)))
    (nreverse issues)))

;;;; Interactive metadata fixing

(defun org-focus--fix-axis (axis choices)
  "Ensure the current entry has exactly one tag from AXIS using CHOICES.
CHOICES is a list of (TAG . KEY)."
  (let* ((tags (org-focus--all-tags-at-point))
         (axis-tags (mapcar #'car choices))
         (present (org-focus--present-tags axis-tags tags)))
    (unless (= (length present) 1)
      (dolist (tag axis-tags)
        (when (member tag tags)
          (org-toggle-tag tag 'off)))
      (org-toggle-tag (org-focus--choose (format "%s:" axis) choices) 'on))))

(defun org-focus-fix-current-entry ()
  "Interactively fix metadata on the current entry."
  (interactive)
  (unless (org-focus--in-org-buffer-p)
    (user-error "Not in an Org buffer"))
  (save-excursion
    (org-back-to-heading t)
    (org-focus--fix-axis "Domain" '(("prod" . ?p) ("team" . ?t) ("org" . ?o)))
    (org-focus--fix-axis "Activity" '(("build" . ?b) ("help" . ?h) ("ops" . ?a) ("sync" . ?m)))
    (org-focus--fix-axis "Intentionality" '(("plan" . ?l) ("unplan" . ?u)))
    (when org-focus-prompt-for-invest
      (let ((tags (org-focus--all-tags-at-point)))
        (unless (member org-focus-invest-tag tags)
          (when (y-or-n-p "Mark as investment? ")
            (org-toggle-tag org-focus-invest-tag 'on)))))
    (save-buffer)
    (message "Org focus metadata fixed for: %s" (org-focus--heading))))

(defun org-focus--ensure-on-clock-in ()
  "Hook to enforce/fix metadata when clocking in, if configured."
  (when (and org-focus-enforce-on-clock-in (org-focus--in-org-buffer-p))
    (save-excursion
      (org-back-to-heading t)
      (org-focus-fix-current-entry))))

;;;; Data collection

(defun org-focus--aggregate (rows lint-rows active-p0 stale-p0)
  "Build the dashboard data plist from collected scan results.
ROWS is a list of clock plists, LINT-ROWS the lint findings, ACTIVE-P0
the count of active urgent tasks, and STALE-P0 the list of stale urgent
task plists."
  (let ((by-domain (make-hash-table :test #'equal))
        (by-activity (make-hash-table :test #'equal))
        (by-intent (make-hash-table :test #'equal))
        (by-priority (make-hash-table :test #'equal))
        (invest-minutes 0)
        (known-metadata-minutes 0)
        (total-minutes 0))
    (dolist (row rows)
      (let* ((minutes (plist-get row :minutes))
             (tags (plist-get row :tags))
             (priority (plist-get row :priority))
             (domain (car (org-focus--present-tags org-focus-domain-tags tags)))
             (activity (car (org-focus--present-tags org-focus-activity-tags tags)))
             (intent (car (org-focus--present-tags org-focus-intent-tags tags))))
        (cl-incf total-minutes minutes)
        (when domain
          (puthash domain (+ minutes (gethash domain by-domain 0)) by-domain))
        (when activity
          (puthash activity (+ minutes (gethash activity by-activity 0)) by-activity))
        (when intent
          (puthash intent (+ minutes (gethash intent by-intent 0)) by-intent))
        (when priority
          (puthash priority (+ minutes (gethash priority by-priority 0)) by-priority))
        (when (member org-focus-invest-tag tags)
          (cl-incf invest-minutes minutes))
        (when (and domain activity intent)
          (cl-incf known-metadata-minutes minutes))))
    (list :rows rows
          :lint lint-rows
          :total total-minutes
          :known known-metadata-minutes
          :invest invest-minutes
          :active-a active-p0
          :stale-a stale-p0
          :by-domain by-domain
          :by-activity by-activity
          :by-intent by-intent
          :by-priority by-priority)))

(defun org-focus--collect (map-fn)
  "Collect focus data over a scope and return the dashboard data plist.
MAP-FN receives a per-entry callback and is responsible for mapping it
over the desired scope (current subtree or all configured files).

Only leaf (childless) entries are measured; parent headings act purely
as structure.  Excluded entries (see `org-focus-exclude-tags') are
skipped entirely."
  (let ((rows '())
        (lint-rows '())
        (active-p0 0)
        (stale-p0 '()))
    (funcall
     map-fn
     (lambda ()
       (unless (or (org-focus--excluded-p)
                   (org-focus--has-child-headings-p))
         ;; Lint findings (no-op for entries that are neither clocked nor active).
         (let ((issues (org-focus--entry-issues t)))
           (when issues
             (push (list :heading (org-focus--heading)
                         :issues issues
                         :marker (copy-marker (point))
                         :file (buffer-file-name))
                   lint-rows)))
         ;; Clock totals.
         (when (org-focus--clocked-entry-p)
           (let ((row (org-focus--collect-clocks-for-entry)))
             (when (> (plist-get row :minutes) 0)
               (push row rows))))
         ;; Urgent (P0) tracking, clocked or not.
         (when (org-focus--active-p0-p)
           (cl-incf active-p0)
           (let ((last (org-focus--last-clock-time)))
             (when (or (null last)
                       (not (org-focus--timestamp-within-days-p
                             last org-focus-stale-p0-days)))
               (push (list :heading (org-focus--heading)
                           :marker (copy-marker (point))
                           :file (buffer-file-name)
                           :last-clock last)
                     stale-p0)))))))
    (org-focus--aggregate rows (nreverse lint-rows) active-p0 (nreverse stale-p0))))

(defun org-focus--collect-subtree-data ()
  "Collect focus data for the subtree at point."
  (unless (org-focus--in-org-buffer-p)
    (user-error "Not in an Org buffer"))
  (org-back-to-heading t)
  (org-focus--collect
   (lambda (fn) (org-with-wide-buffer (org-map-entries fn nil 'tree)))))

(defun org-focus--collect-global-data ()
  "Collect focus data from all configured files."
  (org-focus--collect #'org-focus--org-files-map-entries))

(defun org-focus--group-rows (rows)
  "Group ROWS by heading, summing minutes and counting occurrences.
Returns a list of plists (:heading :minutes :count :priority :marker :file)
sorted by :minutes in descending order.  The marker, file, and priority are
taken from the first-seen occurrence of each heading."
  (let ((table (make-hash-table :test #'equal))
        (order '()))
    (dolist (row rows)
      (let* ((heading (plist-get row :heading))
             (existing (gethash heading table)))
        (if existing
            (puthash heading
                     (plist-put
                      (plist-put existing :minutes
                                 (+ (plist-get existing :minutes)
                                    (plist-get row :minutes)))
                      :count (1+ (plist-get existing :count)))
                     table)
          (puthash heading
                   (list :heading heading
                         :minutes (plist-get row :minutes)
                         :count 1
                         :priority (plist-get row :priority)
                         :marker (plist-get row :marker)
                         :file (plist-get row :file))
                   table)
          (push heading order))))
    (sort (mapcar (lambda (h) (gethash h table)) (nreverse order))
          (lambda (a b) (> (plist-get a :minutes) (plist-get b :minutes))))))

;;;; Rendering helpers

(defun org-focus--insert-section-title (title)
  "Insert TITLE as a section header."
  (insert (propertize title 'face 'bold) "\n"))

(defun org-focus--insert-kv-line (label value &optional face)
  "Insert LABEL and VALUE, applying optional FACE to VALUE."
  (insert (format "%-18s %s\n" (concat label ":")
                  (if face (propertize value 'face face) value))))

(defun org-focus--insert-table-from-hash (title table total order)
  "Insert TITLE and rows from TABLE using TOTAL and ORDER.
ORDER is the list of keys to print."
  (org-focus--insert-section-title title)
  (dolist (key order)
    (let ((minutes (gethash key table 0)))
      (org-focus--insert-kv-line
       (capitalize key)
       (format "%s  (%s)"
               (org-focus--format-hours minutes)
               (org-focus--format-ratio minutes total)))))
  (insert "\n"))

(defun org-focus--insert-button-line (label marker file &optional suffix)
  "Insert clickable LABEL jumping to MARKER in FILE, with optional SUFFIX."
  (insert-text-button
   label
   'action (lambda (_)
             (find-file file)
             (goto-char marker)
             (org-fold-show-context)
             (org-fold-show-entry))
   'follow-link t)
  (when suffix
    (insert suffix))
  (insert "\n"))

(defun org-focus--warning-lines (data)
  "Return a list of warning strings derived from DATA."
  (let* ((total (plist-get data :total))
         (lint (plist-get data :lint))
         (invest (plist-get data :invest))
         (active-a (or (plist-get data :active-a) 0))
         (stale-a (plist-get data :stale-a))
         (help (gethash "help" (plist-get data :by-activity) 0))
         (sync (gethash "sync" (plist-get data :by-activity) 0))
         (unplan (gethash "unplan" (plist-get data :by-intent) 0))
         warnings)
    (when (> active-a org-focus-max-active-p0)
      (push (format "Too many active P0 (urgent) tasks: %d (max %d)"
                    active-a org-focus-max-active-p0)
            warnings))
    (when stale-a
      (push (format "Stale P0 tasks (not clocked in %d days): %d"
                    org-focus-stale-p0-days (length stale-a))
            warnings))
    (when lint
      (push (format "Entries with metadata issues: %d" (length lint)) warnings))
    (when (and (> total 0)
               (< (/ (float invest) total) org-focus-investment-target-ratio))
      (push (format "Investment below target: %s vs target %.0f%%"
                    (org-focus--format-ratio invest total)
                    (* 100 org-focus-investment-target-ratio))
            warnings))
    (when (and (> total 0)
               (> (/ (float unplan) total) org-focus-unplanned-warning-ratio))
      (push (format "Unplanned work high: %s"
                    (org-focus--format-ratio unplan total))
            warnings))
    (when (and (> total 0)
               (> (/ (float sync) total) org-focus-sync-warning-ratio))
      (push (format "Sync/meeting time high: %s"
                    (org-focus--format-ratio sync total))
            warnings))
    (when (and (> total 0)
               (> (/ (float help) total) org-focus-help-warning-ratio))
      (push (format "Help/support time high: %s"
                    (org-focus--format-ratio help total))
            warnings))
    (nreverse warnings)))

;;;; Dashboard

(defun org-focus--render-dashboard (data title &optional scope-label)
  "Render dashboard content for DATA with TITLE.
If SCOPE-LABEL is provided, show it in the header."
  (let* ((buf (get-buffer-create org-focus-dashboard-buffer-name))
         (total (plist-get data :total))
         (known (plist-get data :known))
         (invest (plist-get data :invest))
         (active-a (or (plist-get data :active-a) 0))
         (stale-a (plist-get data :stale-a))
         (warnings (org-focus--warning-lines data))
         (lint (plist-get data :lint))
         (rows (plist-get data :rows)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (special-mode)
        (insert (propertize title 'face '(:height 1.3 :weight bold)))
        (when scope-label
          (insert (format " (%s)" scope-label)))
        (insert "\n\n")

        (org-focus--insert-section-title "Summary")
        (org-focus--insert-kv-line "Total clocked" (org-focus--format-hours total))
        (org-focus--insert-kv-line "Metadata complete"
                                   (format "%s  (%s)"
                                           (org-focus--format-hours known)
                                           (org-focus--format-ratio known total))
                                   (if (= known total) 'success 'warning))
        (org-focus--insert-kv-line "Investment"
                                   (format "%s  (%s)"
                                           (org-focus--format-hours invest)
                                           (org-focus--format-ratio invest total))
                                   (if (and (> total 0)
                                            (< (/ (float invest) total) org-focus-investment-target-ratio))
                                       'warning
                                     'success))
        (insert "\n")

        (org-focus--insert-table-from-hash
         "Domain" (plist-get data :by-domain) total org-focus-domain-tags)
        (org-focus--insert-table-from-hash
         "Activity" (plist-get data :by-activity) total org-focus-activity-tags)
        (org-focus--insert-table-from-hash
         "Intentionality" (plist-get data :by-intent) total org-focus-intent-tags)

        (org-focus--insert-section-title "Time by Priority")
        (let ((by-priority (plist-get data :by-priority)))
          (if (> (hash-table-count by-priority) 0)
              (dolist (priority org-focus-priorities)
                (let ((mins (gethash priority by-priority 0)))
                  (when (> mins 0)
                    (insert (format "%s: %s\n" priority (org-focus--format-hours mins))))))
            (insert "No prioritized entries.\n")))
        (insert "\n")

        (org-focus--insert-section-title "Urgent (P0)")
        (org-focus--insert-kv-line
         "Active" (number-to-string active-a)
         (if (> active-a org-focus-max-active-p0) 'warning 'success))
        (if stale-a
            (progn
              (insert (format "Stale (not clocked in %d days):\n" org-focus-stale-p0-days))
              (dolist (item stale-a)
                (org-focus--insert-button-line
                 (plist-get item :heading)
                 (plist-get item :marker)
                 (plist-get item :file))))
          (insert (propertize "No stale P0 tasks.\n" 'face 'success)))
        (insert "\n")

        (when rows
          (let ((groups (org-focus--group-rows rows)))
            (org-focus--insert-section-title
             (format "Entries with Clocks (%d)" (length groups)))
            (dolist (g groups)
              (let ((priority (plist-get g :priority))
                    (count (plist-get g :count)))
                (org-focus--insert-button-line
                 (plist-get g :heading)
                 (plist-get g :marker)
                 (plist-get g :file)
                 (format "%s  %s%s"
                         (if (> count 1) (format "  (%d)" count) "")
                         (org-focus--format-hours (plist-get g :minutes))
                         (if priority (format "  [%s]" priority) "")))))
            (insert "\n")))

        (org-focus--insert-section-title "Warnings")
        (if warnings
            (dolist (w warnings)
              (insert (propertize (concat "- " w "\n") 'face 'warning)))
          (insert (propertize "No warnings.\n" 'face 'success)))
        (insert "\n")

        (org-focus--insert-section-title "Metadata issues")
        (if lint
            (dolist (item lint)
              (org-focus--insert-button-line
               (plist-get item :heading)
               (plist-get item :marker)
               (plist-get item :file)
               (format "  [%s]" (string-join (plist-get item :issues) "; "))))
          (insert "No metadata issues.\n"))
        (goto-char (point-min))))
    (pop-to-buffer buf)))

(defun org-focus-dashboard-subtree ()
  "Render the dashboard for the subtree at point."
  (interactive)
  (let* ((heading (org-focus--heading))
         (data (org-focus--collect-subtree-data)))
    (org-focus--render-dashboard data "Org Focus Dashboard" (format "subtree: %s" heading))))

(defun org-focus-dashboard-global ()
  "Render the dashboard for all entries in the configured files."
  (interactive)
  (org-focus--render-dashboard
   (org-focus--collect-global-data) "Org Focus Dashboard" "global"))

(defun org-focus-dashboard ()
  "Render the focus dashboard for the subtree at point.
With a `\\[universal-argument]' prefix, use global scope instead."
  (interactive)
  (if current-prefix-arg
      (org-focus-dashboard-global)
    (org-focus-dashboard-subtree)))

;;;; Lint view

(defun org-focus--render-lint (data title &optional scope-label)
  "Render lint content for DATA with TITLE and optional SCOPE-LABEL."
  (let ((items (plist-get data :lint))
        (buf (get-buffer-create org-focus-lint-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (special-mode)
        (insert (propertize title 'face '(:height 1.2 :weight bold)))
        (when scope-label
          (insert (format " (%s)" scope-label)))
        (insert "\n\n")
        (if items
            (dolist (item items)
              (org-focus--insert-button-line
               (plist-get item :heading)
               (plist-get item :marker)
               (plist-get item :file))
              (dolist (issue (plist-get item :issues))
                (insert (format "  - %s\n" issue)))
              (insert "\n"))
          (insert (propertize "No lint issues found.\n" 'face 'success)))
        (goto-char (point-min))))
    (pop-to-buffer buf)))

(defun org-focus-lint-subtree ()
  "Render lint results for the subtree at point."
  (interactive)
  (let* ((heading (org-focus--heading))
         (data (org-focus--collect-subtree-data)))
    (org-focus--render-lint data "Org Focus Lint" (format "subtree: %s" heading))))

(defun org-focus-lint-global ()
  "Render lint results for all entries in the configured files."
  (interactive)
  (org-focus--render-lint
   (org-focus--collect-global-data) "Org Focus Lint" "global"))

(defun org-focus-lint ()
  "Render lint results for the subtree at point.
With a `\\[universal-argument]' prefix, use global scope instead."
  (interactive)
  (if current-prefix-arg
      (org-focus-lint-global)
    (org-focus-lint-subtree)))

;;;; Convenience

(defun org-focus-install-tag-groups ()
  "Set a recommended `org-tag-alist' with shortcut keys.
This overwrites `org-tag-alist', so only use it if that is what you want."
  (interactive)
  (setq org-tag-alist
        '((:startgroup)
          ("prod" . ?p)
          ("team" . ?t)
          ("org"  . ?o)
          (:endgroup)
          (:startgroup)
          ("build" . ?b)
          ("help"  . ?h)
          ("ops"   . ?a)
          ("sync"  . ?m)
          (:endgroup)
          (:startgroup)
          ("plan"   . ?l)
          ("unplan" . ?u)
          (:endgroup)
          ("invest" . ?i)))
  (message "Installed recommended org-tag-alist for org-focus"))

;;;###autoload
(define-minor-mode org-focus-mode
  "Global mode for Org focus enforcement and review.

Key bindings:
- C-c f d  dashboard (subtree scope; C-u C-c f d for global)
- C-c f l  lint (subtree scope; C-u C-c f l for global)
- C-c f f  fix current entry

Commands:
- `org-focus-dashboard-subtree'  dashboard for the current subtree
- `org-focus-dashboard-global'   dashboard for all entries
- `org-focus-lint-subtree'       lint for the current subtree
- `org-focus-lint-global'        lint for all entries

Dashboard shows:
- Total clocked time (no week filtering)
- Metadata completeness and investment ratio
- Breakdown by domain/activity/intentionality
- Time by priority and urgent (P0) status (active count, stale tasks)
- Clickable entries with clocks
- Warnings and metadata issues"
  :global t
  :keymap org-focus-mode-map
  (if org-focus-mode
      (add-hook 'org-clock-in-hook #'org-focus--ensure-on-clock-in)
    (remove-hook 'org-clock-in-hook #'org-focus--ensure-on-clock-in)))

(provide 'org-focus)

;;; org-focus.el ends here
