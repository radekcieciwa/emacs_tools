;;; org-task-report.el --- Aggregate same-titled org entries into a per-day report -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Takes the org entry under the cursor and finds every other entry in the
;; *same buffer* that shares its title (ignoring TODO/DONE state, priority
;; cookie and tags).  It then produces a read-only report that lists each
;; matching entry as a dated block: the note content written under the entry,
;; and the total time clocked in that entry (its "time spent on that day").
;;
;; Scope is intentionally single-file for now: only the buffer that holds the
;; entry under the cursor is scanned.  Extending to `org-agenda-files' later is
;; a matter of swapping `org-task-report--map-entries'.
;;
;; Edge case: two entries carrying the same title on the same date are NOT
;; merged.  They appear as two separate dated blocks in the report.
;;
;; Entry point: M-x org-task-report, bound to C-c t r in Org buffers.
;;
;; The report is presented in one of two styles, chosen by
;; `org-task-report-presentation':
;; - `org'       an Org-mode buffer of dated headings with `file:' links.
;; - `dashboard' a read-only rendered dashboard in the style of the org-focus
;;               dashboard: a summary block, then one dated block per entry
;;               with a clickable source button.
;; `org-task-report-toggle-presentation' (C-c t t in Org buffers, `t' inside a
;; dashboard report) flips between them and re-renders in place.

;;; Code:

(require 'org)
(require 'org-clock)
(require 'subr-x)
(require 'button)

(defgroup org-task-report nil
  "Aggregate same-titled org entries into a per-day report."
  :group 'org)

(defcustom org-task-report-buffer-name "*Org Task Report*"
  "Name of the report buffer."
  :type 'string
  :group 'org-task-report)

(defcustom org-task-report-presentation 'org
  "How `org-task-report' presents its report.

`org'
    An Org-mode buffer: one dated heading per entry, each an Org `file:'
    link back to the source, followed by the entry's note.  Editable, and
    links are followed with RET or \\[org-open-at-point].

`dashboard'
    A read-only rendered buffer in the style of the org-focus dashboard: a
    summary block (entries, days, total and average time), then one dated
    block per entry whose header is a clickable button back to the source
    and carries a bar scaled to the longest entry.

Both are built from the same collected rows, so switching only changes the
rendering.  `org-task-report-toggle-presentation' flips between them and
re-renders the current report."
  :type '(choice (const :tag "Org-mode buffer" org)
                 (const :tag "Rendered dashboard" dashboard))
  :group 'org-task-report)

;;;; Title handling

(defun org-task-report--title-at-point ()
  "Return the normalized title of the entry at point.
`org-get-heading' already drops TODO state, priority cookie and tags;
we additionally collapse internal whitespace and trim."
  (org-task-report--normalize-title (org-get-heading t t t t)))

(defun org-task-report--normalize-title (heading)
  "Normalize HEADING for comparison: collapse whitespace and trim."
  (when heading
    (string-trim (replace-regexp-in-string "[ \t]+" " " heading))))

;;;; Clock parsing

(defun org-task-report--entry-body-region ()
  "Return (START . END) of the current entry's own body.
START is the line after the heading; END is the start of the first child
heading or, failing that, the end of the subtree.  Point must be on the
heading.  The region therefore excludes any child subtrees."
  (save-excursion
    (org-back-to-heading t)
    (let* ((current-level (org-current-level))
           (subtree-end (save-excursion (org-end-of-subtree t t)))
           (body-start (progn (end-of-line)
                              (if (eobp) (point) (1+ (point)))))
           (child-start
            (save-excursion
              (goto-char body-start)
              (let (found)
                (while (and (not found)
                            (re-search-forward "^\\(\\*+\\) " subtree-end t))
                  (when (> (length (match-string 1)) current-level)
                    (setq found (match-beginning 0))))
                found))))
      (cons body-start (or child-start subtree-end)))))

(defun org-task-report--parse-clock-line (line)
  "Parse a CLOCK LINE, returning (DATE . MINUTES) or nil.
DATE is a \"YYYY-MM-DD\" string taken from the clock's start stamp;
MINUTES is the closed duration (open clocks contribute no minutes)."
  (when (string-match "CLOCK:[ \t]*\\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" line)
    (let ((date (match-string 1 line))
          (minutes 0))
      (when (string-match "=>[ \t]+\\([0-9]+\\):\\([0-9]+\\)" line)
        (setq minutes (+ (* 60 (string-to-number (match-string 1 line)))
                         (string-to-number (match-string 2 line)))))
      (cons date minutes))))

(defun org-task-report--entry-clocks ()
  "Return the list of (DATE . MINUTES) for clocks in the entry at point.
Only the entry's own clocks are considered; child subtrees are excluded."
  (let ((region (org-task-report--entry-body-region))
        clocks)
    (save-excursion
      (goto-char (car region))
      (while (re-search-forward "^[ \t]*CLOCK:" (cdr region) t)
        (let ((parsed (org-task-report--parse-clock-line
                       (buffer-substring (line-beginning-position)
                                         (line-end-position)))))
          (when parsed (push parsed clocks)))))
    (nreverse clocks)))

(defun org-task-report--entry-date (clocks)
  "Return the representative date for an entry given its CLOCKS.
The earliest clock date is used; nil when the entry has no clocks."
  (car (car (sort (copy-sequence clocks)
                  (lambda (a b) (string< (car a) (car b)))))))

(defun org-task-report--entry-minutes (clocks)
  "Return the total minutes across CLOCKS."
  (apply #'+ (mapcar #'cdr clocks)))

;;;; Body extraction

(defun org-task-report--entry-note ()
  "Return the note text of the entry at point, or nil when empty.
Planning lines, drawers (PROPERTIES/LOGBOOK) and CLOCK lines are stripped
so only the human-written content remains."
  (let* ((region (org-task-report--entry-body-region))
         (raw (buffer-substring-no-properties (car region) (cdr region)))
         (lines (split-string raw "\n"))
         (in-drawer nil)
         kept)
    (dolist (line lines)
      (cond
       (in-drawer
        (when (string-match-p "^[ \t]*:END:[ \t]*$" line)
          (setq in-drawer nil)))
       ((string-match-p "^[ \t]*:\\(PROPERTIES\\|LOGBOOK\\):[ \t]*$" line)
        (setq in-drawer t))
       ((string-match-p "^[ \t]*CLOCK:" line))
       ((string-match-p "^[ \t]*\\(SCHEDULED\\|DEADLINE\\|CLOSED\\):" line))
       (t (push line kept))))
    (let ((note (string-trim (string-join (nreverse kept) "\n"))))
      (unless (string-empty-p note) note))))

;;;; Collection

(defun org-task-report--map-entries (fn)
  "Call FN with point on each heading in the current buffer.
Single-file scope: only the current buffer is scanned."
  (org-with-wide-buffer
   (org-map-entries fn nil nil)))

(defun org-task-report--collect (title)
  "Collect report rows for every entry whose title matches TITLE.
Each row is a plist with :date :minutes :note :file :line."
  (let (rows)
    (org-task-report--map-entries
     (lambda ()
       (when (equal title (org-task-report--title-at-point))
         (let ((clocks (org-task-report--entry-clocks)))
           (push (list :date (org-task-report--entry-date clocks)
                       :minutes (org-task-report--entry-minutes clocks)
                       :note (org-task-report--entry-note)
                       :file (buffer-file-name)
                       :line (line-number-at-pos))
                 rows)))))
    (org-task-report--sort-rows (nreverse rows))))

(defun org-task-report--sort-rows (rows)
  "Sort ROWS by :date ascending; dateless rows sink to the end."
  (sort rows
        (lambda (a b)
          (let ((da (plist-get a :date))
                (db (plist-get b :date)))
            (cond ((and da db) (string< da db))
                  (da t)
                  (t nil))))))

;;;; Formatting

(defun org-task-report--format-hm (minutes)
  "Format MINUTES as a compact \"Hh MMm\" duration string."
  (let ((h (/ minutes 60))
        (m (% minutes 60)))
    (cond ((= minutes 0) "0m")
          ((= h 0) (format "%dm" m))
          ((= m 0) (format "%dh" h))
          (t (format "%dh %02dm" h m)))))

(defun org-task-report--total-minutes (rows)
  "Return the total clocked minutes across ROWS."
  (apply #'+ (mapcar (lambda (r) (plist-get r :minutes)) rows)))

(defun org-task-report--dates (rows)
  "Return the distinct dates present in ROWS, dateless rows ignored."
  (delete-dups (delq nil (mapcar (lambda (r) (plist-get r :date)) rows))))

;;;; Report: Org presentation

(defun org-task-report--render-org (title rows)
  "Insert an Org report for TITLE from ROWS into the current buffer."
  (let ((total (org-task-report--total-minutes rows)))
    (insert (format "#+TITLE: %s\n\n" title))
    (insert (format "%d entr%s · Total: %s\n"
                    (length rows)
                    (if (= (length rows) 1) "y" "ies")
                    (org-task-report--format-hm total)))
    (dolist (row rows)
      (let* ((date (plist-get row :date))
             (minutes (plist-get row :minutes))
             (note (plist-get row :note))
             (label (or date "(no clock info)")))
        (insert (format "\n* %s · %s\n"
                        (org-task-report--source-link (plist-get row :file)
                                                   (plist-get row :line)
                                                   label)
                        (org-task-report--format-hm minutes)))
        (insert (if note (concat note "\n") "/(no notes)/\n"))))))

(defun org-task-report--source-link (file line label)
  "Return an Org link to FILE at LINE with LABEL, or LABEL when no FILE.
The line number is a snapshot taken when the report was built."
  (if file
      (format "[[file:%s::%d][%s]]" file line label)
    label))

;;;; Report: dashboard presentation

(defvar org-task-report-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "t") #'org-task-report-toggle-presentation)
    map)
  "Keymap active in a dashboard-presented report buffer.")

(defun org-task-report--insert-section-title (title)
  "Insert TITLE as a section header."
  (insert (propertize title 'face 'bold) "\n"))

(defun org-task-report--insert-kv-line (label value)
  "Insert LABEL and VALUE as an aligned key/value line."
  (insert (format "%-18s %s\n" (concat label ":") value)))

(defun org-task-report--goto-source (file line)
  "Visit FILE and put point on LINE, revealing the entry when in Org mode.
LINE is the snapshot taken when the report was built, so editing the source
afterwards may drift it."
  (find-file file)
  (goto-char (point-min))
  (forward-line (1- line))
  (when (derived-mode-p 'org-mode)
    (org-fold-show-context)
    (org-fold-show-entry)))

(defun org-task-report--insert-button-line (label file line &optional suffix)
  "Insert clickable LABEL jumping to LINE in FILE, with optional SUFFIX.
With no FILE (a report built from a buffer not visiting a file) LABEL is
inserted as plain text, mirroring the Org renderer's link fallback."
  (if file
      (insert-text-button label
                          'action (lambda (_) (org-task-report--goto-source file line))
                          'follow-link t)
    (insert label))
  (when suffix
    (insert suffix))
  (insert "\n"))

(defun org-task-report--bar (minutes max-minutes)
  "Return a bar of block characters for MINUTES scaled to MAX-MINUTES."
  (make-string (round (* 16.0 (/ (float minutes) (max 1 max-minutes)))) ?█))

(defun org-task-report--indent (text prefix)
  "Return TEXT with PREFIX prepended to each of its lines."
  (replace-regexp-in-string "^" prefix text))

(defun org-task-report--render-dashboard (title rows)
  "Insert a rendered dashboard for TITLE from ROWS into the current buffer.
Mirrors the org-focus dashboard: a large header, a summary block, then one
dated block per entry whose header line is a button back to the source."
  (special-mode)
  (use-local-map (make-composed-keymap org-task-report-dashboard-mode-map
                                       (current-local-map)))
  (let* ((total (org-task-report--total-minutes rows))
         (days (length (org-task-report--dates rows)))
         (max-minutes (apply #'max 1 (mapcar (lambda (r) (plist-get r :minutes)) rows))))
    (insert (propertize "Org Task Report" 'face '(:height 1.3 :weight bold)))
    (insert (format " (%s)\n\n" title))

    (org-task-report--insert-section-title "Summary")
    (org-task-report--insert-kv-line "Entries" (number-to-string (length rows)))
    (org-task-report--insert-kv-line "Days" (number-to-string days))
    (org-task-report--insert-kv-line "Total clocked"
                                     (org-task-report--format-hm total))
    (org-task-report--insert-kv-line "Average per day"
                                     (org-task-report--format-hm
                                      (if (> days 0) (/ total days) 0)))
    (insert "\n")

    (org-task-report--insert-section-title (format "Entries (%d)" (length rows)))
    (if (null rows)
        (insert "No matching entries.\n")
      (dolist (row rows)
        (let ((note (plist-get row :note))
              (minutes (plist-get row :minutes)))
          (insert "\n")
          (org-task-report--insert-button-line
           ;; Padded to a fixed width so the duration column stays aligned
           ;; when a dateless row widens the label.
           (format "%-15s" (or (plist-get row :date) "(no clock info)"))
           (plist-get row :file)
           (plist-get row :line)
           (format "  %-8s %s"
                   (org-task-report--format-hm minutes)
                   (org-task-report--bar minutes max-minutes)))
          (if note
              (insert (org-task-report--indent note "  ") "\n")
            (insert (propertize "  (no notes)\n" 'face 'shadow))))))
    (insert (propertize "\nPress t for the Org-mode presentation.\n"
                        'face 'shadow))))

;;;; Report

(defvar-local org-task-report--data nil
  "Cons of (TITLE . ROWS) backing the current report buffer.
Kept so `org-task-report-toggle-presentation' can re-render the same data
in the other presentation without rescanning the source buffer.")

(defun org-task-report--render-into-buffer (title rows)
  "Render TITLE and ROWS into the report buffer and return that buffer.
The presentation is chosen by `org-task-report-presentation'.  The data is
stashed buffer-locally after the major mode is set, since switching modes
clears buffer-local variables."
  (let ((buf (get-buffer-create org-task-report-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (pcase org-task-report-presentation
          ('dashboard (org-task-report--render-dashboard title rows))
          ('org (org-task-report--render-org title rows)
                (org-mode))
          (other (user-error "Unknown `org-task-report-presentation': %s" other)))
        (setq org-task-report--data (cons title rows))
        (goto-char (point-min))))
    buf))

(defun org-task-report ()
  "Build a per-day report for the entry under the cursor.
Finds every same-titled entry in the current buffer and renders each as a
dated block showing its note content and the time clocked that day, in the
presentation named by `org-task-report-presentation'."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (let (title rows)
    (save-excursion
      (org-back-to-heading t)
      (setq title (org-task-report--title-at-point)
            rows (org-task-report--collect title)))
    (pop-to-buffer (org-task-report--render-into-buffer title rows))))

(defun org-task-report-toggle-presentation ()
  "Switch `org-task-report-presentation' between `org' and `dashboard'.
An existing report is re-rendered in place from its stored rows, so the
source buffer is not rescanned and the report need not still be reachable."
  (interactive)
  (setq org-task-report-presentation
        (if (eq org-task-report-presentation 'dashboard) 'org 'dashboard))
  (let* ((buf (get-buffer org-task-report-buffer-name))
         (data (and buf (buffer-local-value 'org-task-report--data buf))))
    (if data
        (pop-to-buffer (org-task-report--render-into-buffer (car data) (cdr data)))
      (message "Task report presentation: %s" org-task-report-presentation))))

;;;###autoload
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c t r") #'org-task-report)
  (define-key org-mode-map (kbd "C-c t t") #'org-task-report-toggle-presentation))

(provide 'org-task-report)

;;; org-task-report.el ends here
