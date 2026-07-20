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
;; The report itself is an Org-mode buffer.

;;; Code:

(require 'org)
(require 'org-clock)
(require 'subr-x)

(defgroup org-task-report nil
  "Aggregate same-titled org entries into a per-day report."
  :group 'org)

(defcustom org-task-report-buffer-name "*Org Task Report*"
  "Name of the report buffer."
  :type 'string
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

;;;; Report

(defun org-task-report--render (title rows)
  "Insert an Org report for TITLE from ROWS into the current buffer."
  (let ((total (apply #'+ (mapcar (lambda (r) (plist-get r :minutes)) rows))))
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

(defun org-task-report ()
  "Build a per-day report for the entry under the cursor.
Finds every same-titled entry in the current buffer and renders each as a
dated Org section showing its note content and the time clocked that day."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (let (title rows)
    (save-excursion
      (org-back-to-heading t)
      (setq title (org-task-report--title-at-point)
            rows (org-task-report--collect title)))
    (let ((buf (get-buffer-create org-task-report-buffer-name)))
      (with-current-buffer buf
        (erase-buffer)
        (org-task-report--render title rows)
        (org-mode)
        (goto-char (point-min)))
      (pop-to-buffer buf))))

;;;###autoload
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c t r") #'org-task-report))

(provide 'org-task-report)

;;; org-task-report.el ends here
