;;; diagnose-clocks.el --- Diagnostic tool for clock entries -*- lexical-binding: t; -*-

;; Analyzes clock entries to find issues causing unrealistic totals

(require 'org-focus)

(defun diagnose-clocks--analyze-file (file)
  "Analyze clock entries in FILE for issues."
  (with-current-buffer (find-file-noselect file t)
    (let ((open-clocks 0)
          (long-clocks 0)
          (total-minutes 0)
          (issues '())
          (week-start (encode-time 0 0 0 11 5 2026))
          (week-end (encode-time 0 0 0 18 5 2026)))
      (org-with-wide-buffer
       (org-map-entries
        (lambda ()
          (unless (org-focus--excluded-p)
            (let ((heading (org-focus--heading)))
              (save-excursion
                (org-back-to-heading t)
                (let ((end (org-end-of-subtree t t))
                      (marker (copy-marker (point))))
                  (forward-line 1)
                  (while (< (point) end)
                    (when (looking-at "^[ \t]*CLOCK:")
                      (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
                        (cond
                         ((string-match "=>[ \t]+\\([0-9]+\\):\\([0-9]+\\)" line)
                          (let* ((hours (string-to-number (match-string 1 line)))
                                 (mins (string-to-number (match-string 2 line)))
                                 (total-mins (+ (* 60 hours) mins)))
                            (setq total-minutes (+ total-minutes total-mins))
                            (when (> total-mins 480)
                              (setq long-clocks (1+ long-clocks))
                              (push (list :type "long" :heading heading :hours hours :mins mins
                                         :file (buffer-file-name) :marker marker) issues))))
                         (t
                          (setq open-clocks (1+ open-clocks))
                          (push (list :type "open" :heading heading :file (buffer-file-name)
                                     :line line :marker marker) issues)))))
                    (forward-line 1)))))))
        nil))
      (list :open open-clocks :long long-clocks :total total-minutes :issues issues))))

;;;###autoload
(defun org-focus-diagnose-clocks ()
  "Run clock diagnostics and show results in a buffer."
  (interactive)
  (let ((buf (get-buffer-create "*Org Focus Clock Diagnostics*"))
        (total-open 0)
        (total-long 0)
        (grand-total 0))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (special-mode)
        (insert (propertize "Org Focus Clock Diagnostics\n" 'face '(:height 1.2 :weight bold)))
        (insert (propertize "Analyzing your org files...\n\n" 'face 'font-lock-comment-face))

        (dolist (file (org-agenda-files t))
          (when (and file (file-exists-p file))
            (let* ((result (diagnose-clocks--analyze-file file))
                   (open (plist-get result :open))
                   (long (plist-get result :long))
                   (total (plist-get result :total))
                   (issues (plist-get result :issues)))
              (setq total-open (+ total-open open))
              (setq total-long (+ total-long long))
              (setq grand-total (+ grand-total total))

              (insert (propertize (format "File: %s\n" (file-name-nondirectory file))
                                 'face 'font-lock-string-face))
              (insert (format "  Total clocked: %dh %dm\n" (/ total 60) (mod total 60)))
              (insert (format "  Open clocks: %d\n" open))
              (insert (format "  Long clocks (>8h): %d\n\n" long)))))

        (insert (propertize "\n=== Summary ===\n" 'face '(:weight bold)))
        (insert (format "Total across all files: %dh %dm\n" (/ grand-total 60) (mod grand-total 60)))
        (insert (format "Open/incomplete clocks: %d\n" total-open))
        (insert (format "Long clocks (>8h): %d\n" total-long))

        (if (> grand-total 10080)
            (insert (propertize (format "\n❌ WARNING: %dh exceeds 168h/week limit!\n" (/ grand-total 60))
                               'face '(:foreground "red" :weight bold)))
          (insert (propertize "\n✅ Total hours within normal range\n" 'face '(:foreground "green"))))

        (when (> total-open 0)
          (insert (propertize "\n\n=== Open/Incomplete Clocks ===\n" 'face '(:weight bold :foreground "red")))
          (dolist (issue (apply 'append (mapcar (lambda (f)
                                                  (plist-get (diagnose-clocks--analyze-file f) :issues))
                                                (org-agenda-files t))))
            (when (eq (plist-get issue :type) 'open)
              (insert (propertize (format "• %s\n" (plist-get issue :heading))
                                 'face 'font-lock-warning-face))
              (insert (format "  File: %s\n" (file-name-nondirectory (plist-get issue :file))))
              (insert (format "  Line: %s\n\n" (plist-get issue :line))))))

        (when (> total-long 0)
          (insert (propertize "\n=== Long Clocks (>8h) ===\n" 'face '(:weight bold :foreground "orange")))
          (dolist (issue (apply 'append (mapcar (lambda (f)
                                                  (plist-get (diagnose-clocks--analyze-file f) :issues))
                                                (org-agenda-files t))))
            (when (eq (plist-get issue :type) 'long)
              (insert (propertize (format "• %s\n" (plist-get issue :heading))
                                 'face 'font-lock-warning-face))
              (insert (format "  Duration: %dh %dm\n" (plist-get issue :hours) (plist-get issue :mins)))
              (insert (format "  File: %s\n\n" (file-name-nondirectory (plist-get issue :file)))))))

        (insert (propertize "\n=== Recommendations ===\n" 'face '(:weight bold)))
        (cond
         ((> total-open 0)
          (insert "1. Fix open clocks:\n")
          (insert "   - Find clocks without '=> HH:MM'\n")
          (insert "   - Add closing bracket ] and duration\n"))
         ((> total-long 0)
          (insert "1. Break long clocks:\n")
          (insert "   - Clocks >8h should be split into daily entries\n")
          (insert "   - Or review if they're correct\n")))
        (insert "2. Use M-x org-focus-lint to check metadata\n")
        (insert "3. Use M-x org-focus-dashboard to view your time breakdown\n")

        (goto-char (point-min))))
    (pop-to-buffer buf)))

(provide 'diagnose-clocks)
