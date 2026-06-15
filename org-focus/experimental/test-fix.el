#!/usr/bin/env emacs --script
;; Test script to validate the leaf-node fix

(require 'org)
(require 'org-clock)

;; Load our module
(require (quote org-focus))

;; Set up agenda files
(setq org-agenda-files '("~/Orgs/work/today.org"))
(setq org-focus-current-file-only nil)

(message "\n=== Testing Leaf-Node Fix ===\n")

;; Collect data for this week
(let ((data (org-focus--collect-weekly-data)))
  (let ((total (plist-get data :total))
        (known (plist-get data :known))
        (invest (plist-get data :invest))
        (lint (plist-get data :lint))
        (rows (plist-get data :rows)))

    (message "Total clocked this week: %.1fh" (/ total 60.0))
    (message "Entries with clocks: %d" (length rows))
    (message "Known metadata: %.1fh" (/ known 60.0))
    (message "Investment: %.1fh" (/ invest 60.0))
    (message "Lint issues: %d" (length lint))

    (when (> (/ total 60.0) 168)
      (message "WARNING: Total %.1fh exceeds 168h/week" (/ total 60.0)))

    (when (and (> total 0) (< (/ total 60.0) 25))
      (message "✓ Total is realistic (20-50h expected for work week)"))

    (message "\n=== By Activity ===")
    (let ((by-activity (plist-get data :by-activity)))
      (dolist (activity org-focus-activity-tags)
        (let ((mins (gethash activity by-activity 0)))
          (when (> mins 0)
            (message "%s: %.1fh" activity (/ mins 60.0))))))))

(message "\n=== Done ===\n")
