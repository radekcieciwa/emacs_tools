#!/usr/bin/env emacs --script
;; Debug script to understand what's happening

(require 'org)
(require 'org-clock)
(require 'time-date)

;; Load our module
(require (quote org-focus))

(message "\n=== Debug Info ===\n")

;; Check week boundaries
(pcase-let* ((`(,week-start ,week-end) (org-focus--week-start-end)))
  (message "Week start: %s" (format-time-string "%Y-%m-%d %H:%M:%S" week-start))
  (message "Week end: %s" (format-time-string "%Y-%m-%d %H:%M:%S" week-end)))

;; Load the file and scan for entries
(let ((file (expand-file-name "~/Orgs/work/today.org")))
  (message "File: %s" file)
  (with-current-buffer (find-file-noselect file t)
    (org-with-wide-buffer
     (let ((count 0)
           (with-children 0)
           (without-children 0))
       (org-map-entries
        (lambda ()
          (setq count (1+ count))
          (let ((has-children (org-focus--has-child-headings-p))
                (excluded (org-focus--excluded-p))
                (clocked (org-focus--clocked-entry-p))
                (heading (org-focus--heading)))
            (when clocked
              (if has-children
                  (setq with-children (1+ with-children))
                (setq without-children (1+ without-children))))
            (when (and clocked (< count 5))
              (message "Entry %d: %s (children:%s, excluded:%s, clocked:%s)"
                       count heading has-children excluded clocked))))
        nil 'file)
       (message "\nTotal entries: %d" count)
       (message "Entries with children and clocks: %d" with-children)
       (message "Entries without children and clocks: %d" without-children)))))

(message "\n=== Done ===\n")
