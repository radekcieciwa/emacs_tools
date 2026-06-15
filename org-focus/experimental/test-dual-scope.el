#!/usr/bin/env emacs --script
;; Test dual-scope implementation

(require 'org)
(require 'org-clock)

(require (quote org-focus))

(message "\n=== Testing Dual-Scope Implementation ===\n")

;; Test with global scope
(message "Global scope test:")
(setq org-agenda-files '("~/Orgs/work/today.org"))
(let ((data (org-focus--collect-global-data)))
  (let ((total (plist-get data :total))
        (rows (plist-get data :rows)))
    (message "  Total clocked: %.1fh" (/ total 60.0))
    (message "  Entries with clocks: %d" (length rows))))

;; Test with file that has structure
(message "\nSubtree scope test (on test file):")
(with-current-buffer (find-file-noselect "test-clock-fixtures.org" t)
  (org-back-to-heading t)
  (let ((data (org-focus--collect-subtree-data)))
    (let ((total (plist-get data :total))
          (rows (plist-get data :rows)))
      (message "  Total clocked: %.1fh" (/ total 60.0))
      (message "  Entries with clocks: %d" (length rows)))))

(message "\n=== Done ===\n")
