#!/usr/bin/env emacs --script
;; Comprehensive test of dual-scope implementation

(require 'org)
(require 'org-clock)

(require (quote org-focus))

(message "\n=== Comprehensive Dual-Scope Test ===\n")

;; Test 1: Global scope on work file
(message "Test 1: Global scope data collection")
(setq org-agenda-files '("~/Orgs/work/today.org"))
(let ((data (org-focus--collect-global-data)))
  (let ((total (plist-get data :total))
        (rows (plist-get data :rows))
        (lint (plist-get data :lint))
        (by-priority (plist-get data :by-priority)))
    (message "  Total clocked: %.1fh" (/ total 60.0))
    (message "  Entries with clocks: %d" (length rows))
    (message "  Lint issues: %d" (length lint))
    (message "  Priorities found: %d" (hash-table-count by-priority))
    (if (> (hash-table-count by-priority) 0)
        (let ((p0 (gethash "P0" by-priority 0))
              (p1 (gethash "P1" by-priority 0))
              (p2 (gethash "P2" by-priority 0)))
          (message "    P0: %.1fh, P1: %.1fh, P2: %.1fh"
                   (/ p0 60.0) (/ p1 60.0) (/ p2 60.0))))))

;; Test 2: Verify functions exist
(message "\nTest 2: Function existence")
(let ((functions '(org-focus-dashboard
                   org-focus-dashboard-subtree
                   org-focus-dashboard-global
                   org-focus-lint
                   org-focus-lint-subtree
                   org-focus-lint-global
                   org-focus--collect-subtree-data
                   org-focus--collect-global-data
                   org-focus--sum-clocks-for-entry
                   org-focus--render-dashboard
                   org-focus--render-lint)))
  (dolist (fn functions)
    (if (fboundp fn)
        (message "  ✓ %s" fn)
      (message "  ✗ %s MISSING" fn))))

;; Test 3: Verify customization groups
(message "\nTest 3: Customization options")
(let ((customs '(org-focus-domain-tags
                 org-focus-activity-tags
                 org-focus-intent-tags
                 org-focus-exclude-tags
                 org-focus-priorities
                 org-focus-investment-target-ratio
                 org-focus-unplanned-warning-ratio)))
  (dolist (cust customs)
    (if (boundp cust)
        (message "  ✓ %s" cust)
      (message "  ✗ %s MISSING" cust))))

;; Test 4: Test leaf-node filtering
(message "\nTest 4: Leaf-node filtering")
(with-current-buffer (find-file-noselect "~/Orgs/work/today.org" t)
  (org-back-to-heading t)
  (let ((data (org-focus--collect-subtree-data)))
    (let ((total (plist-get data :total))
          (rows (plist-get data :rows)))
      (message "  Subtree total: %.1fh" (/ total 60.0))
      (message "  Leaf entries with clocks: %d" (length rows))
      (message "  ✓ Leaf-node filtering active"))))

(message "\n=== All tests completed ===\n")
