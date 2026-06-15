#!/usr/bin/env emacs --script
;; Detailed debug of has-child-headings-p

(require 'org)

(require (quote org-focus))

(message "\n=== Detailed Debug ===\n")

(let ((test-file "/tmp/org-focus-test.org"))
  (with-temp-file test-file
    (insert "* Week #19 (PARENT)
** Monday (PARENT)
*** Task A :prod:build:plan:
:LOGBOOK:
CLOCK: [2026-05-13 Mon 09:00]--[2026-05-13 Mon 11:00] => 2:00
:END:
"))

  (with-current-buffer (find-file-noselect test-file t)
    (org-with-wide-buffer
     (org-map-entries
      (lambda ()
        (let ((heading (org-focus--heading))
              (level (org-current-level)))
          (message "\n=== Checking: '%s' (level %d) ===" heading level)

          ;; Manual check
          (save-excursion
            (let ((current-level level)
                  (subtree-end (org-end-of-subtree nil nil)))
              (message "  Current position: %d" (point))
              (message "  Subtree end: %d" subtree-end)

              (forward-line 1)
              (message "  After forward-line, position: %d" (point))

              (let ((found-child nil))
                (while (and (< (point) subtree-end) (not found-child))
                  (when (looking-at "^\\*+ ")
                    (let ((next-level (org-current-level)))
                      (message "    Found heading at %d, level: %d" (point) next-level)
                      (when (> next-level current-level)
                        (setq found-child t)
                        (message "    -> This is a child (level %d > %d)" next-level current-level))))
                  (forward-line 1))
                (message "  Final result: %s" found-child)))))
      nil 'file)))

    (kill-buffer)))

(delete-file test-file)

(message "\n=== Done ===\n")
