#!/usr/bin/env emacs --script
;; Debug has-child-headings-p function

(require 'org)

(require (quote org-focus))

(message "\n=== Debugging has-child-headings-p ===\n")

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
              (level (org-current-level))
              (has-children (org-focus--has-child-headings-p)))
          (message "Heading: '%s'" heading)
          (message "  Level: %d" level)
          (message "  Has children: %s" has-children)
          (message "")))
      nil 'file)))

    (kill-buffer)))

(delete-file test-file)

(message "=== Done ===\n")
