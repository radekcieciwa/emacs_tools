;;; test-clock-simple.el --- Simple clock parsing validation -*- lexical-binding: t; -*-

(require 'org-focus)

;; Test 1: Single 8-hour clock entry
(with-temp-buffer
  (insert "* TODO Task :prod:build:plan:
CLOCK: [2026-05-15 Fri 09:00]--[2026-05-15 Fri 17:00] =>  8:00")
  (org-mode)
  (goto-char (point-min))
  (org-next-visible-heading 1)
  (let ((result (org-focus--sum-clocks-in-range
                 (encode-time 0 0 0 11 5 2026)
                 (encode-time 0 0 0 18 5 2026))))
    (message "Test 1 - Single 8h clock: %d minutes (expected 480) %s"
             result
             (if (= result 480) "✓" "✗"))))

;; Test 2: Multiple clocks (3h + 4h)
(with-temp-buffer
  (insert "* TODO Task :prod:build:plan:
CLOCK: [2026-05-13 Wed 09:00]--[2026-05-13 Wed 12:00] =>  3:00
CLOCK: [2026-05-13 Wed 13:00]--[2026-05-13 Wed 17:00] =>  4:00")
  (org-mode)
  (goto-char (point-min))
  (org-next-visible-heading 1)
  (let ((result (org-focus--sum-clocks-in-range
                 (encode-time 0 0 0 11 5 2026)
                 (encode-time 0 0 0 18 5 2026))))
    (message "Test 2 - Multiple clocks (3h+4h): %d minutes (expected 420) %s"
             result
             (if (= result 420) "✓" "✗"))))

;; Test 3: Fractional hours (2:30)
(with-temp-buffer
  (insert "* TODO Task :prod:build:plan:
CLOCK: [2026-05-15 Fri 09:00]--[2026-05-15 Fri 11:30] =>  2:30")
  (org-mode)
  (goto-char (point-min))
  (org-next-visible-heading 1)
  (let ((result (org-focus--sum-clocks-in-range
                 (encode-time 0 0 0 11 5 2026)
                 (encode-time 0 0 0 18 5 2026))))
    (message "Test 3 - Fractional (2:30): %d minutes (expected 150) %s"
             result
             (if (= result 150) "✓" "✗"))))

;; Test 4: No clock entries
(with-temp-buffer
  (insert "* TODO Task :prod:build:plan:
Some content but no clocks")
  (org-mode)
  (goto-char (point-min))
  (org-next-visible-heading 1)
  (let ((result (org-focus--sum-clocks-in-range
                 (encode-time 0 0 0 11 5 2026)
                 (encode-time 0 0 0 18 5 2026))))
    (message "Test 4 - No clocks: %d minutes (expected 0) %s"
             result
             (if (= result 0) "✓" "✗"))))

;; Test 5: Clock outside date range
(with-temp-buffer
  (insert "* TODO Task :prod:build:plan:
CLOCK: [2026-05-10 Thu 09:00]--[2026-05-10 Thu 17:00] =>  8:00")
  (org-mode)
  (goto-char (point-min))
  (org-next-visible-heading 1)
  (let ((result (org-focus--sum-clocks-in-range
                 (encode-time 0 0 0 11 5 2026)
                 (encode-time 0 0 0 18 5 2026))))
    (message "Test 5 - Outside range: %d minutes (expected 0) %s"
             result
             (if (= result 0) "✓" "✗"))))

(message "\nClock parsing validation complete!")
