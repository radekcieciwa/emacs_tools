;;; qjira.el --- A set of utilities for working with Magiclab Jira instance -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'jiralib2)

(defgroup qjira nil
  "QJira customization group."
  :group 'applications)

(defcustom qjira-project-prefixes '("IOS" "MAPI" "AND")
  "A list of project prefixes recognized by QJira"
  :group 'qjira
  :type '(repeat string))

(defun qjira-ticket-validation (str)
  "Take STRING an input and return jira ticket format if discovered."
  (when-let ((string-match "\\(\\([a-zA-Z]*\\)-[0-9]*\\)" str)
             (issue-number (match-string 1 str))
             (project (match-string 2 str)))
    ;; check if both matches are non-nil (shouldn't both be non-nil if we matched?)
    (unless (and project issue-number)
      (error "Project description not detected"))

    ;; check if the project is correct
    (unless (member project qjira-project-prefixes)
      (error "Project not not recognized"))

    ;; issue-number needed, right?
    issue-number))

(defun qjira-convert-to-link-with-summary ()
  "Take current a symbol at point pointer and look up in order url, symbol (pasteboard?)."
  (interactive)
  (when-let ((bounds (bounds-of-thing-at-point 'symbol))
             (left (car bounds))
             (right (cdr bounds))
             (ticket (qjira-ticket-validation (buffer-substring left right)))
             (summary (thread-last (jiralib2-get-issue ticket)
                        (alist-get 'fields)
                        (alist-get 'summary))))
    (message "Found ticket: %s %s" ticket summary)

    ;; drop the original text
    (delete-region left right)

    ;; insert a replacement text (summary + org link)
    (let ((link (concat " [[" jiralib2-url "/browse/" ticket "][" ticket "]]"))
          (summary (replace-regexp-in-string "\\[.*\\][ ]*" "" summary)))
      (insert (concat summary link)))))


(provide 'qjira)
;;; qjira.el ends here
