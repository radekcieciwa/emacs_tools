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
  (when (string-match "\\(\\([a-zA-Z]*\\)-[0-9]*\\)" str)
    (let ((issue-number (match-string 1 str))
          (project (match-string 2 str)))
      ;; check if both matches are non-nil (shouldn't both be non-nil if we matched?)
      (unless (and project issue-number)
        (error "Project description not detected"))

      ;; check if the project is correct
      (unless (member project qjira-project-prefixes)
        (error "Project not not recognized"))

      ;; issue-number needed, right?
      issue-number)))

(defun qjira-convert-to-link-with-summary ()
  "Take current a symbol at point pointer and look up in order url, symbol (pasteboard?)."
  (interactive)
  (when (thing-at-point 'symbol)
    (let ((ticket (qjira-ticket-validation (thing-at-point 'symbol))))
      (when ticket
        (let ((summary (alist-get 'summary (alist-get 'fields (jiralib2-get-issue ticket)))))
          (message "Found ticket: %s %s" ticket summary)
          (delete-region (car (bounds-of-thing-at-point 'symbol)) (cdr (bounds-of-thing-at-point 'symbol)))
          (insert (concat (replace-regexp-in-string "\\[.*\\][ ]*" "" summary) " [[" jiralib2-url "/browse/" ticket "][" ticket "]]")))))))


(provide 'qjira)
;;; qjira.el ends here
