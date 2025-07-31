;;; qjira.el --- A set of utilities for working with Magiclab Jira instance -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'jiralib2)

(defgroup qjira nil
  "QJira customization group."
  :group 'applications)

(defcustom qjira-project-prefixes '("IOS" "MAPI" "AND" "MP" "IAT" "TECHS" "MRE")
  "A list of project prefixes recognized by QJira"
  :group 'qjira
  :type '(repeat string))

(defun qjira-ticket-validation (str)
  "Take STRING an input and return jira ticket format if discovered."
  (when-let (((string-match "\\(\\([a-zA-Z]*\\)-[0-9]*\\)" str))
             (issue-number (match-string 1 str))
             (project (match-string 2 str)))
    ;; check if both matches are non-nil (shouldn't both be non-nil if we matched?)
    (unless (and project issue-number)
      (error "Project description not detected"))

    ;; check if the project is correct
    (unless (member project qjira-project-prefixes)
      (error "Project type (%s) not recognized" project))

    ;; issue-number needed, right?
    issue-number))

(defun find-matching-words (regexp text)
  "Find all substrings in TEXT that match REGEXP."
  (let ((start 0)
        matches)
    (while (string-match regexp text start)
      (push (match-string 1 text) matches)
      (setq start (match-end 0)))
    (nreverse matches)))

(defun qjira-slack-review-message ()
  (interactive)
  (when-let (
	     (bounds (bounds-of-thing-at-point 'symbol))
             (left (car bounds))
             (right (cdr bounds))
             (ticket (qjira-ticket-validation (buffer-substring left right))
		     )
	     (jira-ticket (jiralib2-get-issue ticket)
			  )
             (summary
	      (thread-last jira-ticket
                        (alist-get 'fields)
                        (alist-get 'summary)
			)
	      )
	     (commits-section
	      (thread-last jira-ticket
                        (alist-get 'fields)
                        (alist-get 'customfield_10146)
			)
	      )
	     (git-PR (car (find-matching-words "\\(https://github.bumble.dev/ios/bumble/pull.*?\\)]" commits-section)))
	     )
    (message "Found ticket: %s %s %s" ticket summary git-PR)

    ;; drop the original text
    (delete-region left right)

    ;; insert a replacement text (summary + org link)
    (let* (
	   (jira-link  (format ":jira: %s" (format "%s/browse/%s" jiralib2-url ticket))
		       )
	   (summary (format "Please review: %s" (replace-regexp-in-string "\\[.*\\][ ]*" "" summary))
		    )
	   (git-link (format ":git: %s" git-PR)
		     )
	   )
	   (insert (format "%s\n%s\n%s\n%s" ticket summary jira-link git-link)
		   )
    ))
)

(defun qjira-convert-to-link-with-summary ()
  "Take current a symbol at point pointer and look up in order url, symbol (pasteboard?)."
  (interactive)
  (when-let ((bounds (bounds-of-thing-at-point 'symbol))
             (left (car bounds))
             (right (cdr bounds))
             (ticket (qjira-ticket-validation (buffer-substring left right)))
	     (summary (thread-last (jiralib2-get-issue ticket)
                        (alist-get 'fields)
                        (alist-get 'summary)))
	     )
    (message "Found ticket: %s %s" ticket summary)

    ;; drop the original text
    (delete-region left right)

    ;; insert a replacement text (summary + org link)
    (let* ((url (format "%s/browse/%s" jiralib2-url ticket))
           (org-link  (format "[[%s][%s]]" url ticket))
           (summary (replace-regexp-in-string "\\[.*\\][ ]*" "" summary)))
      (insert (format "%s %s" summary org-link)))))


(provide 'qjira)
;;; qjira.el ends here
