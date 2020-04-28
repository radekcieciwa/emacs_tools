;;; qjira.el --- A set of utilities for working with Magiclab Jira instance

;;; Commentary:
;;

;;; Code:

(defgroup qjira nil
  "QJira customization group."
  :group 'applications)

(defun jira-ticket-validation (string)
  "Take STRING an input and return jira ticket format if discovered."
  (if (string-match "\\(\\([a-zA-Z]*\\)-[0-9]*\\)" string)
      (let (
	    (issue-number (match-string 1 string))
	    (project (match-string 2 string))
	    )
	(if (and project issue-number)
	    (if (car (member project '("IOS" "MAPI" "AND")))
		issue-number
	      )
	  (message "Project description not detected")
	  )
	)
    )
  )

(defun jira-convert-to-link-with-sumary ()
  "Take current a symbol at point pointer and look up in order url, symbol (pasteboard?)."
  (interactive)
  (if (thing-at-point 'symbol)
      (let ((ticket (jira-ticket-validation (thing-at-point 'symbol))))
	(if ticket
	    (let ((summary (alist-get 'summary (alist-get 'fields (jiralib2-get-issue ticket)))))
	      (message "Found ticket: %s %s" ticket summary)
              (delete-region (car (bounds-of-thing-at-point 'symbol)) (cdr (bounds-of-thing-at-point 'symbol)))
              (insert (concat (replace-regexp-in-string "\\[.*\\][ ]*" "" summary) " [[" jiralib2-url "/browse/" ticket "][" ticket "]]"))
              )
	  )
	)
    )
  )

(provide 'qjira)
;;; qjira.el ends here
