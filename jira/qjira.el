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

(defcustom qjira-retry-timeouts '(5 10)
  "Per-attempt timeouts, in seconds, used when fetching a JIRA issue.
The number of elements determines the number of attempts; each entry
bounds one attempt.  A failed attempt (timeout or error) falls back to
the next element.  Values are sized to absorb cold-start latency (DNS,
TLS, VPN warmup) on the first fetch after a session is established."
  :group 'qjira
  :type '(repeat integer))

(defun retry-with-timeouts (thunk timeouts &optional description)
  "Call THUNK repeatedly until it returns non-nil, retrying on failure.
TIMEOUTS is a list of per-attempt timeouts in seconds; THUNK is invoked
once per element, each call bounded by `with-timeout' for that many
seconds.  An attempt that times out (THUNK returns nil) or signals an
error falls back to the next timeout.  Returns the first non-nil value
THUNK produces.  Signals an error only after all attempts are exhausted.

DESCRIPTION is an optional string naming the operation, used in
progress and error messages.

This is a generic helper, independent of JIRA, intended for reuse with
any flaky, timeout-prone operation."
  (let ((remaining timeouts)
        (attempt 0)
        (result nil)
        (last-error nil)
        (label (or description "operation")))
    (while (and remaining (null result))
      (let ((timeout (car remaining)))
        (setq remaining (cdr remaining)
              attempt (1+ attempt))
        (condition-case err
            ;; `with-timeout' returns nil on timeout without signalling, so a
            ;; nil result simply leaves the loop running for the next attempt.
            (setq result (with-timeout (timeout nil)
                           (funcall thunk)))
          (error
           (setq last-error err)
           (message "retry-with-timeouts: attempt %d for %s failed: %s"
                    attempt label (error-message-string err))))))
    (unless result
      (if last-error
          (error "Failed %s after %d attempt(s): %s"
                 label attempt (error-message-string last-error))
        (error "Failed %s after %d attempt(s): timed out" label attempt)))
    result))

(defun qjira-get-issue-with-retry (ticket)
  "Fetch JIRA issue TICKET, retrying on timeout or error.
Attempts and their timeouts are configured by `qjira-retry-timeouts'.

Establishes the JIRA session *before* the timed retry loop.  A cold
`jiralib2-session-call' must first run a synchronous login round-trip,
and `with-timeout' cannot cleanly abort a `:sync t' request: a deadline
firing mid-login throws out of `request' without setting
`jiralib2--session', wasting the attempt and leaving stale state that
can poison the next one.  Priming the session here keeps the timed
portion to the single, fast, idempotent issue fetch."
  (unless jiralib2--session
    (jiralib2-session-login))
  (retry-with-timeouts (lambda () (jiralib2-get-issue ticket))
                       qjira-retry-timeouts
                       (format "fetch of ticket %s" ticket)))

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
	     (jira-ticket (qjira-get-issue-with-retry ticket)
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
	     (summary (thread-last (qjira-get-issue-with-retry ticket)
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
