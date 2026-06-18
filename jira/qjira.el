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

(defcustom qjira-retry-timeouts '(2 5 10)
  "Per-attempt timeouts, in seconds, used when fetching a JIRA issue.
The number of elements determines the number of attempts; each entry
bounds one attempt.  A failed attempt (timeout or error) falls back to
the next element.

Values grow short-to-long on purpose: the first, tight attempt returns
fast in the common case where the connection is already warm, while the
later, longer attempts absorb cold-start latency (DNS, TLS, VPN warmup,
session re-login) only when it is actually needed."
  :group 'qjira
  :type '(repeat number))

(defun retry-with-timeouts (thunk timeouts &optional description on-failure)
  "Call THUNK repeatedly until it returns non-nil, retrying on failure.
TIMEOUTS is a list of per-attempt timeouts in seconds.  THUNK is invoked
once per element as (funcall THUNK TIMEOUT) -- the per-attempt timeout is
passed in so the thunk can hand it to a backend that cancels cleanly (e.g.
`request' \\='s native `:timeout') -- and is additionally bounded by
`with-timeout' for that many seconds as a backstop.  An attempt that times
out (THUNK returns nil, or the `with-timeout' fires) or signals an error
falls back to the next timeout.  Returns the first non-nil value THUNK
produces.  Signals an error only after all attempts are exhausted.

DESCRIPTION is an optional string naming the operation, used in
progress and error messages.

ON-FAILURE, if non-nil, is a function called with no arguments after each
failed attempt.  Use it to reset any state a failed attempt may have left
inconsistent, so the next attempt starts clean.

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
                           (funcall thunk timeout)))
          (error
           (setq last-error err)
           (message "retry-with-timeouts: attempt %d for %s failed: %s"
                    attempt label (error-message-string err))))
        (when (and (null result) on-failure)
          (funcall on-failure))))
    (unless result
      (if last-error
          (error "Failed %s after %d attempt(s): %s"
                 label attempt (error-message-string last-error))
        (error "Failed %s after %d attempt(s): timed out" label attempt)))
    result))

(defun qjira--reset-session ()
  "Clear the cached JIRA session cookie.
Called after a failed fetch attempt so the next one performs a clean login
instead of reusing a possibly stale or half-written `jiralib2--session'.

A cold or `with-timeout'-interrupted login can leave `jiralib2--session' set
to an expired cookie (the `setq' inside `jiralib2-session-login' never ran, or
ran against an old value).  Reusing it makes the next fetch answer 401 and
re-login *inside* the timed window, which on a cold connection just times out
again -- the loop never converges, yet an immediate manual retry on a warm
connection succeeds.  Forcing the cookie back to nil here breaks that cycle."
  (setq jiralib2--session nil))

(defun qjira--get-issue-once (ticket timeout)
  "Single attempt to fetch JIRA issue TICKET, bounded by TIMEOUT seconds.
Logs in first when no session exists, then fetches the issue passing TIMEOUT
natively to `request' (through `jiralib2-session-call').  The native timeout
cancels the underlying curl process cleanly on expiry, unlike a `with-timeout'
throw, which abandons the in-flight `:sync t' request."
  (unless jiralib2--session
    (jiralib2-session-login))
  (jiralib2-session-call (format "/rest/api/2/issue/%s" ticket)
                         :timeout timeout))

(defun qjira-get-issue-with-retry (ticket)
  "Fetch JIRA issue TICKET, retrying on timeout or error.
Attempts and their timeouts are configured by `qjira-retry-timeouts'.

Each attempt logs in when needed and fetches the issue within its timeout
\(see `qjira--get-issue-once'); after any failed attempt the session cookie is
cleared (see `qjira--reset-session') so the next attempt starts from a clean,
bounded login rather than reusing poisoned session state."
  (retry-with-timeouts
   (lambda (timeout) (qjira--get-issue-once ticket timeout))
   qjira-retry-timeouts
   (format "fetch of ticket %s" ticket)
   #'qjira--reset-session))

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
