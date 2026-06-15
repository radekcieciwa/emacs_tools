;; Add functionality to extract my extension of jira tickets as org-mode entries to git worktree checkout
;; Extract jira key, and tries to open magit on that repository

(defun my/magit-open-jira-worktree ()
  "Extract Jira key from current org heading and open its worktree in Magit."
  (interactive)
  (let* ((worktree-root "~/Development/bumble1/_tickets/") ;; <-- change this
         (heading (org-get-heading t t t t))
         (jira-key (when (string-match "\\b[A-Z]+-[0-9]+\\b" heading)
                     (match-string 0 heading))))
    (unless jira-key
      (user-error "No Jira key found in current org heading"))
    (let ((repo-dir (expand-file-name jira-key worktree-root)))
      (unless (file-directory-p repo-dir)
        (user-error "Directory does not exist: %s" repo-dir))
      (magit-status repo-dir))))

(provide 'magit-fast)
