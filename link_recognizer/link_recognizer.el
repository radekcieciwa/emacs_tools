
 (defun jira-link-recognizer (string)
   (if (string-match "https?://bmbl.atlassian.net/browse/\\([a-zA-Z]*-[0-9]*\\)" string)
       (let ((issue-number (match-string 1 string)))
	 (if issue-number
           (cons string issue-number)))))


 (defun usersplit-link-recognizer (string)
   (if (string-match "https?://\.*?/usersplit/split-tests\.*?id=\\([0-9]*\\)" string)
       (let ((issue-number (match-string 1 string)))
	 (if issue-number
             (cons string (concat "UserSplit-" issue-number))))
     (if (string-match "https?://\.*?/usersplit/test\.*?id=\\([0-9]*\\)" string)
	 (let ((issue-number (match-string 1 string)))
	   (if issue-number
               (cons string (concat "UserSplit-" issue-number)))))
     ))

 (defun usergroup-link-recognizer (string)
   (if (string-match "https?://\.*?/usersplit/user-group\.*?id=\\([0-9]*\\)" string)
       (let ((issue-number (match-string 1 string)))
	 (if issue-number
           (cons string (concat "UserGroup-" issue-number))))))

 (defun qaapi-link-recognizer (string)
   (if (string-match "https://qaapi.\[a-z]*.com/\\([a-zA-Z0-9]*\\)?\.*" string)
       (let ((method-name (match-string 1 string)))
	 (if method-name
           (cons string (concat "qaapi::" method-name))))))

(defun qaapi-link-with-user-recognizer (possible_link)
  (if (string-match "https://qaapi.\[a-z]*.com/\\([a-zA-Z0-9]*\\)?\.*user_id=\\([0-9]*\\)?\.*" possible_link)
      (let (
	    (qaapi_name (match-string 1 possible_link))
	    (qaapi_user_id (match-string 2 possible_link))
	    )
      (and qaapi_name qaapi_user_id
	   (cons possible_link (concat "qaapi::" qaapi_name " (user_id=" qaapi_user_id ")"))))))

(defvar recognized-domains '(
    ("https://docs.google.com/*" "Google Doc")
    ("https://www.youtube.com/*" "YouTube")
    ("https://bumble.slack.com/*" "Slack")
    ("https://vpn-eu1.staffpass.com/gelato/*" "Gelato")
    ("https://allegro.pl/*" "Allegro")
    ("https://www.amazon.pl/*" "Amazon")
    ("https://www.figma.com/*" "Figma")
    ("https://miro.com/app/*" "Miro")
    ("https://bmbl.atlassian.net/wiki/*" "Wiki (Atlassian)")
    ("https://stackoverflow.com/*" "Stack")
    ("https://mobile-ci.bumble.dev/buildConfiguration/*" "TeamCity")
    ("https://bumble.sentry.io/*" "Sentry")
    ("https://console.cloud.google.com/cloud-build/*" "Cloud Build")
))

(defun mapped-domain-link-recognizer-domains (link domains)
  (let ((head (car domains)))
    (if head
	(if (string-match (car head) link)
	    head
	  (mapped-domain-link-recognizer-domains link (cdr domains))
    )
  )))

(defun mapped-domain-link-recognizer (string)
  (let ((found_mapping (mapped-domain-link-recognizer-domains string recognized-domains)))
    (if found_mapping
      (cons string (car (cdr found_mapping)))
    )
  )
)

(defun badoo-jira-wiki-link-recognizer (string)
  (if (string-match "https://wiki.\[a-z]*.com/.+/\\([a-zA-Z0-9+-]*\\)+\?\.*" string)
      (let ((page-name (match-string 1 string)))
	(if page-name
          (cons string (concat "WIKI:" (replace-regexp-in-string "+" " " page-name)))))))

(defun mobiledoc-link-recognizer (string)
  (if (string-match "http://\\(mapi-\\([0-9]+\\)+.\\)?mobiledoc.badoojira.com*" string)
    (let ((mapi-ticket (match-string 2 string)))
      (if mapi-ticket
	 (cons string (concat "MAPI (" mapi-ticket ")"))
	 (cons string "MAPI"))
      )
    )
  )

;;; GitHub ---------------------------------------------------------------------

(defvar github-host-prefixes '(("github.com" . "gh")
                               ("github.bumble.dev" . "ghe"))
  "Alist of known GitHub hosts and the prefix used in their descriptions.")

(defun github-host-prefix (host)
  "Description prefix for HOST, or nil when HOST is not a known GitHub host."
  (cdr (assoc host github-host-prefixes)))

(defun github-url-parts (string)
  "Split STRING into (PREFIX OWNER REPO PATH ANCHOR) when it is a GitHub URL.

PREFIX is the short host label (see `github-host-prefixes'), PATH is
everything after `OWNER/REPO/' with the query string and the anchor
stripped (empty string for a repository root) and ANCHOR is the `#...'
fragment without the hash (nil when absent).  Returns nil for URLs that
are not hosted on a known GitHub host."
  (when (string-match
         "\\`https?://\\([^/?#]+\\)/\\([^/?#]+\\)/\\([^/?#]+\\)\\(?:/\\([^?#]*\\)\\)?\\(?:\\?[^#]*\\)?\\(?:#\\(.*\\)\\)?\\'"
         string)
    (let ((prefix (github-host-prefix (match-string 1 string)))
          (owner (match-string 2 string))
          (repo (match-string 3 string))
          (path (or (match-string 4 string) ""))
          (anchor (match-string 5 string)))
      (if prefix (list prefix owner repo path anchor)))))

(defun github-blob-label (path anchor)
  "Label for a blob PATH like `blob/main/dir/File.swift' with line ANCHOR."
  (let ((file (file-name-nondirectory path))
        (lines (if (and anchor
                        (string-match "\\`L\\([0-9]+\\)\\(?:-L\\([0-9]+\\)\\)?" anchor))
                   (if (match-string 2 anchor)
                       (concat (match-string 1 anchor) "-" (match-string 2 anchor))
                     (match-string 1 anchor)))))
    (if lines (concat file ":" lines) file)))

(defun github-path-label (path anchor)
  "Describe PATH (with ANCHOR) inside a GitHub repository.
Returns nil for a repository root and for paths with no known shape, so
callers fall back to the plain `PREFIX/OWNER/REPO' description."
  (cond
   ((string-match "\\`pull/\\([0-9]+\\)" path)
    (concat "PR#" (match-string 1 path)))
   ((string-match "\\`actions/runs/\\([0-9]+\\)/job/\\([0-9]+\\)" path)
    (concat "run#" (match-string 1 path) " job#" (match-string 2 path)))
   ((string-match "\\`actions/runs/\\([0-9]+\\)/attempts/\\([0-9]+\\)" path)
    (concat "run#" (match-string 1 path) " (attempt " (match-string 2 path) ")"))
   ((string-match "\\`actions/runs/\\([0-9]+\\)" path)
    (concat "run#" (match-string 1 path)))
   ((string-match "\\`actions/jobs/\\([0-9]+\\)" path)
    (concat "job#" (match-string 1 path)))
   ((string-match "\\`actions/workflows/\\([^/]+\\)" path)
    (concat "workflow " (match-string 1 path)))
   ((string-match "\\`actions" path) "actions")
   ((string-match "\\`issues/\\([0-9]+\\)" path)
    (concat "issue#" (match-string 1 path)))
   ((string-match "\\`discussions/\\([0-9]+\\)" path)
    (concat "discussion#" (match-string 1 path)))
   ((string-match "\\`commits?/\\([0-9a-f]\\{7,40\\}\\)" path)
    (concat "commit " (substring (match-string 1 path) 0 7)))
   ((string-match "\\`compare/\\([^/]+\\)" path)
    (concat "compare " (match-string 1 path)))
   ((string-match "\\`releases/tag/\\([^/]+\\)" path)
    (concat "release " (match-string 1 path)))
   ((string-match "\\`releases" path) "releases")
   ((string-match "\\`blob/" path) (github-blob-label path anchor))
   ((string-match "\\`tree/\\(.+\\)" path)
    (concat "tree " (match-string 1 path)))
   ((string-match "\\`wiki/\\(.+\\)" path)
    (concat "wiki " (replace-regexp-in-string "-" " " (match-string 1 path))))
   ((string-match "\\`projects/\\([0-9]+\\)" path)
    (concat "project#" (match-string 1 path)))
   ((string-match "\\`milestone/\\([0-9]+\\)" path)
    (concat "milestone#" (match-string 1 path)))))

(defun github-link-recognizer (string)
  "Recognize repository level GitHub links on github.com and GitHub Enterprise.
Pull requests, Actions runs and jobs, issues, discussions, commits,
compares, releases, files, trees and wiki pages get a dedicated label;
anything else falls back to `PREFIX/OWNER/REPO'."
  (if-let ((parts (github-url-parts string)))
      (let* ((prefix (nth 0 parts))
             (owner (nth 1 parts))
             (repo (nth 2 parts))
             (label (github-path-label (nth 3 parts) (nth 4 parts)))
             (base (concat prefix "/" owner "/" repo)))
        (cons string (if label (concat base "/" label) base)))))

(defun github-org-link-recognizer (string)
  "Recognize organisation level GitHub links, e.g. `/orgs/ORG/projects/3'."
  (if-let ((parts (github-url-parts string)))
      (let ((prefix (nth 0 parts))
            (owner (nth 2 parts))
            (path (nth 3 parts)))
        (if (string-equal (nth 1 parts) "orgs")
            (cons string
                  (if (string-match "\\`projects/\\([0-9]+\\)" path)
                      (concat prefix "/" owner "/project#" (match-string 1 path))
                    (concat prefix "/" owner)))))))

(defun github-owner-link-recognizer (string)
  "Recognize a bare GitHub owner (user or organisation) page."
  (if (string-match "\\`https?://\\([^/?#]+\\)/\\([^/?#]+\\)/?\\(?:[?#].*\\)?\\'" string)
      (let ((prefix (github-host-prefix (match-string 1 string)))
            (owner (match-string 2 string)))
        (if prefix (cons string (concat prefix "/" owner))))))

(defun github-gist-link-recognizer (string)
  "Recognize a gist link, keeping the owner and a short gist id."
  (if (string-match "\\`https?://gist\\.github\\.com/\\([a-zA-Z0-9_-]+\\)/\\([0-9a-f]+\\)" string)
      (let ((owner (match-string 1 string))
            (gist-id (match-string 2 string)))
        (cons string (concat "gist/" owner "/"
                             (substring gist-id 0 (min 7 (length gist-id))))))))

(defun teamcity-link-recognizer (string)
  (if (string-match "https?:\/\/mobile-ci\.bumble\.dev\/buildConfiguration\/\\([a-zA-Z0-9_]+\\)\/\\([0-9]+\\)" string)
      (if-let ((build-type (match-string 1 string))
               (build-id (match-string 2 string)))
          (cons string (concat "TeamCity/" build-type "/" build-id)))))

(defun github-enterprise-pull-link-recognizer (string)
  "Recognize GitHub Enterprise pull requests only.
Superseded by `github-link-recognizer', which returns the same
description for these URLs and also handles github.com and every other
GitHub URL shape.  Kept as a narrow entry point."
  (if-let ((parts (github-url-parts string)))
      (if (and (string-equal (nth 0 parts) "ghe")
               (string-match "\\`pull/[0-9]+" (nth 3 parts)))
          (github-link-recognizer string))))

(defvar org-link-recognizers '(
     teamcity-link-recognizer
     mapped-domain-link-recognizer
     jira-link-recognizer
     usersplit-link-recognizer
     usergroup-link-recognizer
     qaapi-link-with-user-recognizer
     qaapi-link-recognizer
     badoo-jira-wiki-link-recognizer
     mobiledoc-link-recognizer
     github-gist-link-recognizer
     github-org-link-recognizer
     github-link-recognizer
     github-owner-link-recognizer
))

(defun find-recognizer (value recognizers)
  (let ((head (car recognizers)))
    (if head
      (let ((match (funcall head value)))
        (if match
          match
          (find-recognizer value (cdr recognizers))
	  )
	)
      )
    )
  )

 (defun insert-link-interceptor-for-value (value)
   (let ((found_recognizer (find-recognizer value org-link-recognizers)))
     (if found_recognizer
	 found_recognizer)
     )
   )

;; I needed to pass function in #' manner: https://emacs.stackexchange.com/questions/32753/call-interactive-function-from-elisp-code-without-worrying-about-arguments

 (defun org-insert-link-interceptor ()
   "If it could recognizer the link from the clipboard it'll prettify and paste it"
   (interactive)
   (let ((clipboard (shell-command-to-string "pbpaste")))
     (let ((match (insert-link-interceptor-for-value clipboard)))
       (if match
         (insert (org-make-link-string (car match) (cdr match)))
	 (funcall-interactively #'org-insert-link)
	 )
       )
     )
   )
