
 (defun jira-link-recognizer (string)
   (if (string-match "https?://jira\.*?/browse/\\([a-zA-Z]*-[0-9]*\\)" string)
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
    ("https://github.com/*" "GitHub")
    ("https://allegro.pl/*" "Allegro")
    ("https://www.amazon.pl/*" "Amazon")
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

(defvar org-link-recognizers '(
     mapped-domain-link-recognizer
     jira-link-recognizer
     usersplit-link-recognizer
     usergroup-link-recognizer
     qaapi-link-with-user-recognizer
     qaapi-link-recognizer
     badoo-jira-wiki-link-recognizer
     mobiledoc-link-recognizer
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
