
;; Eval checks with C-M-x

(load-file "link_recognizer.el")

;;(setq debug-on-error t)

(let (
      (t1 (jira-link-recognizer "https://jira.badoojira.com/browse/IOS-33361"))
      )
      (assert (equal '("https://jira.badoojira.com/browse/IOS-33361" . "IOS-33361") t1))
      )

;; usergroup-link-recognizer

(let (
      (t1 (usergroup-link-recognizer "https://vpn-eu1.staffpass.com/usersplit/user-groups/object.phtml?id=2081"))
      (t2 (usergroup-link-recognizer "https://vpn-eu1.staffpass.com/usersplit/test.phtml?id=9595"))      
      )
      (assert (equal '("https://vpn-eu1.staffpass.com/usersplit/user-groups/object.phtml?id=2081" . "UserGroup-2081") t1))
      (assert (equal nil t2))
  )

;; usersplit-link-recognizer

(let (
      (t1 (usersplit-link-recognizer "https://vpn-eu1.staffpass.com/usersplit/test.phtml?id=9595"))
      (t2 (usersplit-link-recognizer "https://vpn-eu1.staffpass.com/usersplit/user-groups/object.phtml?id=2081"))      
      )
      (assert (equal '("https://vpn-eu1.staffpass.com/usersplit/test.phtml?id=9595" . "UserSplit-9595") t1))
      (assert (equal nil t2))
      )

;; mapped-domain-link-recognizer

;; (car (car recognized-domains))
;; (cdr recognized-domains)
(mapped-domain-link-recognizer "https://www.youtube.com/watch?v=XNgRgBP0chY")

(let (
      (t1 (mapped-domain-link-recognizer "https://www.youtube.com/watch?v=XNgRgBP0chY"))
      (t2 (mapped-domain-link-recognizer "https://www.onet.pl"))      
      )
      (assert (equal '("https://www.youtube.com/watch?v=XNgRgBP0chY" . "YouTube") t1))
      (assert (equal nil t2))
      )

;; org-insert-link-interceptor

;; (shell-command-to-string "pbpaste")
;; (funcall-interactively #'org-insert-link)

(insert-link-interceptor-for-value "https://vpn-eu1.staffpass.com/usersplit/test.phtml?id=9595")
(insert-link-interceptor-for-value "https://www.youtube.com/watch?v=RQ0f7dBcDtI")

(let (
      (t1 (insert-link-interceptor-for-value "https://vpn-eu1.staffpass.com/usersplit/test.phtml?id=9595"))
      (t2 (insert-link-interceptor-for-value "https://www.youtube.com/watch?v=RQ0f7dBcDtI"))      
      )
      (assert (equal '("https://vpn-eu1.staffpass.com/usersplit/test.phtml?id=9595" . "UserSplit-9595") t1))
      (assert (equal nil t2))
  )
