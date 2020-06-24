
;; Eval checks with C-M-x

(load-file "qjira.el")
(let (
      (t1 (qjira-ticket-validation "IOS-11111"))
      (t2 (qjira-ticket-validation "SRV-11111")) ;; not recognized project
      )
      (assert (equal "IOS-11111" t1))
      (assert (equal nil t2))
  )
