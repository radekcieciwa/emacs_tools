
;; Eval checks with C-M-x

(load-file "qjira.el")
(let (
      (t1 (qjira-ticket-validation "IOS-11111"))
      (t2 (qjira-ticket-validation "SRV-11111")) ;; not recognized project
      )
      (assert (equal "IOS-11111" t1))
      (assert (equal nil t2))
  )

;; retry-with-timeouts: succeeds on first attempt

(let (
      (calls 0)
      (result nil)
      )
      (setq result (retry-with-timeouts
                    (lambda () (setq calls (1+ calls)) "ok")
                    '(1 1)))
      (assert (equal "ok" result))
      (assert (equal 1 calls)) ;; no retry needed
  )

;; retry-with-timeouts: falls back to next attempt after an error

(let (
      (calls 0)
      (result nil)
      )
      (setq result (retry-with-timeouts
                    (lambda ()
                      (setq calls (1+ calls))
                      (if (= calls 1)
                          (error "boom") ;; first attempt fails
                        "recovered"))    ;; second attempt succeeds
                    '(1 1)))
      (assert (equal "recovered" result))
      (assert (equal 2 calls)) ;; retried once
  )

;; retry-with-timeouts: falls back to next attempt after a timeout (nil)

(let (
      (calls 0)
      (result nil)
      )
      (setq result (retry-with-timeouts
                    (lambda ()
                      (setq calls (1+ calls))
                      (if (= calls 1)
                          nil          ;; simulate timeout / empty result
                        "recovered"))
                    '(1 1)))
      (assert (equal "recovered" result))
      (assert (equal 2 calls))
  )

;; retry-with-timeouts: signals an error once all attempts are exhausted

(let (
      (calls 0)
      (errored nil)
      )
      (condition-case _err
          (retry-with-timeouts
           (lambda () (setq calls (1+ calls)) (error "always fails"))
           '(1 1))
        (error (setq errored t)))
      (assert (equal t errored))
      (assert (equal 2 calls)) ;; one attempt per timeout entry
  )
