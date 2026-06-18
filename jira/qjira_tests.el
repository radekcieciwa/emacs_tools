
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
                    (lambda (_timeout) (setq calls (1+ calls)) "ok")
                    '(1 1)))
      (assert (equal "ok" result))
      (assert (equal 1 calls)) ;; no retry needed
  )

;; retry-with-timeouts: passes the per-attempt timeout to the thunk

(let (
      (seen nil)
      )
      (retry-with-timeouts
       (lambda (timeout) (setq seen timeout) "ok")
       '(7 11))
      (assert (equal 7 seen)) ;; first attempt's timeout handed in
  )

;; retry-with-timeouts: falls back to next attempt after an error

(let (
      (calls 0)
      (result nil)
      )
      (setq result (retry-with-timeouts
                    (lambda (_timeout)
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
                    (lambda (_timeout)
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
           (lambda (_timeout) (setq calls (1+ calls)) (error "always fails"))
           '(1 1))
        (error (setq errored t)))
      (assert (equal t errored))
      (assert (equal 2 calls)) ;; one attempt per timeout entry
  )

;; retry-with-timeouts: on-failure runs after each failed attempt, not after success

(let (
      (calls 0)
      (resets 0)
      )
      (retry-with-timeouts
       (lambda (_timeout)
         (setq calls (1+ calls))
         (if (= calls 1) nil "recovered")) ;; fail once, then succeed
       '(1 1)
       "operation"
       (lambda () (setq resets (1+ resets))))
      (assert (equal 1 resets)) ;; reset after the single failed attempt only
  )

;; retry-with-timeouts: on-failure runs for every failed attempt when all fail

(let (
      (resets 0)
      )
      (condition-case _err
          (retry-with-timeouts
           (lambda (_timeout) nil) ;; always times out
           '(1 1)
           "operation"
           (lambda () (setq resets (1+ resets))))
        (error nil))
      (assert (equal 2 resets)) ;; one reset per failed attempt
  )
