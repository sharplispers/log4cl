(log4cl-test:defsubsuite :log4cl-test.regressions)
(log4cl-test:subsuite-start)

(deftest join-thread-error.issue#1 ()
  "https://github.com/sharplispers/log4cl/issues/1"
  (log4cl:start-hierarchy-watcher-thread)
  (finishes (log4cl-impl::save-hook)))

(deftest recursive-lock.issue#8.1 ()
  "https://github.com/sharplispers/log4cl/issues/8"
  (finishes
    (with-simple-restart (abort "Abort")
      (handler-bind
          ((error (lambda (condition)
                    (log:error "caught error: ~A" condition)
                    (abort))))
        (log:warn "~@{~A ~A~}" 1)))))

(defun logging-function ()
  (log:warn "I log, too")
  :result)

(deftest recursive-lock.issue#8.2 ()
  "https://github.com/sharplispers/log4cl/issues/8"
  (finishes (log:warn "~A" (logging-function))))
