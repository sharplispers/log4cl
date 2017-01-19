(log4cl-test:defsubsuite :log4cl-test.regressions)
(log4cl-test:subsuite-start)

(deftest join-thread-error.issue#1 ()
  "https://github.com/sharplispers/log4cl/issues/1"
  (log4cl:start-hierarchy-watcher-thread)
  (finishes (log4cl-impl::save-hook)))
