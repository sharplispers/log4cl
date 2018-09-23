;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; Copyright (c) 2013, 2014, Jan Moringen. All rights reserved.
;;;
;;; License: Apache-2.0
;;;
(in-package #:log4cl)

(defmethod appender-do-append ((appender syslog-appender) logger level log-func)
  (let* ((syslog-level (%log4cl-level->syslog-level level))
         (layout       (appender-layout appender))
         (message
           (with-output-to-string (stream)
             (layout-to-stream layout stream logger level log-func))))
    (sb-posix:openlog (syslog-appender-name appender)
                      (logior sb-posix:log-user
                              (if (syslog-appender-include-pid? appender)
                                  sb-posix:log-pid 0)))
    (sb-posix:syslog syslog-level "~A" message)
    (sb-posix:closelog)))

;; Utility functions

(defun %log4cl-level->syslog-level (level)
  (let ((level/keyword (aref +log-level-to-keyword+ level)))
    (ecase level/keyword
      (:fatal sb-posix:log-crit)
      (:error sb-posix:log-err)
      (:warn  sb-posix:log-warning)
      (:info  sb-posix:log-info)
      ((:debu1 :debu2 :debu3 :debu4 :debu5 :debu6 :debu7 :debu8 :debu9)
       sb-posix:log-debug))))
