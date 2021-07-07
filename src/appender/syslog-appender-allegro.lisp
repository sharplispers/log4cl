;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; Copyright (c) 2020, Frank Goenninger <frank.goenninger@goenninger.net>
;;; All rights reserved.
;;;
;;; Based on syslog-appender-sbcl.lisp by Jan Moringen, a.k.a. scymtym.
;;; License: Apache-2.0
;;;
(in-package #:log4cl)

(defmethod appender-do-append ((appender syslog-appender) logger level log-func)
  (let* ((syslog-level (%log4cl-level->syslog-level level))
         (layout       (appender-layout appender))
         (message
          (with-output-to-string (stream)
            (layout-to-stream layout stream logger level log-func))))
    (excl.osi:openlog (syslog-appender-name appender)
                      (logior excl.osi:*log-user*
			      (if (syslog-appender-include-pid? appender)
                                  excl.osi:*log-pid*
				  0))
		      (ensure-facilty-value (syslog-appender-facility appender)))
    (excl.osi:syslog syslog-level "~A" message)
    (excl.osi:closelog)))

;; Utility functions

(defun %log4cl-level->syslog-level (level)
  (let ((level/keyword (aref +log-level-to-keyword+ level)))
    (ecase level/keyword
      (:fatal excl.osi:*log-crit*)
      (:error excl.osi:*log-err*)
      (:warn  excl.osi:*log-warning*)
      (:info  excl.osi:*log-info*)
      ((:debu1 :debu2 :debu3 :debu4 :debu5 :debu6 :debu7 :debu8 :debu9)
       excl.osi:*log-debug*))))

(defun ensure-facilty-value (facility)
  (let ((facility-symbol (intern (string-upcase (format nil "~A" facility)) 'keyword)))
    (case facility-symbol
      (:local0       excl.osi:*log-local0*)
      (:local1       excl.osi:*log-local1*)
      (:local2       excl.osi:*log-local2*)
      (:local3       excl.osi:*log-local3*)
      (:local4       excl.osi:*log-local4*)
      (:local5       excl.osi:*log-local5*)
      (:local6       excl.osi:*log-local6*)
      (:local7       excl.osi:*log-local7*)
      (:log-auth     excl.osi:*log-auth*)
      (:log-authpriv excl.osi:*log-authpriv*)
      (:log-cron     excl.osi:*log-cron*)
      (:log-daemon   excl.osi:*log-daemon*)
      (:log-ftp      excl.osi:*log-ftp*)
      (:log-kern     excl.osi:*log-kern*)
      (:log-lpr      excl.osi:*log-lpr*)
      (:log-mail     excl.osi:*log-mail*)
      (:log-news     excl.osi:*log-news*)
      (:log-syslog   excl.osi:*log-syslog*)
      (:log-user     excl.osi:*log-user*)
      (:log-uucp     excl.osi:*log-uucp*)
      (otherwise     excl.osi:*log-user*))))
