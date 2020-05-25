;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; Copyright (c) 2013, 2014, Jan Moringen. All rights reserved.
;;;
;;; License: Apache-2.0
;;;

(in-package #:log4cl)

(defclass syslog-appender (appender)
  ((name :initarg :name :type string
         :accessor syslog-appender-name)
   (include-pid? :initarg :include-pid? :type boolean
                 :accessor syslog-appender-include-pid?)
   (facility :initarg :facility :type keyword
	     :accessor syslog-appender-facility))
  (:default-initargs
   :layout (make-instance 'pattern-layout :conversion-pattern "%m")
    :name (lisp-implementation-type)
    :include-pid? t
    :facility :local0)
  (:documentation
   "An appender that writes log messages to the syslog.

The identity of the syslog connection is controlled by the :name
initarg and defaults to

  (lisp-implementation-type)

The :include-pid? initarg controls whether log entries produced by the
syslog connection should include the process id (PID). The default is
true.

The :facility initarg provides the syslog facility as a keyword (:local0 ..
local7)."))

(defmethod property-alist ((instance syslog-appender))
  (append (call-next-method)
          '((:name name :string-skip-whitespace)
            (:include-pid? include-pid? boolean)
	    (:facility facility keyword))))
