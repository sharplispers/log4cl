;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; Copyright (c) 2012, Max Mikhanosha. All rights reserved.
;;;
;;; This file is licensed to You under the Apache License, Version 2.0
;;; (the "License"); you may not use this file except in compliance
;;; with the License.  You may obtain a copy of the License at
;;; http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(asdf:defsystem "log4cl"
  :version "1.1.3"
  :depends-on ("bordeaux-threads"
               #+sbcl "sb-posix") ; for SB-POSIX:GETPID in pattern-layout.lisp
  :components ((:module "src"
                :serial t
                :components ((:file "impl-package")
                             (:file "defs")
                             (:file "naming")
                             #+sbcl (:file "naming-sbcl")
                             #+ccl (:file "naming-ccl")
                             (:file "hierarchy-base")
                             (:file "hierarchy")
                             (:file "logger")
                             (:file "logging-macros")
                             (:file "self-logger")))
               (:module "appender"
                :pathname "src/appender"
                :depends-on ("src")
                :serial t
                :components ((:file "layout")
                             (:file "simple-layout")
                             (:file "pattern-layout")

                             (:file "appender-base")
                             (:file "appender")))
               (:module "configuration"
                :pathname "src"
                :depends-on ("src" "appender")
                :serial t
                :components ((:file "configurator")
                             (:file "property-parser")
                             (:file "property-configurator")))
               (:module "watcher"
                :pathname "src"
                :depends-on ("src" "appender")
                :components ((:file "watcher")))
               (:module "client-package"
                :pathname "src"
                :depends-on ("src" "configuration")
                :components ((:file "package"))))
  :in-order-to ((test-op (test-op "log4cl/test"))))

(defmethod perform :after ((op load-op) (system (eql (find-system "log4cl"))))
  (let ((package (find-package '#:log4cl)))
    (when package
      (let ((*package* package)
            (foo (find-symbol (symbol-name '#:%fix-root-logger-check))))
        (when foo
          (funcall foo)))))
  (values))

(asdf:defsystem "log4cl/syslog"
  :version "1.1.3"
  :depends-on ("log4cl"
               #-sbcl "cl-syslog")
  :components ((:module "appender"
                :pathname "src/appender"
                :serial t
                :components ((:file "syslog-appender")
                             #+sbcl (:file "syslog-appender-sbcl")
                             #-sbcl (:file "syslog-appender-cffi")))))

(asdf:defsystem "log4cl/test"
  :version "1.1.3"
  :depends-on ("log4cl" "stefil")
  :components ((:module "tests"
                :serial t
                :components ((:file "test-defs")
                             (:file "test-logger")
                             (:file "test-category-separator")
                             (:file "test-layouts")
                             (:file "test-appenders")
                             (:file "test-configurator")
                             (:file "test-speed")
                             (:file "test-file-category")
                             (:file "test-compat")
                             (:file "test-regressions")))))

(defmethod perform ((op test-op) (system (eql (find-system :log4cl/test))))
  (let ((*package* (find-package :log4cl-test)))
    (eval (read-from-string "(stefil:funcall-test-with-feedback-message 'log4cl-test::test)")))
  (values))
