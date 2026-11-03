;;; -*- Mode: Lisp -*-
;;; Optional: regenerate src/odbc/odbc-api-generated.lisp from the ODBC headers
;;; (odbc-headers/) and check it against a C compiler. Not needed to load
;;; qss-cl-odbc.
;;;   (asdf:load-system :qss-cl-odbc-generate)
;;;   (qss-cl-odbc-generate:generate)
;;;   (qss-cl-odbc-generate:check :cc "gcc")      ; needs qss-cl-odbc loaded

(asdf:defsystem "qss-cl-odbc-generate"
  :description "Generator for the ODBC declarations of qss-cl-odbc"
  :license "BSD"
  :depends-on (:cffi :uiop)
  :components ((:module "tools"
                :serial t
                :components ((:file "generate-odbc-api")
                             (:file "check-odbc-api")))))
