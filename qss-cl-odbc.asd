;;; -*- Mode: Lisp -*-

(asdf:defsystem "qss-cl-odbc"
  :description "ODBC 3.5 for Common Lisp: Unicode, 64 bit, Windows/Linux/macOS"
  :author "Roland Averkamp (2005); QSS / Friedrich Dominicus (2026)"
  :license "BSD"
  :depends-on (:cffi :babel)            ; babel: byte lengths of text encodings
  :components ((:module "odbc"
                :pathname "src/odbc/"
                :serial t
                :components ((:file "package")
                             (:file "global")
                             (:file "odbc-library")
                             (:file "odbc-api-generated") ; from odbc-headers/, see tools/
                             (:file "ffi-support")
                             (:file "dates")
                             (:file "odbc-calls")
                             (:file "parameter")
                             (:file "column")
                             (:file "odbc-main")
                             (:file "odbc-utilities"))))
  :in-order-to ((asdf:test-op (asdf:test-op "qss-cl-odbc-test"))))
