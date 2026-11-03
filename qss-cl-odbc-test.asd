;;; -*- Mode: Lisp -*-
;;; QSS 2026-10-13: Tests fuer qss-cl-odbc mit FiveAM
;;; (asdf:test-system :qss-cl-odbc)  bzw.  (asdf:test-system :qss-cl-odbc-test)
;;; DSN fuer die Integrationstests: Umgebungsvariable QSS_ODBC_DSN

(asdf:defsystem "qss-cl-odbc-test"
  :description "Unit- und Integrationstests fuer qss-cl-odbc"
  :depends-on (:qss-cl-odbc :qss-cl-odbc-generate :fiveam)
  :pathname "src/test/"
  :components ((:file "qss-tests")
               (:file "chinook-tests")       ; liest chinook/expected.sexp
               (:static-file "chinook/expected.sexp")
               (:static-file "chinook/Chinook_Sqlite.sqlite"))
  :perform (asdf:test-op (o c)
             (unless (uiop:symbol-call :qss-cl-odbc-test :run-all)
               (error "qss-cl-odbc: Tests fehlgeschlagen"))))
