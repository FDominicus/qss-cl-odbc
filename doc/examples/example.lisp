;;; -*- Mode: Lisp -*-
;;; qss-cl-odbc documentation, example (b2): what example.c does, with
;;; qss-cl-odbc. Errors are signalled as QSS-CL-ODBC:SQL-ERROR; handles,
;;; buffers, encodings and diagnostics are handled by qss-cl-odbc.
;;;
;;;   (asdf:load-system :qss-cl-odbc)
;;;   (load "doc/examples/example.lisp")
;;;   (example "EXAMPLE")
;;;
;;; Copyright (c) 2026 QSS / Friedrich Dominicus, BSD license (see license.txt)

(defun example (&optional (dsn "EXAMPLE"))
  (let ((con (qss-cl-odbc:connect-generic :dsn dsn)))
    (unwind-protect
         (progn
           ;; PostgreSQL answers with a notice when the table does not exist:
           ;; a QSS-CL-ODBC:SQL-WARNING, muffled here
           (handler-bind ((qss-cl-odbc:sql-warning #'muffle-warning))
             (qss-cl-odbc:exec-command con "DROP TABLE IF EXISTS person"))
           (qss-cl-odbc:exec-command con "CREATE TABLE person (id INTEGER PRIMARY KEY, name VARCHAR(50))")
           ;; prepared INSERT with two parameters, executed twice
           (qss-cl-odbc:with-prepared-statement
               (insert con "INSERT INTO person (id, name) VALUES (?, ?)"
                       '(:integer :in) '(:unicode-string :in 50))
             (loop for name in '("Ana" "Bo")
                   for id from 1
                   do (qss-cl-odbc:exec-prepared-update insert id name)))
           (qss-cl-odbc:commit con)
           ;; SELECT, row by row
           (qss-cl-odbc:do-query ((id name) con "SELECT id, name FROM person ORDER BY id")
             (format t "~D: ~A~%" id name))
           ;; a failing statement
           (handler-case (qss-cl-odbc:exec-query con "SELECT * FROM no_such_table")
             (qss-cl-odbc:sql-error (e)
               (format t "error ~A: ~A~%" (qss-cl-odbc:sql-condition-state e)
                       (qss-cl-odbc:sql-condition-message e))))
           (qss-cl-odbc:exec-command con "DROP TABLE person")
           (qss-cl-odbc:commit con))
      (qss-cl-odbc:close-connection con)))
  (values))
