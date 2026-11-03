;;; -*- Mode: Lisp -*-
;;; qss-cl-odbc documentation, example (b1): example.c translated line by line
;;; to Common Lisp, on the declarations generated from the ODBC headers
;;; (%%SQL-... = the C function, $SQL_... = the C constant). This is the level
;;; below qss-cl-odbc; normally you write example.lisp instead.
;;;
;;;   (asdf:load-system :qss-cl-odbc)
;;;   (load "doc/examples/example-raw.lisp")
;;;   (qss-cl-odbc::example-raw "EXAMPLE")
;;;
;;; Copyright (c) 2026 QSS / Friedrich Dominicus, BSD license (see license.txt)

(in-package :qss-cl-odbc)

(defun example-check (rc type handle what)
  "check() of example.c: on SQL_ERROR print all diagnostic records and stop."
  (unless (member rc (list $SQL_SUCCESS $SQL_SUCCESS_WITH_INFO $SQL_NO_DATA))
    (cffi:with-foreign-objects ((state :uint16 6) (native 'sql-integer)
                                (message :uint16 1024) (len 'sql-small-int))
      (loop for rec from 1
            while (member (%%sql-get-diag-rec-w type handle rec state native message 1024 len)
                          (list $SQL_SUCCESS $SQL_SUCCESS_WITH_INFO))
            do (format *error-output* "~A: [~A] ~A~%" what
                       (get-wide-string-nts state) (get-wide-string-nts message))))
    (error "~A failed" what)))

(defun example-raw (&optional (dsn "EXAMPLE"))
  (cffi:with-foreign-objects ((penv :pointer) (pdbc :pointer) (pstmt :pointer))
    ;; environment and connection
    (example-check (%%sql-alloc-handle $SQL_HANDLE_ENV (cffi:null-pointer) penv)
                   $SQL_HANDLE_ENV (cffi:null-pointer) "env")
    (let ((env (cffi:mem-ref penv :pointer)))
      (example-check (%%sql-set-env-attr env $SQL_ATTR_ODBC_VERSION (cffi:make-pointer $SQL_OV_ODBC3) 0)
                     $SQL_HANDLE_ENV env "version")
      (example-check (%%sql-alloc-handle $SQL_HANDLE_DBC env pdbc) $SQL_HANDLE_ENV env "dbc")
      (let ((dbc (cffi:mem-ref pdbc :pointer)))
        (with-wide-string (connection (format nil "DSN=~A" dsn))
          (example-check (%%sql-driver-connect-w dbc (cffi:null-pointer) connection $SQL_NTS
                                                 (cffi:null-pointer) 0 (cffi:null-pointer)
                                                 $SQL_DRIVER_NOPROMPT)
                         $SQL_HANDLE_DBC dbc "connect"))
        (example-check (%%sql-alloc-handle $SQL_HANDLE_STMT dbc pstmt) $SQL_HANDLE_DBC dbc "stmt")
        (let ((stmt (cffi:mem-ref pstmt :pointer)))
          (flet ((exec (sql what)
                   (with-wide-string (text sql)
                     (example-check (%%sql-exec-direct-w stmt text $SQL_NTS)
                                    $SQL_HANDLE_STMT stmt what))))
            ;; a table
            (with-wide-string (text "DROP TABLE IF EXISTS person")
              (%%sql-exec-direct-w stmt text $SQL_NTS))
            (exec "CREATE TABLE person (id INTEGER PRIMARY KEY, name VARCHAR(50))" "create")

            ;; prepared INSERT with two parameters, executed twice
            (cffi:with-foreign-objects ((id 'sql-integer) (name :uint16 51)
                                        (id-ind 'sql-len) (name-ind 'sql-len))
              (setf (cffi:mem-ref id-ind 'sql-len) 0
                    (cffi:mem-ref name-ind 'sql-len) $SQL_NTS)
              (with-wide-string (text "INSERT INTO person (id, name) VALUES (?, ?)")
                (example-check (%%sql-prepare-w stmt text $SQL_NTS) $SQL_HANDLE_STMT stmt "prepare"))
              (example-check (%%sql-bind-parameter stmt 1 $SQL_PARAM_INPUT $SQL_C_SLONG $SQL_INTEGER
                                                   0 0 id 0 id-ind)
                             $SQL_HANDLE_STMT stmt "bind 1")
              (example-check (%%sql-bind-parameter stmt 2 $SQL_PARAM_INPUT $SQL_C_WCHAR $SQL_WVARCHAR
                                                   50 0 name (* 2 51) name-ind)
                             $SQL_HANDLE_STMT stmt "bind 2")
              (loop for n in '("Ana" "Bo")
                    for i from 1
                    do (setf (cffi:mem-ref id 'sql-integer) i)
                       (%put-unicode-string name n) ; UTF-16 with terminating 0
                       (example-check (%%sql-execute stmt) $SQL_HANDLE_STMT stmt "insert")))
            (example-check (%%sql-free-stmt stmt $SQL_RESET_PARAMS) $SQL_HANDLE_STMT stmt "reset")
            (example-check (%%sql-end-tran $SQL_HANDLE_DBC dbc $SQL_COMMIT) $SQL_HANDLE_DBC dbc "commit")

            ;; SELECT, read row by row with SQLGetData
            (exec "SELECT id, name FROM person ORDER BY id" "select")
            (cffi:with-foreign-objects ((rid 'sql-integer) (rname :uint16 51) (ind 'sql-len))
              (loop while (= (%%sql-fetch stmt) $SQL_SUCCESS)
                    do (example-check (%%sql-get-data stmt 1 $SQL_C_SLONG rid 0 ind)
                                      $SQL_HANDLE_STMT stmt "id")
                       (example-check (%%sql-get-data stmt 2 $SQL_C_WCHAR rname (* 2 51) ind)
                                      $SQL_HANDLE_STMT stmt "name")
                       (format t "~D: ~A~%" (cffi:mem-ref rid 'sql-integer) (get-wide-string-nts rname))))
            (example-check (%%sql-free-stmt stmt $SQL_CLOSE) $SQL_HANDLE_STMT stmt "close")

            ;; a failing statement: read its diagnostics
            (with-wide-string (text "SELECT * FROM no_such_table")
              (when (= (%%sql-exec-direct-w stmt text $SQL_NTS) $SQL_ERROR)
                (cffi:with-foreign-objects ((state :uint16 6) (native 'sql-integer)
                                            (message :uint16 1024) (len 'sql-small-int))
                  (%%sql-get-diag-rec-w $SQL_HANDLE_STMT stmt 1 state native message 1024 len)
                  (format t "error ~A: ~A~%" (get-wide-string-nts state) (get-wide-string-nts message)))))

            ;; clean up
            (with-wide-string (text "DROP TABLE person")
              (%%sql-exec-direct-w stmt text $SQL_NTS))
            (%%sql-end-tran $SQL_HANDLE_DBC dbc $SQL_COMMIT)
            (%%sql-free-handle $SQL_HANDLE_STMT stmt)))
        (%%sql-disconnect dbc)
        (%%sql-free-handle $SQL_HANDLE_DBC dbc))
      (%%sql-free-handle $SQL_HANDLE_ENV env)))
  (values))
