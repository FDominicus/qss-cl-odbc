;;; -*- Mode: Lisp -*-
;;; The calls into the ODBC driver manager.
;;;
;;; Built on the declarations generated from Microsoft's ODBC headers
;;; (odbc-api-generated.lisp). Every call checks its return code: errors
;;; signal SQL-ERROR, SQL_SUCCESS_WITH_INFO signals SQL-WARNING (WARN), both
;;; with all diagnostic records of the handle (SQLGetDiagRecW). Text goes to
;;; and comes from the driver as UTF-16 (the ...W functions).
;;;
;;; The upper layer (odbc-main, column, parameter) uses the %-functions here;
;;; their names and arguments are those plain-odbc has used since 2005.
;;;
;;; Copyright (c) 2026 QSS / Friedrich Dominicus, BSD license (see license.txt)

(in-package :qss-cl-odbc)

;;; ------------------------------------------------------------------
;;; Conditions

(define-condition sql-condition (condition)
  ((error-message :initarg :error-message :initform nil :reader sql-condition-message)
   (error-code :initarg :error-code :initform nil :reader sql-condition-native-error)
   (sql-state :initarg :sql-state :initform nil :reader sql-condition-state)
   (diagnostics :initarg :diagnostics :initform '() :reader sql-condition-diagnostics
                :documentation "All diagnostic records: list of (sqlstate native-error message)."))
  (:report (lambda (condition stream)
             (format stream "~A, error code ~A, State: ~A."
                     (sql-condition-message condition)
                     (sql-condition-native-error condition)
                     (sql-condition-state condition)))))

(define-condition sql-error (sql-condition error) ())

(define-condition sql-warning (sql-condition warning) ())

;;; ------------------------------------------------------------------
;;; Temporary foreign memory

(defmacro with-temporary-allocations (bindings &body body)
  "Like LET for foreign memory: every value is freed with FOREIGN-FREE on exit."
  `(let ,bindings
     (unwind-protect (progn ,@body)
       ,@(loop for (var) in (reverse bindings)
               collect `(unless (cffi:null-pointer-p ,var) (cffi:foreign-free ,var))))))

;;; ------------------------------------------------------------------
;;; Diagnostics (SQLGetDiagRecW)

(defun usable-handle-p (handle)
  (and handle (not (cffi:null-pointer-p handle))))

(defun diagnostic-handle (henv hdbc hstmt)
  "Most specific handle given: (values handle-type handle), or NIL."
  (cond ((usable-handle-p hstmt) (values $SQL_HANDLE_STMT hstmt))
        ((usable-handle-p hdbc) (values $SQL_HANDLE_DBC hdbc))
        ((usable-handle-p henv) (values $SQL_HANDLE_ENV henv))
        (t nil)))

;; Buffers for text the driver returns start small and grow. A full buffer
;; counts as possibly truncated: psqlODBC/unixODBC report a truncated message
;; with SQL_SUCCESS and the truncated length (ODBC wants SQL_SUCCESS_WITH_INFO
;; and the full length; seen 2026-10-14).
(defconstant +max-text-buffer+ 32000)   ; buffer lengths are SQLSMALLINT

(defun next-capacity (capacity returned-length)
  "Larger buffer when RETURNED-LENGTH (characters, without the 0) may have
been cut by CAPACITY, else NIL."
  (when (and (>= returned-length capacity) (< capacity +max-text-buffer+))
    (min +max-text-buffer+ (max (* 2 capacity) (1+ returned-length)))))

;; Start large: psqlODBC splits a long message into several records at the
;; size of the first request, counted in UTF-8 bytes; when the cut falls into
;; a multi-byte character (a German umlaut), SQLGetDiagRecW returns only
;; "Error: SqlState=..." instead of the text (psqlODBC 16, 2026-10-14).
(defparameter *diagnostic-buffer-size* 8190
  "First buffer size (characters) for diagnostic messages.")

(defun diagnostic-record (handle-type handle record)
  "Record number RECORD of HANDLE as (sqlstate native-error message), or NIL
when there is none. A message that may be cut is read again, larger."
  (cffi:with-foreign-objects ((state :uint16 6)
                              (native 'sql-integer)
                              (text-length 'sql-small-int))
    (let ((capacity *diagnostic-buffer-size*))
      (loop
        (let ((message (alloc-wide-buffer capacity)))
          (unwind-protect
               (let ((rc (%%sql-get-diag-rec-w handle-type handle record state native
                                               message (1+ capacity) text-length)))
                 (if (not (or (= rc $SQL_SUCCESS) (= rc $SQL_SUCCESS_WITH_INFO)))
                     (return nil)
                     (let ((larger (next-capacity capacity (cffi:mem-ref text-length 'sql-small-int))))
                       (if larger
                           (setf capacity larger)
                           (return (list (get-wide-string-nts state)
                                         (cffi:mem-ref native 'sql-integer)
                                         (get-wide-string-nts message)))))))
            (cffi:foreign-free message)))))))

(defun diagnostic-records (handle-type handle)
  "All diagnostic records of HANDLE, in order."
  (loop for record from 1 to 100
        for diagnostic = (diagnostic-record handle-type handle record)
        while diagnostic
        collect diagnostic))

(defun sql-state (henv hdbc hstmt)
  "SQLSTATE of the first diagnostic record of the most specific handle, or NIL."
  (multiple-value-bind (handle-type handle) (diagnostic-handle henv hdbc hstmt)
    (and handle-type (first (diagnostic-record handle-type handle 1)))))

;;; ------------------------------------------------------------------
;;; Return codes

(defun make-sql-condition (class records default-message)
  (if records
      (make-condition class
                      :error-message (format nil "~{~A~^ | ~}" (mapcar #'third records))
                      :sql-state (first (first records))
                      :error-code (second (first records))
                      :diagnostics records)
      (make-condition class :error-message default-message)))

(defun error-handling-fun (result-code henv hdbc hstmt)
  "Classifies RESULT-CODE of a call on the given handles. Returns the code and
a condition to signal (SQL-ERROR or SQL-WARNING), or NIL."
  (flet ((records ()
           (multiple-value-bind (handle-type handle) (diagnostic-handle henv hdbc hstmt)
             (and handle-type (diagnostic-records handle-type handle)))))
    (cond
      ((or (= result-code $SQL_SUCCESS) (= result-code $SQL_NO_DATA)
           (= result-code $SQL_NEED_DATA))
       (values result-code nil))
      ((= result-code $SQL_SUCCESS_WITH_INFO)
       (let ((records (records)))
         (values result-code (and records (make-sql-condition 'sql-warning records nil)))))
      ((= result-code $SQL_ERROR)
       (values result-code
               (make-sql-condition 'sql-error (records)
                                   "[qss-cl-odbc] ODBC call failed, the driver gave no diagnostic record")))
      ((= result-code $SQL_INVALID_HANDLE)
       (values result-code (make-condition 'sql-error :error-message "[ODBC error] Invalid handle")))
      ((= result-code $SQL_STILL_EXECUTING)
       (values result-code (make-condition 'sql-error :error-message "[ODBC error] Still executing")))
      (t
       (values result-code
               (make-condition 'sql-error
                               :error-message (format nil "[qss-cl-odbc] unknown ODBC return code ~D"
                                                      result-code)))))))

(defun check-result (result-code &key henv hdbc hstmt)
  "Signals the condition for RESULT-CODE (error or warning); returns RESULT-CODE."
  (multiple-value-bind (code condition) (error-handling-fun result-code henv hdbc hstmt)
    (typecase condition
      (error (error condition))
      (warning (warn condition)))
    code))

(defmacro with-error-handling ((&key henv hdbc hstmt) odbc-call &body body)
  "Evaluates ODBC-CALL, signals its error or warning, then BODY; without BODY
the return code is the value."
  `(progn
     (check-result ,odbc-call :henv ,henv :hdbc ,hdbc :hstmt ,hstmt)
     ,@body))

;;; ------------------------------------------------------------------
;;; Handles

(defun allocate-handle (handle-type input-handle &key henv hdbc)
  (cffi:with-foreign-object (output :pointer)
    (setf (cffi:mem-ref output :pointer) (cffi:null-pointer))
    (check-result (%%sql-alloc-handle handle-type (or input-handle (cffi:null-pointer)) output)
                  :henv henv :hdbc hdbc)
    (cffi:mem-ref output :pointer)))

(defun %new-environment-handle ()
  "Environment handle, registered as ODBC 3 application."
  (let ((henv (allocate-handle $SQL_HANDLE_ENV nil)))
    (check-result (%%sql-set-env-attr henv $SQL_ATTR_ODBC_VERSION
                                      (cffi:make-pointer $SQL_OV_ODBC3) 0)
                  :henv henv)
    henv))

(defun %new-db-connection-handle (henv)
  (allocate-handle $SQL_HANDLE_DBC henv :henv henv))

(defun %new-statement-handle (hdbc)
  (allocate-handle $SQL_HANDLE_STMT hdbc :hdbc hdbc))

(defun %free-statement (hstmt option)
  "OPTION :drop frees the handle; :close, :unbind, :reset as SQLFreeStmt."
  (if (eq option :drop)
      (check-result (%%sql-free-handle $SQL_HANDLE_STMT hstmt) :hstmt hstmt)
      (check-result (%%sql-free-stmt hstmt (ecase option
                                             (:close $SQL_CLOSE)
                                             (:unbind $SQL_UNBIND)
                                             (:reset $SQL_RESET_PARAMS)))
                    :hstmt hstmt)))

(defun %free-connection (hdbc)
  (check-result (%%sql-free-handle $SQL_HANDLE_DBC hdbc) :hdbc hdbc))

;;; ------------------------------------------------------------------
;;; Connections

(defun %sql-driver-connect (henv hdbc connection-string completion)
  "Connects HDBC; returns the completed connection string of the driver."
  (declare (string connection-string))
  (let ((completion (ecase completion
                      (:complete $SQL_DRIVER_COMPLETE)
                      (:required $SQL_DRIVER_COMPLETE_REQUIRED)
                      (:prompt $SQL_DRIVER_PROMPT)
                      (:noprompt $SQL_DRIVER_NOPROMPT)))
        (capacity 1024))
    (with-wide-string (in connection-string)
      (with-temporary-allocations ((out (alloc-wide-buffer capacity))
                                   (out-length (cffi:foreign-alloc 'sql-small-int :initial-element 0)))
        (check-result (%%sql-driver-connect-w hdbc (cffi:null-pointer) in $SQL_NTS
                                              out (1+ capacity) out-length completion)
                      :henv henv :hdbc hdbc)
        (get-wide-string-nts out)))))

(defun %disconnect (hdbc)
  (check-result (%%sql-disconnect hdbc) :hdbc hdbc))

(defun %commit (henv hdbc)
  (declare (ignore henv))
  (check-result (%%sql-end-tran $SQL_HANDLE_DBC hdbc $SQL_COMMIT) :hdbc hdbc))

(defun %rollback (henv hdbc)
  (declare (ignore henv))
  (check-result (%%sql-end-tran $SQL_HANDLE_DBC hdbc $SQL_ROLLBACK) :hdbc hdbc))

(defun set-connection-attribute (hdbc attribute value)
  "SQLSetConnectAttrW with an integer or a string VALUE."
  (etypecase value
    (integer
     (check-result (%%sql-set-connect-attr-w hdbc attribute (cffi:make-pointer value) $SQL_IS_UINTEGER)
                   :hdbc hdbc))
    (string
     (with-wide-string (ptr value)
       (check-result (%%sql-set-connect-attr-w hdbc attribute ptr $SQL_NTS) :hdbc hdbc)))))

(defun disable-autocommit (hdbc)
  (set-connection-attribute hdbc $SQL_ATTR_AUTOCOMMIT $SQL_AUTOCOMMIT_OFF))

(defun enable-autocommit (hdbc)
  (set-connection-attribute hdbc $SQL_ATTR_AUTOCOMMIT $SQL_AUTOCOMMIT_ON))

(defun %start-connection-trace (hdbc filename)
  (set-connection-attribute hdbc $SQL_ATTR_TRACEFILE filename)
  (set-connection-attribute hdbc $SQL_ATTR_TRACE $SQL_OPT_TRACE_ON))

(defun %stop-connection-trace (hdbc)
  (set-connection-attribute hdbc $SQL_ATTR_TRACE $SQL_OPT_TRACE_OFF))

;;; SQLGetInfoW: the value type depends on the information type (ODBC
;;; reference, SQLGetInfo). Character strings and 16-bit values are listed;
;;; the handles of the driver are pointer-sized; everything else is a
;;; 32-bit SQLUINTEGER (numbers and bitmasks).

(defparameter +info-strings+
  (list $SQL_ACCESSIBLE_PROCEDURES $SQL_ACCESSIBLE_TABLES $SQL_CATALOG_NAME
        $SQL_CATALOG_NAME_SEPARATOR $SQL_CATALOG_TERM $SQL_COLLATION_SEQ $SQL_COLUMN_ALIAS
        $SQL_DATA_SOURCE_NAME $SQL_DATA_SOURCE_READ_ONLY $SQL_DATABASE_NAME $SQL_DBMS_NAME
        $SQL_DBMS_VER $SQL_DESCRIBE_PARAMETER $SQL_DM_VER $SQL_DRIVER_NAME
        $SQL_DRIVER_ODBC_VER $SQL_DRIVER_VER $SQL_EXPRESSIONS_IN_ORDERBY
        $SQL_IDENTIFIER_QUOTE_CHAR $SQL_INTEGRITY $SQL_KEYWORDS $SQL_LIKE_ESCAPE_CLAUSE
        $SQL_MAX_ROW_SIZE_INCLUDES_LONG $SQL_MULT_RESULT_SETS $SQL_MULTIPLE_ACTIVE_TXN
        $SQL_NEED_LONG_DATA_LEN $SQL_ODBC_VER $SQL_ORDER_BY_COLUMNS_IN_SELECT
        $SQL_PROCEDURE_TERM $SQL_PROCEDURES $SQL_ROW_UPDATES $SQL_SCHEMA_TERM
        $SQL_SEARCH_PATTERN_ESCAPE $SQL_SERVER_NAME $SQL_SPECIAL_CHARACTERS $SQL_TABLE_TERM
        $SQL_USER_NAME $SQL_XOPEN_CLI_YEAR))

(defparameter +info-16-bit+
  (list $SQL_ACTIVE_ENVIRONMENTS $SQL_CATALOG_LOCATION $SQL_CONCAT_NULL_BEHAVIOR
        $SQL_CORRELATION_NAME $SQL_CURSOR_COMMIT_BEHAVIOR $SQL_CURSOR_ROLLBACK_BEHAVIOR
        $SQL_FILE_USAGE $SQL_GROUP_BY $SQL_IDENTIFIER_CASE $SQL_MAX_CATALOG_NAME_LEN
        $SQL_MAX_COLUMN_NAME_LEN $SQL_MAX_COLUMNS_IN_GROUP_BY $SQL_MAX_COLUMNS_IN_INDEX
        $SQL_MAX_COLUMNS_IN_ORDER_BY $SQL_MAX_COLUMNS_IN_SELECT $SQL_MAX_COLUMNS_IN_TABLE
        $SQL_MAX_CONCURRENT_ACTIVITIES $SQL_MAX_CURSOR_NAME_LEN $SQL_MAX_DRIVER_CONNECTIONS
        $SQL_MAX_IDENTIFIER_LEN $SQL_MAX_PROCEDURE_NAME_LEN $SQL_MAX_SCHEMA_NAME_LEN
        $SQL_MAX_TABLE_NAME_LEN $SQL_MAX_TABLES_IN_SELECT $SQL_MAX_USER_NAME_LEN
        $SQL_NON_NULLABLE_COLUMNS $SQL_NULL_COLLATION $SQL_QUOTED_IDENTIFIER_CASE
        $SQL_TXN_CAPABLE $SQL_ODBC_API_CONFORMANCE $SQL_ODBC_SQL_CONFORMANCE
        $SQL_ODBC_SAG_CLI_CONFORMANCE))

(defparameter +info-pointer-sized+
  (list $SQL_DRIVER_HDBC $SQL_DRIVER_HENV $SQL_DRIVER_HDESC $SQL_DRIVER_HLIB $SQL_DRIVER_HSTMT))

(defun %sql-get-info (hdbc info-type)
  "Value of INFO-TYPE: a string or an integer."
  (if (member info-type +info-strings+)
      (let ((capacity 2048))
        (with-temporary-allocations ((value (alloc-wide-buffer capacity))
                                     (value-length (cffi:foreign-alloc 'sql-small-int :initial-element 0)))
          ;; buffer length in bytes for SQLGetInfoW
          (check-result (%%sql-get-info-w hdbc info-type value (* 2 (1+ capacity)) value-length)
                        :hdbc hdbc)
          (get-wide-string-nts value)))
      (let ((type (cond ((member info-type +info-16-bit+) 'sql-u-small-int)
                        ((member info-type +info-pointer-sized+) 'sql-u-len)
                        (t 'sql-u-integer))))
        ;; zero-filled 8 bytes: a driver that writes fewer bytes than the type
        ;; leaves the rest 0
        (cffi:with-foreign-objects ((value :uint64) (value-length 'sql-small-int))
          (setf (cffi:mem-ref value :uint64) 0)
          (check-result (%%sql-get-info-w hdbc info-type value (cffi:foreign-type-size type) value-length)
                        :hdbc hdbc)
          (cffi:mem-ref value type)))))

;;; ------------------------------------------------------------------
;;; Statements

(defun %sql-exec-direct (sql hstmt henv hdbc)
  "Executes SQL; returns the return code (SQL_NEED_DATA for data-at-execution
parameters, SQL_NO_DATA when nothing was affected)."
  (declare (string sql))
  (with-wide-string (text sql)
    (check-result (%%sql-exec-direct-w hstmt text $SQL_NTS) :henv henv :hdbc hdbc :hstmt hstmt)))

(defun %sql-prepare (hstmt sql)
  (declare (string sql))
  (with-wide-string (text sql)
    (check-result (%%sql-prepare-w hstmt text $SQL_NTS) :hstmt hstmt)))

(defun %sql-execute (hstmt)
  (check-result (%%sql-execute hstmt) :hstmt hstmt))

(defun result-columns-count (hstmt)
  (cffi:with-foreign-object (count 'sql-small-int)
    (check-result (%%sql-num-result-cols hstmt count) :hstmt hstmt)
    (cffi:mem-ref count 'sql-small-int)))

(defun result-rows-count (hstmt)
  (cffi:with-foreign-object (count 'sql-len)
    (check-result (%%sql-row-count hstmt count) :hstmt hstmt)
    (cffi:mem-ref count 'sql-len)))

(defun %describe-column (hstmt column-number)
  "COLUMN-NUMBER counts from 1. Values: name, SQL type, size, decimal digits,
nullable."
  (declare (type (integer 1) column-number))
  (cffi:with-foreign-objects ((name-length 'sql-small-int)
                              (sql-type 'sql-small-int)
                              (size 'sql-u-len)
                              (digits 'sql-small-int)
                              (nullable 'sql-small-int))
    (let ((capacity 256))
      (loop
        (let ((name (alloc-wide-buffer capacity)))
          (unwind-protect
               (let ((rc (%%sql-describe-col-w hstmt column-number name (1+ capacity) name-length
                                               sql-type size digits nullable)))
                 (if (and (or (= rc $SQL_SUCCESS) (= rc $SQL_SUCCESS_WITH_INFO))
                          (next-capacity capacity (cffi:mem-ref name-length 'sql-small-int)))
                     ;; name may be cut: read again with a larger buffer
                     (setf capacity (next-capacity capacity (cffi:mem-ref name-length 'sql-small-int)))
                     (progn
                       (check-result rc :hstmt hstmt)
                       (return (values (get-wide-string-nts name)
                                       (cffi:mem-ref sql-type 'sql-small-int)
                                       (cffi:mem-ref size 'sql-u-len)
                                       (cffi:mem-ref digits 'sql-small-int)
                                       (cffi:mem-ref nullable 'sql-small-int))))))
            (cffi:foreign-free name)))))))

(defun %bind-column (hstmt column-index c-type buffer buffer-length indicator)
  "COLUMN-INDEX counts from 0."
  (declare (type (integer 0) column-index))
  (check-result (%%sql-bind-col hstmt (1+ column-index) c-type buffer buffer-length indicator)
                :hstmt hstmt))

(defun %sql-bind-parameter (hstmt parameter-index direction c-type sql-type column-size
                            decimal-digits buffer buffer-length indicator)
  "PARAMETER-INDEX counts from 0."
  (declare (type (integer 0) parameter-index))
  (check-result (%%sql-bind-parameter hstmt (1+ parameter-index) direction c-type sql-type
                                      column-size decimal-digits buffer buffer-length indicator)
                :hstmt hstmt))

(defun %sql-fetch (hstmt)
  "Return code; SQL_NO_DATA after the last row."
  (check-result (%%sql-fetch hstmt) :hstmt hstmt))

(defun %sql-get-data (hstmt column-index c-type buffer buffer-length indicator)
  "COLUMN-INDEX counts from 0. Truncation (01004) is signalled as a warning."
  (declare (type (integer 0) column-index))
  (check-result (%%sql-get-data hstmt (1+ column-index) c-type buffer buffer-length indicator)
                :hstmt hstmt))

(defun %sql-get-data-raw (hstmt column-index c-type buffer buffer-length indicator)
  "Like %SQL-GET-DATA without checking: returns the return code, for callers
that read in parts and handle 01004 themselves."
  (declare (type (integer 0) column-index))
  (%%sql-get-data hstmt (1+ column-index) c-type buffer buffer-length indicator))

(defun %sql-param-data (hstmt value-pointer)
  (check-result (%%sql-param-data hstmt value-pointer) :hstmt hstmt))

(defun %sql-put-data (hstmt buffer length)
  (check-result (%%sql-put-data hstmt buffer length) :hstmt hstmt))

(defun %sql-len-data-at-exec (length)
  "SQL_LEN_DATA_AT_EXEC(length) of sqlext.h."
  (+ (- length) $SQL_LEN_DATA_AT_EXEC_OFFSET))

(defun %sql-more-results (hstmt)
  "T when a further result is ready, NIL when there is none. Errors of the
next result are signalled (before 2026-10-14 they were lost)."
  (/= $SQL_NO_DATA (check-result (%%sql-more-results hstmt) :hstmt hstmt)))

;;; ------------------------------------------------------------------
;;; Catalog functions (NIL for an argument = not given)

(defun %sql-tables (hstmt catalog schema table table-type)
  (with-wide-strings ((c catalog) (s schema) (tb table) (ty table-type))
    (check-result (%%sql-tables-w hstmt c $SQL_NTS s $SQL_NTS tb $SQL_NTS ty $SQL_NTS)
                  :hstmt hstmt)))

(defun %sql-columns (hstmt catalog schema table column)
  (with-wide-strings ((c catalog) (s schema) (tb table) (co column))
    (check-result (%%sql-columns-w hstmt c $SQL_NTS s $SQL_NTS tb $SQL_NTS co $SQL_NTS)
                  :hstmt hstmt)))

(defun %sql-primary-keys (hstmt catalog schema table)
  (with-wide-strings ((c catalog) (s schema) (tb table))
    (check-result (%%sql-primary-keys-w hstmt c $SQL_NTS s $SQL_NTS tb $SQL_NTS)
                  :hstmt hstmt)))

(defun %sql-foreign-keys (hstmt pk-catalog pk-schema pk-table fk-catalog fk-schema fk-table)
  (with-wide-strings ((c1 pk-catalog) (s1 pk-schema) (t1 pk-table)
                      (c2 fk-catalog) (s2 fk-schema) (t2 fk-table))
    (check-result (%%sql-foreign-keys-w hstmt c1 $SQL_NTS s1 $SQL_NTS t1 $SQL_NTS
                                        c2 $SQL_NTS s2 $SQL_NTS t2 $SQL_NTS)
                  :hstmt hstmt)))
