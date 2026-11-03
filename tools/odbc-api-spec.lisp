;;; -*- Mode: Lisp -*-
;;; What qss-cl-odbc takes from the ODBC headers (odbc-headers/*.h).
;;; Read by tools/generate-odbc-api.lisp; the result is
;;; src/odbc/odbc-api-generated.lisp. After a change here, regenerate:
;;;   (asdf:load-system :qss-cl-odbc-generate)
;;;   (qss-cl-odbc-generate:generate)
;;;
;;; Copyright (c) 2026 QSS / Friedrich Dominicus, BSD license (see license.txt)

(;; Headers in include order (each includes the previous one).
 :headers ("sqltypes.h" "sql.h" "sqlext.h" "sqlucode.h")

 ;; Preprocessor symbols. The headers are Windows headers; the ODBC ABI is the
 ;; same with unixODBC and iODBC except for the pointer-sized types, which the
 ;; generator finds by reading the headers once with and once without _WIN64.
 ;; ODBCVER: the headers' own default (ODBC 3.80).
 ;; ODBCINT64: sqltypes.h asks other compilers than MSVC to define it.
 :defines (("WIN32" . "1") ("ODBCINT64" . "__int64"))
 :pointer-width-define "_WIN64"

 ;; Header type -> Lisp foreign type name (DEFCTYPE in the generated file).
 ;; Pointers and handles become :POINTER and need no entry.
 :types (("SQLSMALLINT"  sql-small-int)
         ("SQLUSMALLINT" sql-u-small-int)
         ("SQLINTEGER"   sql-integer)
         ("SQLUINTEGER"  sql-u-integer)
         ("SQLLEN"       sql-len)
         ("SQLULEN"      sql-u-len)
         ("SQLBIGINT"    sql-bigint)
         ("SQLUBIGINT"   sql-u-bigint)
         ("SQLDOUBLE"    sql-double)
         ("SQLREAL"      sql-real)
         ("SQLCHAR"      sql-char)
         ("SQLWCHAR"     sql-wchar)
         ("SQLRETURN"    sql-return))

 ;; Header structure -> Lisp structure name (DEFCSTRUCT).
 :structs (("DATE_STRUCT"      sql-c-date)
           ("TIME_STRUCT"      sql-c-time)
           ("TIMESTAMP_STRUCT" sql-c-timestamp))

 ;; Functions qss-cl-odbc calls. Text arguments only through the Unicode
 ;; (...W) variants. Lisp name: %% + the C name in Lisp style
 ;; (SQLGetDiagRecW -> %%SQL-GET-DIAG-REC-W).
 :functions ("SQLAllocHandle" "SQLFreeHandle"
             "SQLSetEnvAttr" "SQLSetConnectAttrW"
             "SQLDriverConnectW" "SQLDisconnect"
             "SQLGetInfoW" "SQLGetDiagRecW" "SQLEndTran"
             "SQLExecDirectW" "SQLPrepareW" "SQLExecute"
             "SQLNumResultCols" "SQLRowCount" "SQLDescribeColW"
             "SQLBindCol" "SQLFetch" "SQLGetData" "SQLMoreResults" "SQLFreeStmt"
             "SQLBindParameter" "SQLParamData" "SQLPutData"
             "SQLTablesW" "SQLColumnsW" "SQLPrimaryKeysW" "SQLForeignKeysW")

 ;; Constants: every object-like macro whose name starts with this prefix and
 ;; whose value is an integer or a string constant.
 :constant-prefix "SQL_"

 ;; Constants left out: groups of (names reason); the reasons are written
 ;; into the generated file.
 :exclude-constants
 ((("SQL_SPEC_MAJOR" "SQL_SPEC_MINOR" "SQL_SPEC_STRING")
   "sqlext.h of microsoft/ODBC-Specification sets them to 4.00 (draft ODBC 4.0) regardless of ODBCVER; qss-cl-odbc uses ODBC 3.80 (mingw-w64: 3.80, unixODBC 2.3.12: 3.52)")
  (("SQL_OPT_TRACE_FILE_DEFAULT")
   "default trace file of the driver manager, platform-specific (Windows \\SQL.LOG, unixODBC /tmp/SQL.LOG)")))
