;;;-*- Mode: Lisp; Package: COMMON-LISP-USER -*-

;; qss-cl-odbc, derived from plain-odbc (ODBC module for clisp)
;; Copyright (C) Roland Averkamp 2005
;; Roland.Averkamp@gmx.de
;; the license agreement can be found in file license.txt

(in-package :common-lisp-user) 

(defpackage "QSS-CL-ODBC"
  (:use 
   "COMMON-LISP"  "CFFI")
;  rav, 20.12.2007 I do not think this is necessary anymore
; #+mcl "CCL" #+cormanlisp "WIN32" "CFFI")
  (:export
   "*ODBC-LIBRARY-FILE*" ; QSS: hier statt nachtraeglich in global.lisp (Neuladen)
   "EXEC-SQL"
   "EXEC-QUERY" 
   "EXEC-UPDATE" 
   "EXEC-COMMAND"

   "EXEC-SQL*"
   "EXEC-QUERY*" 
   "EXEC-UPDATE*" 
   "EXEC-COMMAND*"
   "MAP-QUERY" ; QSS
   "*CONNECT-AUTOCOMMIT*" ; QSS
   "MAP-QUERY*" ; QSS
   "DO-QUERY" ; QSS

   "PREPARE-STATEMENT" 
   "EXEC-PREPARED-QUERY" 
   "EXEC-PREPARED-UPDATE"
   "EXEC-PREPARED-COMMAND"
   "FREE-STATEMENT" 
   "CONNECT"   

   ;Metadata
   "GET-PRIMARY-KEYS"
   "GET-FOREIGN-KEYS"
   "GET-TABLES"
   "GET-COLUMNS"
   ;"DRIVER-CONNECT"
   "CONNECT-GENERIC"
   "CLOSE-CONNECTION"
   "COMMIT"
   "ROLLBACK"
   
   "WITH-PREPARED-STATEMENT"
   
   "*UNIVERSAL-TIME-TO-DATE-DATAYPE*"
   "*DATE-DATATYPE-TO-UNIVERSAL-TIME*"
   "*DATE-TYPE-PREDICATE*"
   "*DATE-TIME-ZONE*" ; QSS
   "ENCODE-DATE" ; QSS
   "DECODE-DATE" ; QSS
   "*DECIMAL-AS-RATIONAL*" ; QSS
   "*ANSI-ENCODING*" ; QSS
   "CONNECTION-ANSI-ENCODING" ; QSS 2026-10-14
   ;; QSS 2026-10-14: conditions and their readers
   "SQL-CONDITION" "SQL-ERROR" "SQL-WARNING"
   "SQL-CONDITION-MESSAGE" "SQL-CONDITION-STATE" "SQL-CONDITION-NATIVE-ERROR"
   "SQL-CONDITION-DIAGNOSTICS"

   ;; utilities
   "*DEFAULT-ACCESS-DSN*"
   "*DEFAULT-ORACLE-DSN*"
   "*DEFAULT-SQL-SERVER-DSN*"
   "*DEFAULT-MYSQL-DSN*"
   "*DEFAULT-SQLITE-DSN*"

   
   "USE-BIND-COLUMN"
   "INTEGER-AS-BIGINT" ; QSS 2026-11-03

   "CONNECT-ACCESS"
   "CONNECT-SQL-SERVER"
   "CONNECT-ORACLE" 
   "CONNECT-MYSQL"
   "CONNECT-SQLITE" 

   "TRACE-CONNECTION"
   "UNTRACE-CONNECTION"


)) 
 
