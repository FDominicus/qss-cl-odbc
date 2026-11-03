;;; -*- Mode: lisp -*-

;; qss-cl-odbc, derived from plain-odbc (ODBC module for clisp)
;; Copyright (C) Roland Averkamp 2005
;; Roland.Averkamp@gmx.de
;; the license agreement can be found in file license.txt


(in-package :qss-cl-odbc)

(defvar *default-access-dsn* "default-access-dsn")
(defvar *default-oracle-dsn* "default-oracle-dsn")
(defvar *default-sql-server-dsn* "default-sql-server-dsn")
(defvar *default-mysql-dsn* "default-mysql-dsn")
(defvar *default-sqlite-dsn* "default-sqlite-dsn")



;; fixme, do we need to quote the attributes with {}?
;; this seems not work with the following conenction strings

(defun connect-access (filename)
  (connect-generic :DSN *default-access-dsn* 
                   :DBQ filename))


(defun connect-oracle (tns-name user pw)
  (connect-generic :dsn *default-oracle-dsn* 
                   :dbq  tns-name
                   :uid user
                   :pwd pw))

(defun connect-sql-server (server database &optional (user-id nil) (password nil))
  (if (and user-id password)
    (connect-generic :dsn *default-sql-server-dsn*
                     :database database
                     :server server
                     :Trusted_connection "NO"
                     :uid user-id
                     :pwd password)
    (connect-generic :dsn *default-sql-server-dsn*
                     :database database
                     :server server
                     :Trusted_connection "YES"
                     )))

(defun connect-mysql (server database user password)
  (apply 'connect-generic 
         (append 
          (list :dsn *default-mysql-dsn*
                :server server :uid user :pwd password)
          (if database (list :database database) ()) 
         )))

;; QSS 2026-10-14: der SQLite-ODBC-Treiber (Werner) nimmt ANSI-Text als UTF-8,
;; unter Linux wie unter Windows (dort am 14.10.2026 nachgeprueft).
(defun connect-sqlite (filename) 
  (CONNECT-GENERIC :dsn *default-sqlite-dsn* :database filename :ansi-encoding :utf-8))



(defun with-prepared-statement-fun (con string params fun)
  (let ((stm (apply #'prepare-statement con string params)))
    (unwind-protect 
      (funcall fun stm)
      (free-statement stm))))

(defmacro with-prepared-statement  ((stm con string &rest params) &body body)
  `(with-prepared-statement-fun ,con ,string (list ,@params) (lambda (,stm) ,@body)))

