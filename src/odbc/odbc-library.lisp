;;; -*- Mode: Lisp -*-
;;; Loading the ODBC driver manager.
;;;
;;; Windows: odbc32.dll. Linux/BSD: unixODBC (libodbc.so.2; the unversioned
;;; libodbc.so exists only with the development package). macOS: unixODBC or
;;; iODBC. *ODBC-LIBRARY-FILE* (global.lisp), when set before loading,
;;; names the library explicitly.
;;;
;;; Copyright (c) 2026 QSS / Friedrich Dominicus, BSD license (see license.txt)

(in-package :qss-cl-odbc)

(cffi:define-foreign-library odbc
  (:windows "odbc32.dll")
  (:darwin (:or "libodbc.2.dylib" "libodbc.dylib" "libiodbc.2.dylib" "libiodbc.dylib"))
  (:unix (:or "libodbc.so.2" "libodbc.so" "libodbc.so.1")))

(if *odbc-library-file*
    (cffi:load-foreign-library *odbc-library-file*)
    (cffi:use-foreign-library odbc))
