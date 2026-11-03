;;; -*- Mode: Lisp -*-

;; qss-cl-odbc, derived from plain-odbc (ODBC module for clisp)
;; Copyright (C) Roland Averkamp 2005
;; Roland.Averkamp@gmx.de
;; the license agreement can be found in file license.txt


(in-package :qss-cl-odbc)

(defvar *odbc-library-file* nil
  "File of the ODBC driver manager, when the default should not be used
(Windows odbc32.dll, Linux/BSD libodbc.so.2, macOS unixODBC or iODBC; see
odbc-library.lisp). Set it before loading qss-cl-odbc.")


(defvar *universal-time-to-date-dataype* 'identity)
(defvar *date-datatype-to-universal-time* 'identity)
(defvar *date-type-predicate* (lambda (x) (declare (ignore x)) nil))

(defvar *max-precision* 65536)

(defvar *lob-fetch-buffer-size* 100000)

(defvar *max-column-buffer-size* 8001)

(defvar *default-string-parameter-size* 8001)

(defvar  *DEFAULT-BINARY-PARAMETER-SIZE* 8001)


;; QSS 2026-10-13: NUMERIC/DECIMAL (Access: Waehrung) exakt als rationale Zahl
;; statt als Double lesen. NIL stellt das alte Verhalten wieder her.
(defvar *decimal-as-rational* t)
;; QSS 2026-10-13: Spaltengroesse fuer LOB-Parameter ohne Laengenangabe
;; (Access meldet fuer Memo/LONGCHAR 1073741823)
(defvar *default-lob-column-size* 1073741823)
;; QSS 2026-10-13: Zeitzone fuer die Umrechnung Datum <-> Universal Time.
;; Access (und die meisten Datenbanken) speichern Wanduhrzeit ohne Zeitzone.
;; 0 = ohne Zeitzone und ohne Sommerzeit: jede Wanduhrzeit kommt unveraendert
;; zurueck (vorher: 02:30 am Tag der Umstellung auf Sommerzeit wurde 03:30).
;; Eine Universal Time aus qss-cl-odbc ist dann die Wanduhrzeit als UTC;
;; zerlegen mit (decode-universal-time ut 0).
;; NIL = lokale Zeitzone von Lisp (altes Verhalten).
(defvar *date-time-zone* 0)
;; QSS 2026-10-14: SQL-Typ fuer :decimal-Parameter. 2 = SQL_NUMERIC (Access,
;; SQLite, SQL Server); 3 = SQL_DECIMAL, kennt der Access-Treiber nicht.
(defvar *decimal-parameter-sql-type* 2)
