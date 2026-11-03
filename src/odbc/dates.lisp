;;; -*- Mode: Lisp -*-
;;; Dates: universal time <-> SQL_C_TYPE_TIMESTAMP, also before 1900.
;;;
;;; ENCODE-/DECODE-UNIVERSAL-TIME reject years before 1900 in SBCL, but Access
;;; stores a time of day on 1899-12-30. The day part is therefore computed
;;; here (proleptic Gregorian calendar); the time of day goes through the
;;; standard functions in the zone *DATE-TIME-ZONE* (global.lisp, default 0:
;;; the wall clock time of the database as UTC, see ENCODE-DATE).
;;;
;;; Copyright (c) 2026 QSS / Friedrich Dominicus, BSD license (see license.txt)

(in-package :qss-cl-odbc)

;; DAYS-FROM-CIVIL and CIVIL-FROM-DAYS follow Howard Hinnant's algorithms
;; "days_from_civil" / "civil_from_days" (public domain,
;; https://howardhinnant.github.io/date_algorithms.html).
(defun days-from-civil (year month day)
  "Days since 1970-01-01 (proleptic Gregorian calendar)."
  (let* ((y (if (<= month 2) (1- year) year))
         (era (floor y 400))
         (year-of-era (- y (* era 400)))
         (shifted-month (mod (+ month 9) 12))            ; March = 0
         (day-of-year (+ (floor (+ (* 153 shifted-month) 2) 5) (1- day)))
         (day-of-era (+ (* year-of-era 365) (floor year-of-era 4)
                        (- (floor year-of-era 100)) day-of-year)))
    (+ (* era 146097) day-of-era -719468)))

(defun civil-from-days (days)
  "Inverse of DAYS-FROM-CIVIL: year, month, day."
  (let* ((z (+ days 719468))
         (era (floor z 146097))
         (day-of-era (- z (* era 146097)))
         (year-of-era (floor (- day-of-era (floor day-of-era 1460)
                                (- (floor day-of-era 36524)) (floor day-of-era 146096))
                             365))
         (day-of-year (- day-of-era (- (+ (* 365 year-of-era) (floor year-of-era 4))
                                       (floor year-of-era 100))))
         (shifted-month (floor (+ (* 5 day-of-year) 2) 153))
         (day (1+ (- day-of-year (floor (+ (* 153 shifted-month) 2) 5))))
         (month (if (< shifted-month 10) (+ shifted-month 3) (- shifted-month 9)))
         (year (+ year-of-era (* era 400))))
    (values (if (<= month 2) (1+ year) year) month day)))

(defconstant +days-1901-01-01+ -25202)  ; (days-from-civil 1901 1 1)

;; Reference 1901-01-01 instead of 1900-01-01: east of UTC, 1900-01-01 00:xx
;; local time lies before universal time 0, which SBCL rejects as well.
(defun encode-universal-time* (second minute hour day month year)
  "Like ENCODE-UNIVERSAL-TIME, also before 1900; zone *DATE-TIME-ZONE*."
  (flet ((encode (s mi h d mo y)
           (if *date-time-zone*
               (encode-universal-time s mi h d mo y *date-time-zone*)
               (encode-universal-time s mi h d mo y))))
    (if (>= year 1901)
        (encode second minute hour day month year)
        (+ (encode second minute hour 1 1 1901)
           (* 86400 (- (days-from-civil year month day) +days-1901-01-01+))))))

(defun decode-universal-time* (universal-time)
  "Like DECODE-UNIVERSAL-TIME (first six values), also for values before
1900 (negative); zone *DATE-TIME-ZONE*."
  (flet ((decode (u)
           (if *date-time-zone*
               (decode-universal-time u *date-time-zone*)
               (decode-universal-time u))))
    (let ((safe (* 86400 400)))
      (if (>= universal-time safe)
          (multiple-value-bind (s mi h d mo y) (decode universal-time)
            (values s mi h d mo y))
          ;; shift by whole days into the safe range, decode, take the days
          ;; back off in the calendar
          (let ((shift (ceiling (- safe universal-time) 86400)))
            (multiple-value-bind (s mi h d mo y) (decode (+ universal-time (* shift 86400)))
              (multiple-value-bind (y2 mo2 d2) (civil-from-days (- (days-from-civil y mo d) shift))
                (values s mi h d2 mo2 y2))))))))

(defun encode-date (year month day &optional (hour 0) (minute 0) (second 0))
  "Universal time for a wall clock time of the database (see *DATE-TIME-ZONE*).
Use this, not ENCODE-UNIVERSAL-TIME, for date values given to qss-cl-odbc."
  (encode-universal-time* second minute hour day month year))

(defun decode-date (universal-time)
  "Wall clock time of a universal time from qss-cl-odbc: year, month, day,
hour, minute, second."
  (multiple-value-bind (s mi h d mo y) (decode-universal-time* universal-time)
    (values y mo d h mi s)))

;;; SQL_C_TYPE_TIMESTAMP (structure SQL-C-TIMESTAMP from the headers)

(defun %put-sql-c-timestamp (pointer y mo d h mi s fr)
  (cffi:with-foreign-slots ((year month day hour minute second fraction)
                            pointer (:struct sql-c-timestamp))
    (setf year y month mo day d hour h minute mi second s fraction fr))
  pointer)

(defun timestamp-to-universal-time (pointer)
  "Universal time and fraction (nanoseconds) of a SQL_C_TYPE_TIMESTAMP."
  (cffi:with-foreign-slots ((year month day hour minute second fraction)
                            pointer (:struct sql-c-timestamp))
    (values (encode-universal-time* second minute hour day month year) fraction)))
