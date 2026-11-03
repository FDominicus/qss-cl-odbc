;;; -*- Mode: lisp -*-

;; qss-cl-odbc, derived from plain-odbc (ODBC module for clisp)
;; Copyright (C) Roland Averkamp 2005
;; Roland.Averkamp@gmx.de
;; the license agreement can be found in file license.txt

(in-package :qss-cl-odbc)

;;; abstract base class
(defclass column ()
  (
   ;(lisp-type :initarg :lisp-type)
   (c-type)
   (position :initarg :position)
   (hstmt :accessor hstmt :initarg :hstmt)
   (sql-type :initarg :sql-type)
   (column-name :initarg :column-name)
   (column-size :initform 0 :initarg :column-size)
   (decimal-digits :initform 0 :initarg :decimal-digits)
   (value-ptr :initform nil)
   (buffer-length :initform 0)
   (ind-ptr :initform nil :initarg :ind-ptr)
   (nullable :initarg :nullable)
   (bound)))

;;; retrieve data via get-data
(defclass getdata-column (column)
  ((bound :initform nil)))

;;; retrieve data via a bound buffer
(defclass bound-column (column)
  ((bound :initform t)))

(defun column-is-bound (c)
  (slot-value c 'bound))



(defgeneric initialize-column (param arglist))

(defgeneric get-column-value (parameter))

;; QSS 2026-10-13: ODBC 3 -- Zeichenspalten immer als SQL_C_WCHAR lesen (der
;; Treiber wandelt), damit nichts an der ANSI-Codepage haengt; Datum/Zeit mit
;; den Typcodes von ODBC 3 (91/92/93) und den alten (9/10/11).
(defun column-info-to-class-and-args (sql-type column-size decimal-digits)
  (declare (ignore decimal-digits))
  ;; QSS 2026-10-14: columns larger than a bound buffer may be (MySQL/MariaDB
  ;; report TEXT and catalog columns with 16 MB) are read with SQLGetData,
  ;; like LONGVARCHAR -- before: error "column ... larger than maximum size"
  (when (and column-size (> column-size *max-precision*))
    (case sql-type
      ((#.$SQL_CHAR #.$SQL_VARCHAR #.$SQL_WCHAR #.$SQL_WVARCHAR)
       (return-from column-info-to-class-and-args (values 'uclob-column nil)))
      ((#.$SQL_BINARY #.$SQL_VARBINARY)
       (return-from column-info-to-class-and-args (values 'blob-column nil)))))
  (case sql-type
    ((#.$SQL_FLOAT #.$SQL_DOUBLE #.$SQL_REAL) (values 'double-column nil))
    ((#.$SQL_BINARY #.$SQL_VARBINARY)
      (values 'binary-column nil))
    ((#.$SQL_LONGVARBINARY)
      (values 'blob-column nil))
    ((#.$SQL_CHAR #.$SQL_VARCHAR #.$SQL_WCHAR #.$SQL_WVARCHAR)
      (values 'unicode-string-column nil))
    ((#.$SQL_LONGVARCHAR #.$SQL_WLONGVARCHAR)
      (values 'uclob-column nil))
    ((#.$SQL_INTEGER #.$SQL_SMALLINT #.$SQL_TINYINT #.$SQL_BIT)
      (values 'integer-column nil))
    ((#.$SQL_TIMESTAMP #.$SQL_DATE #.$SQL_TYPE_TIMESTAMP #.$SQL_TYPE_DATE)
      (values 'date-column nil))
    ((#.$SQL_TIME #.$SQL_TYPE_TIME)       ; reine Zeit als Text hh:mm:ss
      (values 'unicode-string-column nil))
    (#.$SQL_BIGINT (values 'bigint-column))
    ((-11)  ; uniqueidentifier, guid on sql server
      (values 'binary-column nil))
    ((#.$SQL_NUMERIC #.$SQL_DECIMAL)
     (values (if *decimal-as-rational* 'exact-decimal-column 'double-column) nil))
    (otherwise (values 'unicode-string-column nil))))

;;; this  is an experiment:
;;; maybe it is possible to retrieve all variable size columns (char and binary)
;;; as lob (via sql_get_data)
;;; it seems to work with oracle and sqlite
;;; but Sql server does not like it. I believe SQL server is closest to the
;;; odbc spec. 
;;; I believe that the lob columns have to be the last columns in a
;;; resultset. Sql Server complains at the first fetch with sql_get_data
;;; ex.:  (qss-cl-odbc:exec-query *con* "select 1 as a,'b' as b")
;;; works
;;; (qss-cl-odbc:exec-query *con* "select 1 as a,'b' as b,2 as c") 
;;; does not work: - [Microsoft][SQL Native Client]Invalid Descriptor Index, error code 0, State: S1002.
;;; Documentation
;;; The SQL Server Native Client ODBC driver does not support using SQLGetData to 
;;; retrieve data in random column order. All unbound columns processed with SQLGetData 
;;; must have higher column ordinals than the bound columns in the result set.
(defun column-info-to-class-and-args-prefer-lob (sql-type column-size decimal-digits)
  (declare (ignore decimal-digits column-size))
  (case sql-type
    ((#.$SQL_FLOAT #.$SQL_DOUBLE #.$SQL_REAL) (values 'gd-double-column nil))
    ((#.$SQL_BINARY #.$SQL_VARBINARY #.$SQL_LONGVARBINARY)
      (values 'blob-column nil))
    ((#.$SQL_CHAR #.$SQL_VARCHAR #.$SQL_LONGVARCHAR
      #.$SQL_WCHAR #.$SQL_WVARCHAR #.$SQL_WLONGVARCHAR
      #.$SQL_TIME #.$SQL_TYPE_TIME)
      (values 'uclob-column))
    ((#.$SQL_INTEGER #.$SQL_SMALLINT #.$SQL_TINYINT #.$SQL_BIT)
      (values 'gd-integer-column nil))
    ((#.$SQL_TIMESTAMP #.$SQL_DATE #.$SQL_TYPE_TIMESTAMP #.$SQL_TYPE_DATE)
      (values 'gd-date-column nil))
    (#.$SQL_BIGINT (values 'gd-bigint-column))
    ((-11)  ; uniqueidentifier, guid on sql server
      (values 'blob-column nil))
    ;; QSS 2026-10-14: wie gebunden -- vorher hier immer Double, auch mit
    ;; *DECIMAL-AS-RATIONAL* = T (Waehrung ungenau, sobald USE-BIND-COLUMN NIL)
    ((#.$SQL_NUMERIC #.$SQL_DECIMAL)
     (values (if *decimal-as-rational* 'gd-exact-decimal-column 'gd-double-column) nil))
    (otherwise (values 'uclob-column nil))))


(defun create-column (query pos use-bind)
  (let ((hstmt (slot-value query 'hstmt)))
  (multiple-value-bind (column-name sql-type column-size decimal-digits nullable)
      ;; in odbc, columns are 1 based, in lisp they are 0 based
      (%describe-column hstmt (+ pos 1))
    ;; when retrieving TIMESTAMP from oracle databases we get SQL_NULL_TYPE as sql-type
    ;; and the when we fetch a coredump
    (when (eql sql-type $SQL_TYPE_NULL)
      (error "Column ~A, name ~A is of type SQL_NULL_TYPE, remove the column from the query." (+ pos 1) column-name))
    (multiple-value-bind (column-class args)
      ;; QSS 2026-11-03: siehe INTEGER-AS-BIGINT (SQLite kuerzt sonst auf 32 Bit)
      (let ((read-type (if (and (eql sql-type $SQL_INTEGER)
                                (integer-as-bigint (connection query)))
                           $SQL_BIGINT
                           sql-type)))
        (if use-bind
            (column-info-to-class-and-args read-type column-size decimal-digits)
            (column-info-to-class-and-args-prefer-lob read-type column-size decimal-digits)))
      (let ((column (make-instance column-class
                                   :column-name column-name
                                   :position pos
                                   :hstmt hstmt
                                   :sql-type sql-type
                                   :column-size column-size
                                   :decimal-digits decimal-digits
                                   :ind-ptr (cffi:foreign-alloc 'sql-len)
                                   :nullable nullable)))
        (initialize-column column args)
        (when (column-is-bound column)
          (with-slots (buffer-length value-ptr ind-ptr c-type)
              column
            ;(setf value-ptr (cffi:foreign-alloc :long buffer-length))
;            (setf ind-ptr (cffi:foreign-alloc 'sql-len))
            (%bind-column hstmt 
                          pos
                          c-type
                          value-ptr
                          buffer-length
                          ind-ptr)))
          column)))))

;(defun get-column-value-or-null (column)

(defun get-len (column)
  (cffi:mem-ref (slot-value column 'ind-ptr) 'sql-len))

(defun exec-get-data (column) 
  (%sql-get-data (slot-value column 'hstmt)
                 (slot-value column 'position) ; should be called in right order
                 (slot-value column 'c-type)
                 (slot-value column 'value-ptr)
                 (slot-value column 'buffer-length)
                 (slot-value column 'ind-ptr)))  

;;;-------------------
;;;   unicode-string
;;;------------------- 

;; a simple 16 bit unicode column, in ODBC this is SQL_WCHAR (SQL_WVARCHAR) 
;; and SQL_C_WCHAR 

(defclass unicode-string-column (bound-column) ())

(defmethod initialize-column ((column unicode-string-column) args)
  (declare (ignore args))
  (setf (slot-value column 'c-type) $SQL_C_WCHAR)
  ;; QSS: column-size zaehlt Zeichen; Platz fuer 4 Bytes je Zeichen
  ;; (Surrogatpaare) plus abschliessende 0
  (setf (slot-value column 'buffer-length)
          (if (zerop (slot-value column 'column-size))
            *max-precision*
            (if (> (slot-value column 'column-size) *max-precision*)
              (error "column ~A has length ~A, larger than maximum size ~A" 
                     (let ((name (slot-value column 'column-name)))
                       (if (equalp name "") 
                         (slot-value column 'position)
                         name))
                     (slot-value column 'column-size)
                     *max-precision*)
              (* 4 (1+ (slot-value column 'column-size))))))
  (setf (slot-value column 'value-ptr) 
          (cffi:foreign-alloc :uchar :count (slot-value column 'buffer-length))))

(defmethod get-column-value ((column unicode-string-column))
  (let ((len (cffi:mem-ref (slot-value column 'ind-ptr) 'sql-len)))
    ;; len is size in bytes, not characters!
    (if (= len $SQL_NULL_DATA)
        nil
        (let ((max (- (slot-value column 'buffer-length) 2)))
          (when (> len max)
            (warn "column ~A truncated: ~D bytes, buffer ~D" (slot-value column 'column-name) len max)
            (setf len (logand max (lognot 1))))
          (%get-unicode-string (slot-value column 'value-ptr) len)))))



;;;--------------------
;;; integer column
;;;--------------------

(defclass integer-column (bound-column) ())

(defmethod initialize-column ((column integer-column) args)
  (declare (ignore args))
  (setf (slot-value column 'c-type) $SQL_C_SLONG)
  (setf (slot-value column 'buffer-length)
        (cffi:foreign-type-size 'sql-integer))
  (setf (slot-value column 'value-ptr)
        (cffi:foreign-alloc 'sql-integer)))


(defmethod get-column-value ((column integer-column))
  (let ((len (cffi:mem-ref (slot-value column 'ind-ptr) 'sql-len)))
    (if (= len $SQL_NULL_DATA)
        nil
        (cffi:mem-ref (slot-value column 'value-ptr) 'sql-integer))))


;;;--------------------
;;; integer column 2, via get-data
;;;--------------------

(defclass gd-integer-column (getdata-column) ())

(defmethod initialize-column ((column gd-integer-column) args)
  (declare (ignore args))
  (setf (slot-value column 'c-type) $SQL_C_SLONG)
  (setf (slot-value column 'buffer-length)
        (cffi:foreign-type-size 'sql-integer))
  (setf (slot-value column 'value-ptr)
        (cffi:foreign-alloc 'sql-integer)))


(defmethod get-column-value ((column gd-integer-column))
  (exec-get-data column)
  (let ((len (get-len column)))
    (if (= len $SQL_NULL_DATA)
        nil
      (cffi:mem-ref (slot-value column 'value-ptr) 'sql-integer))))



;;;--------------------
;;; double column
;;;--------------------

(defclass double-column (bound-column) ())

(defmethod initialize-column ((column double-column) args)
  (declare (ignore args))
  (setf (slot-value column 'c-type) $SQL_C_DOUBLE)
  (setf (slot-value column 'buffer-length) 
          (cffi:foreign-type-size :double))
  (setf (slot-value column 'value-ptr) (cffi:foreign-alloc :double)))

(defmethod get-column-value ((column double-column))
  (let ((len (cffi:mem-ref (slot-value column 'ind-ptr) 'sql-len)))
    (if (= len $SQL_NULL_DATA)
        nil
        (progn
          (cffi:mem-ref (slot-value column 'value-ptr) :double)))))

;;;--------------------
;;; double column 2 , get-data
;;;--------------------

(defclass gd-double-column (getdata-column) ()) 

(defmethod initialize-column ((column gd-double-column) args)
  (declare (ignore args))
  (setf (slot-value column 'c-type) $SQL_C_DOUBLE)
  (setf (slot-value column 'buffer-length)
          (cffi:foreign-type-size :double))
  (setf (slot-value column 'value-ptr) (cffi:foreign-alloc :double)))

(defmethod get-column-value ((column gd-double-column))
  (exec-get-data column)
  (let ((len (cffi:mem-ref (slot-value column 'ind-ptr) 'sql-len)))
    (if (= len $SQL_NULL_DATA)
        nil
        (progn
          (cffi:mem-ref (slot-value column 'value-ptr) :double)))))


;;;------------------------
;;; date column
;;;------------------------
(defclass date-column (bound-column) ())

(defmethod initialize-column ((column date-column) args)
  (declare (ignore args)) 
  (setf (slot-value column 'c-type) $SQL_C_TYPE_TIMESTAMP) ; QSS: ODBC 3
  (setf (slot-value column 'buffer-length) 
          (cffi:foreign-type-size '(:struct sql-c-timestamp)))
  (setf (slot-value column 'value-ptr) (cffi:foreign-alloc :uchar :count 32)))

(defmethod get-column-value ((column date-column))
  (let ((len (cffi:mem-ref (slot-value column 'ind-ptr) 'sql-len)))
    (if (= len $SQL_NULL_DATA)
        nil
        (funcall *universal-time-to-date-dataype*
                 (timestamp-to-universal-time (slot-value column 'value-ptr))))))

;;;------------------------
;;; date column 2.
;;;------------------------
(defclass gd-date-column (getdata-column) ())

(defmethod initialize-column ((column gd-date-column) args)
  (declare (ignore args)) 
  (setf (slot-value column 'c-type) $SQL_C_TYPE_TIMESTAMP) ; QSS: ODBC 3
  (setf (slot-value column 'buffer-length) 
          (cffi:foreign-type-size '(:struct sql-c-timestamp)))
  (setf (slot-value column 'value-ptr) (cffi:foreign-alloc :uchar :count 32)))

(defmethod get-column-value ((column gd-date-column))
  (exec-get-data column)
  (let ((len (get-len column)))
    (if (= len $SQL_NULL_DATA)
        nil
      (progn
        (funcall *universal-time-to-date-dataype*
                 (timestamp-to-universal-time (slot-value column 'value-ptr)))))))

;;;--------------------------
;;; binary column
;;;--------------------------

(defclass binary-column (bound-column) ())

(defmethod initialize-column ((column binary-column) args)
  (declare (ignore args))
  (setf (slot-value column 'c-type) $SQL_C_BINARY)
  (setf (slot-value column 'buffer-length) 
          (if (zerop (slot-value column 'column-size))
            *max-precision*
            (if (> (slot-value column 'column-size) *max-precision*)
              (error "column ~A is to large" (slot-value column 'column-size))
              (slot-value column 'column-size))))
  (setf (slot-value column 'value-ptr)
          (cffi:foreign-alloc :uchar 
                              :count (slot-value column 'buffer-length))))


(defmethod get-column-value ((column binary-column))
  (let ((len (cffi:mem-ref (slot-value column 'ind-ptr) 'sql-len)))
    (if (= len $SQL_NULL_DATA) 
      nil
      ;; QSS: nie ueber das Pufferende lesen. Manche Treiber melden eine zu
      ;; kleine Spaltengroesse (SQLite-ODBC: BLOB als BINARY(255)); dann
      ;; warnen und kuerzen. Vollstaendig liest (setf use-bind-column nil).
      (let ((max (slot-value column 'buffer-length)))
        (when (or (= len $SQL_NO_TOTAL) (> len max))
          (warn "binary column ~A truncated to ~D bytes (driver reported size ~D); use (setf (use-bind-column con) nil)"
                (slot-value column 'column-name) max (slot-value column 'column-size))
          (setf len max))
        (get-byte-vector (slot-value column 'value-ptr) len)))))

;;;----------------------------
;;; bigint column
;;;----------------------------
(defclass bigint-column (bound-column) ())

(defmethod initialize-column ((column bigint-column) args)
  (declare (ignore args))
  (setf (slot-value column 'c-type) $SQL_C_CHAR)
  ;; bigint is 64 bit, 2^64 has 20 digits, additional 1 sign =21 chars, 
  ;; say 25 for safety
  (setf (slot-value column 'buffer-length) 25)
  (setf (slot-value column 'value-ptr) (cffi:foreign-alloc :uchar :count 25)))

(defmethod get-column-value ((column bigint-column))
  (let ((len (cffi:mem-ref (slot-value column 'ind-ptr) 'sql-len)))
    (if (= len $SQL_NULL_DATA)
        nil
        (parse-integer (get-string (slot-value column 'value-ptr) len)))))


;;;----------------------------
;;; bigint column 2, with get-data
;;;----------------------------
(defclass gd-bigint-column (getdata-column) ())

(defmethod initialize-column ((column gd-bigint-column) args)
  (declare (ignore args))
  (setf (slot-value column 'c-type) $SQL_C_CHAR)
  ;; bigint is 64 bit, 2^64 has 20 digits, additional 1 sign =21 chars, 
  ;; say 25 for safety
  (setf (slot-value column 'buffer-length) 25)
  (setf (slot-value column 'value-ptr) (cffi:foreign-alloc :uchar :count 25)))

(defmethod get-column-value ((column gd-bigint-column))
  (exec-get-data column)
  (let ((len (get-len column)))
    (if (= len $SQL_NULL_DATA)
        nil
      (parse-integer (get-string (slot-value column 'value-ptr) len)))))

;;;-----------------------------
;;; uclob column
;;;-----------------------------
(defclass uclob-column (getdata-column) ())

(defmethod initialize-column ((column uclob-column) args)
  (declare (ignore args))
  (setf (slot-value column 'c-type) $SQL_C_WCHAR)
  (setf (slot-value column 'buffer-length) *max-precision*))

(defmethod get-column-value ((column uclob-column))
  (let* ((value-ptr (cffi:foreign-alloc :char :count (slot-value column 'buffer-length))))
    (unwind-protect
      (get-unicode-character-data 
       (slot-value column 'hstmt)
       (slot-value column 'position)
       value-ptr 
       (slot-value column 'buffer-length)
       (slot-value column 'ind-ptr))
      (cffi:foreign-free value-ptr))))
        
;;;-----------------------------
;;; blob column
;;;-----------------------------
(defclass blob-column (getdata-column) ())

(defmethod initialize-column ((column blob-column) args)
  (declare (ignore args))
  (setf (slot-value column 'c-type) $SQL_C_BINARY)
  (setf (slot-value column 'buffer-length) *max-precision*))

(defmethod get-column-value ((column blob-column))
  (let* ((value-ptr (cffi:foreign-alloc :uchar 
                                        :count (slot-value column 'buffer-length))))
    (unwind-protect
      (get-binary-data 
       (slot-value column 'hstmt)
       (slot-value column 'position)
       value-ptr 
       (slot-value column 'buffer-length)
       (slot-value column 'ind-ptr))
      (cffi:foreign-free value-ptr))))

;;; the version for 16bit unicode 

;; QSS 2026-10-13: alle Teile als Bytes sammeln und einmal als UTF-16LE
;; wandeln -- ein Surrogatpaar kann an einer Teilgrenze liegen.
(defun get-unicode-character-data (hstmt position value-ptr buffer-length
                                   ind-ptr)
  (flet ((check (code)
           (unless (or (= code $SQL_SUCCESS)
                       (= code $SQL_SUCCESS_WITH_INFO))
             (multiple-value-bind (code2 condition)
                 (error-handling-fun code nil nil hstmt)
               (declare (ignore code2))
               (error condition))))
         (fetch ()
           (%sql-get-data-raw hstmt position $SQL_C_WCHAR value-ptr buffer-length ind-ptr)))
    (let ((sqlret (fetch))
          (octets (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
          (chunk (logand (- buffer-length 2) (lognot 1))))
      (check sqlret)
      (let ((len (cffi:mem-ref ind-ptr 'sql-len)))
        (when (= len $SQL_NULL_DATA)
          (return-from get-unicode-character-data nil))
        (loop
          (setf len (cffi:mem-ref ind-ptr 'sql-len))
          (if (and (= sqlret $SQL_SUCCESS_WITH_INFO)
                   (or (= len $SQL_NO_TOTAL) (> len chunk))
                   (equal (sql-state nil nil hstmt) "01004"))
              ;; Puffer voll, ohne die abschliessende 0
              (progn
                (loop for i from 0 below chunk
                      do (vector-push-extend (cffi:mem-aref value-ptr :uint8 i) octets))
                (setf sqlret (fetch))
                (check sqlret))
              (progn
                (loop for i from 0 below len
                      do (vector-push-extend (cffi:mem-aref value-ptr :uint8 i) octets))
                (return))))
        (wchar-bytes-to-string (coerce octets '(simple-array (unsigned-byte 8) (*))))))))
    
(defun get-binary-data (hstmt position value-ptr buffer-length ind-ptr)
  ;; local error handling, we can not use the general error handling
  ;; since this resets the sql-state
  ;; anyway the normal error handling would warn because of 
  ;; data truncation which is in this procedure not an abnormal condition
  
  (flet ((handle-error (code)
           (unless (or (= code $SQL_SUCCESS)
                       (= code $SQL_SUCCESS_WITH_INFO))
             (multiple-value-bind (code2 condition)
                 (error-handling-fun code nil nil hstmt)
               (declare (ignore code2))
               (error condition)))))
 
    (let* ((sqlret (%sql-get-data-raw hstmt
                                      position
                                      $SQL_C_BINARY
                                      value-ptr
                                      buffer-length
                                      ind-ptr)))
      (handle-error sqlret)
      (let ((len (cffi:mem-ref ind-ptr 'sql-len)))
        (if (= len $sql_null_data)
            nil
            (let ((res (make-array 0
                                   :element-type '(unsigned-byte 8)
                                   :adjustable t))
                  (res-len 0))
              (loop
               (if (and (= sqlret $SQL_SUCCESS_WITH_INFO)
                        (equal (sql-state nil nil hstmt)
                               "01004"))
            
                   (let ((vec (get-byte-vector value-ptr buffer-length)))
                     (setf res (adjust-array res (+ res-len buffer-length)))
                     (setf (subseq res res-len (+ res-len buffer-length)) vec)
                     (setf res-len (length res))
                     (setf sqlret (%sql-get-data-raw hstmt
                                                     position
                                                     $SQL_C_BINARY
                                                     value-ptr
                                                     buffer-length
                                                     ind-ptr))
                     (handle-error sqlret))
                   (return)))
        
              (setf len (cffi:mem-ref ind-ptr 'sql-len))
              (let ((vec (get-byte-vector value-ptr len)))
                (setf res (adjust-array res (+ res-len len)))
                (setf (subseq res res-len (+ res-len len)) vec))
              res))))))
;;;--------------------------------------------------------------------
;;; QSS 2026-10-13: exakte Dezimalspalte
;;; Der Wert wird als SQL_C_CHAR geholt (ODBC schreibt hier laut Spezifikation
;;; immer einen Punkt) und exakt in eine rationale Zahl gewandelt. Ein Komma
;;; wird zur Sicherheit ebenfalls als Dezimaltrenner angenommen.

(defun parse-decimal-string (string)
  (let* ((s (string-trim " " string))
         (neg (and (plusp (length s)) (char= (char s 0) #\-)))
         (s (if (and (plusp (length s)) (member (char s 0) '(#\- #\+))) (subseq s 1) s))
         (dot (position-if (lambda (c) (member c '(#\. #\,))) s))
         (int-part (if dot (subseq s 0 dot) s))
         (frac-part (if dot (subseq s (1+ dot)) "")))
    (unless (and (every #'digit-char-p int-part) (every #'digit-char-p frac-part)
                 (plusp (+ (length int-part) (length frac-part))))
      (error "not a decimal number: ~S" string))
    (let ((n (+ (* (if (zerop (length int-part)) 0 (parse-integer int-part))
                   (expt 10 (length frac-part)))
                (if (zerop (length frac-part)) 0 (parse-integer frac-part)))))
      (/ (if neg (- n) n) (expt 10 (length frac-part))))))

(defclass exact-decimal-column (bound-column) ())

(defmethod initialize-column ((column exact-decimal-column) args)
  (declare (ignore args))
  (setf (slot-value column 'c-type) $SQL_C_CHAR)
  (setf (slot-value column 'buffer-length) 64)
  (setf (slot-value column 'value-ptr)
        (cffi:foreign-alloc :char :count 64)))

(defmethod get-column-value ((column exact-decimal-column))
  (let ((len (cffi:mem-ref (slot-value column 'ind-ptr) 'sql-len)))
    (if (= len $SQL_NULL_DATA)
        nil
        (parse-decimal-string (get-string (slot-value column 'value-ptr) (min len 63))))))

;; QSS 2026-10-14: exakte Dezimalzahl ueber SQLGetData (USE-BIND-COLUMN NIL)
(defclass gd-exact-decimal-column (getdata-column) ())

(defmethod initialize-column ((column gd-exact-decimal-column) args)
  (declare (ignore args))
  (setf (slot-value column 'c-type) $SQL_C_CHAR)
  (setf (slot-value column 'buffer-length) 64)
  (setf (slot-value column 'value-ptr)
        (cffi:foreign-alloc :char :count 64)))

(defmethod get-column-value ((column gd-exact-decimal-column))
  (exec-get-data column)
  (let ((len (cffi:mem-ref (slot-value column 'ind-ptr) 'sql-len)))
    (if (= len $SQL_NULL_DATA)
        nil
        (parse-decimal-string (get-string (slot-value column 'value-ptr) (min len 63))))))
