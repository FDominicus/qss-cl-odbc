;;; -*- Mode: Lisp -*-

;; qss-cl-odbc, derived from plain-odbc (ODBC module for clisp)
;; Copyright (C) Roland Averkamp 2005
;; Roland.Averkamp@gmx.de
;; the license agreement can be found in file license.txt

(in-package :qss-cl-odbc)

(defvar *open-connections* nil)


;; the main class, this a a wrapper around the connection
;; handle from odbc
;; an instance can only be created by connecting,
;; via connect, driver-connect or the special connect functions 
;; for oracle, sql-server or access
;; (make-instance 'odbc-connection ...) is useless
;; once a connection is closed it can not be reopened

(defclass odbc-connection ()
  (;; any reason to have more than one henv?
   (henv :initform nil :allocation :class :initarg :henv :accessor henv)
   (hdbc :initform nil :initarg :hdbc :accessor hdbc)
   (connected-p :initform nil :reader CONNECTED-P)
   (server-name :reader server-name)
   (dbms-name :reader dbms-name)
   (user-name :reader user-name)
   ;; info returned from SQLGetInfo
   (info :initform (make-hash-table) :reader db-info)
   (use-bind :initform t :accessor use-bind-column)
   ;; QSS 2026-10-14: Kodierung der ANSI-Schnittstelle dieser Verbindung
   ;; (:string/:clob-Parameter, SQL_C_CHAR-Spalten). Haengt am Treiber, nicht am
   ;; Betriebssystem: Access unter Windows will die ANSI-Codepage (Windows-1252),
   ;; der SQLite-ODBC-Treiber (Werner) immer UTF-8. Beim Verbinden festgelegt.
   (ansi-encoding :initform nil :accessor connection-ansi-encoding)
   ;; QSS 2026-11-03: SQL_INTEGER-Spalten als SQL_C_SBIGINT lesen. Der
   ;; SQLite-ODBC-Treiber (Werner, 0.99991) meldet jeden ganzzahligen Ausdruck
   ;; ohne deklarierten Typ als SQL_INTEGER (sum(), Literale, auch CAST AS
   ;; BIGINT) und kuerzt beim Lesen als SQL_C_SLONG stillschweigend auf 32 Bit
   ;; (3000000000 -> -1294967296, ohne 22003). SQLite rechnet immer mit 64 Bit.
   ;; Beim Verbinden fuer SQLite auf T gesetzt.
   (integer-as-bigint :initform nil :accessor integer-as-bigint))
  #+cormanlisp (:metaclass cl::class-slot-class)
  )

;; QSS 2026-10-14: Jeder Einstieg mit Verbindung (Abfrage, Prepare, Parameter,
;; Katalog) bindet *ANSI-ENCODING* an die Kodierung seiner Verbindung. So
;; koennen Verbindungen mit verschiedener Kodierung gleichzeitig offen sein,
;; auch verschachtelt (DO-QUERY auf Access, darin EXEC-UPDATE auf SQLite).
(defmacro with-connection-encoding ((connection) &body body)
  `(let ((*ansi-encoding* (or (connection-ansi-encoding ,connection) *ansi-encoding*)))
     ,@body))

(defun check-ansi-encoding (encoding)
  "Fehler bei einer Kodierung, die babel/CFFI nicht kennt (z. B. Tippfehler)."
  (handler-case (babel-encodings:get-character-encoding encoding)
    (error ()
      (error "qss-cl-odbc: unknown ANSI encoding ~S (e.g. :cp1252, :utf-8, :latin-1)" encoding)))
  encoding)

(defun get-odbc-info (con info-type)
  (with-slots (hdbc info) con
    (or (gethash info-type info)
        (setf (gethash info-type info)
              (%sql-get-info hdbc info-type)))))

;; once a connection is created, fill some further fields

(defun build-connection-string (list)
  (if (null list)
    ""
    (let ((sos (make-string-output-stream)))
      (dolist (e list)
        (write-string (string (car e)) sos)
        (write-string "=" sos)
        (write-string (string (cdr e)) sos)
        (write-string ";" sos))
      (let ((res (get-output-stream-string sos)))
        (subseq res 0 (- (length res) 1)))))) 

(defvar *connect-autocommit* nil
  "NIL (Vorgabe): Verbindungen ohne Autocommit, jede Aenderung braucht COMMIT.
Achtung: dann haelt auch jedes SELECT eine Transaktion offen, bis COMMIT oder
ROLLBACK -- SQLite sperrt solange Schreiber, Access kann alte Staende zeigen.
T: Autocommit bleibt an (empfohlen fuer Verbindungen, die nur lesen).")

(defvar *odbc3-environment-p* nil
  "T, sobald die gemeinsame Umgebung von dieser Fassung mit ODBC 3 angelegt wurde.")

(defun driver-connect (connection-string &key ansi-encoding)
 "ANSI-ENCODING: Kodierung fuer :string/:clob dieser Verbindung; NIL = der Wert
von *ANSI-ENCODING* beim Verbinden (spaetere Aenderungen der Variablen wirken
auf diese Verbindung nicht mehr)."
 (let ((con (make-instance 'odbc-connection)))
   (setf (connection-ansi-encoding con)
         (check-ansi-encoding (or ansi-encoding *ansi-encoding*)))
   ;; QSS: die Umgebung gilt fuer den ganzen Lisp-Prozess. Stammt sie noch von
   ;; einer aelteren Fassung (ODBC 2, SQLAllocEnv), neu anlegen -- sonst haengen
   ;; ODBC-3-Verbindungen an einer ODBC-2-Umgebung (Speicherfehler unter Windows).
   (unless (and (henv con) *odbc3-environment-p*) ;; has class allocation!
     (setf (henv con) (%new-environment-handle))
     (setf *odbc3-environment-p* t))
   (setf (hdbc con) (%new-db-connection-handle (henv con)))
   (let ((completion (%sql-driver-connect 
                      (henv con) (hdbc con)  connection-string
                      :noprompt)))
     (declare (ignore completion))
     ;;;
     (push con *open-connections*)
     (setf (slot-value con 'connected-p) t)
     (let* ((server-name (get-odbc-info con $SQL_SERVER_NAME))
            (dbms-name (get-odbc-info con $SQL_DBMS_NAME))
            (is-txn-capable (/= (get-odbc-info con $SQL_TXN_CAPABLE) 
                               $SQL_TC_NONE))
            (user-name
               ;; rav
               ;; for mysql/unoixodbc (get-odbc-info con $SQL_USER_NAME) core dumps
               ;; not known if this is mysql specific or unixodbc specific
             ;; rav, 20.12.2007 remove the check, it works with mysql 5.0 on windows
               ;(if (equalp dbms-name "MySQL")
               ;  "unknown"
                 (get-odbc-info con $SQL_USER_NAME)
               ;  )
                 ))
             
       ;; QSS: *CONNECT-AUTOCOMMIT* = T laesst Autocommit an (fuer reine Leser:
       ;; sonst haelt jedes SELECT eine Transaktion offen, bis COMMIT/ROLLBACK)
       (when (and is-txn-capable (not *connect-autocommit*))
         (disable-autocommit (hdbc con)))
       (setf (slot-value con 'server-name) server-name)
       (setf (slot-value con 'dbms-name) dbms-name)
       (setf (slot-value con 'user-name) user-name)
       (when (search "sqlite" (string-downcase (or dbms-name "")))
         (setf (integer-as-bigint con) t))
       con))))

(defun connect (dsn userid password)
  (let ((str (build-connection-string (list (cons :dsn dsn)
                                           (cons :uid userid)
                                           (cons :pwd password)))))
    (driver-connect str)))

(defun split-connect-attributes (attrs)
  "QSS 2026-10-14: trennt :ANSI-ENCODING (Einstellung von qss-cl-odbc) von den
Schluesseln der Verbindungszeichenfolge. Rueckgabe: Paare (Schluessel . Wert)
fuer die Zeichenfolge und die Kodierung (oder NIL)."
  (unless (evenp (length attrs))
    (error "keyword value pairs required"))
  (let ((pairs nil) (encoding nil))
    (do ((attrs attrs (cddr attrs)))
        ((null attrs))
      (let ((key (car attrs)) (value (cadr attrs)))
        (unless (keywordp key)
          (error "expected a keyword, got ~S" key))
        (cond ((eq key :ansi-encoding) (setf encoding value))
              ((stringp value) (push (cons key value) pairs))
              (t (error "expected a string for ~S, got ~S" key value)))))
    (values (nreverse pairs) encoding)))

(defun connect-generic (&rest attrs)
  "Schluessel/Werte der Verbindungszeichenfolge, z. B. :dsn \"TESTDB\".
Zusaetzlich :ansi-encoding (Keyword, z. B. :utf-8) fuer diese Verbindung."
  (multiple-value-bind (pairs encoding) (split-connect-attributes attrs)
    (driver-connect (build-connection-string pairs) :ansi-encoding encoding)))


(defmethod print-object ((connection odbc-connection) s)
  (format s "#<~A SERVER=~S DBMS=~S USER=~S>"
          (class-name (class-of connection))
          (if (slot-boundp connection 'server-name) 
            (slot-value connection 'server-name)
            "")
          (if (slot-boundp connection 'dbms-name) 
            (slot-value connection 'dbms-name)
            "")
          (if (slot-boundp connection 'user-name) 
            (slot-value connection 'user-name)
            "")))



;; before the connection is closed, it is rolled back.
;; odbc complains if a connection with an uncomitted transaction
;; is closed. Committing the transaction would be wrong.

(defun close-connection (con)
  (when (connected-p con)
    ;; if there is an active transaction and we disconnect then
    ;; odbc creates an error message
    ;; anyway, a transaction must be explicitly commited
    (rollback con)
    (%disconnect (hdbc con))
    (%free-connection (hdbc con))
    (SLOT-MAKUNBOUND con 'hdbc)
    (setf (slot-value con 'connected-p) nil)
    (setf *open-connections* (remove con *open-connections*))
    nil))

(defun trace-connection (con filename)
  (%start-connection-trace (hdbc con) filename)
  nil)

(defun untrace-connection (coN)
  (%stop-connection-trace (hdbc con))
  nil) 


(defun commit (con)
  (%commit (henv con) (hdbc con))
  nil)

(defun rollback (con)
  (%rollback (henv con) (hdbc con))
  nil)

;; this class is a wrapper for odbc statement handles.
;; it is visible to the user only as prepared statement
;; the only way to create a odbc-query is to create a 
;; prepared statement

;; fixme: what is the difference between statement and prepared-statement

(defclass odbc-query ()
  ((connection :initarg :connection :reader connection)
   (active-p :initarg :active-p :initform nil)
   (hstmt :initform nil :initarg :hstmt :accessor hstmt) ; = cursor??
   (columns :initform nil)
   (column-count :initform nil :accessor column-count)
   (parameters :initarg :parameters :accessor parameters)
   (use-bind :initform t :initarg :use-bind)))

(defun use-bind-p (q)
  (slot-value q 'use-bind))

(defun make-query (con)
  (let ((new-query (make-instance 'odbc-query
                                  :connection con
                                  :active-p nil
                                  ;; column-count should be nil
                                  ;;(clone-database database)
                                  :active-p t
                                  :use-bind (slot-value con 'use-bind))))
   (setf (hstmt new-query) (%new-statement-handle (hdbc con)))
   new-query))


(defclass prepared-statement (odbc-query) ())
 
;; ((parameters :initarg :parameters :accessor parameters)))

(defun make-prepared-statement (con)
  (let ((new-query (make-instance 'prepared-statement
                                  :connection con
                                  :active-p nil
                                  ;; column-count should be nil
                                  ;;(clone-database database)
                                  :active-p t
                                  :use-bind (slot-value con 'use-bind))))
   (setf (hstmt new-query) (%new-statement-handle (hdbc con)))
   new-query))




(defun column-names (query)
  (let ((columns (slot-value query 'columns))
        (res nil))
    (dotimes (i (length columns))
      (push (slot-value (aref columns i) 'column-name) res))
    (nreverse res)))

(defun bind-columns (query columncount)
  (with-slots (hstmt 
               columns
               column-count) 
              query 
      (when (zerop columncount)
        (error "can not bind columns, there is no result set"))
      (setf column-count columncount)
      ;; QSS 2026-10-14: NIL, not 0 -- if binding fails halfway, UNBIND-COLUMNS
      ;; skips the empty places (before: MISSING-SLOT on 0 hid the real error)
      (setf columns (make-array column-count :initial-element nil))
      (dotimes (pos column-count)
        ;; the columns are 0 based, at least here
        (let ((col (create-column query pos (use-bind-p query))))
          (setf (aref columns pos) col)))))

(defun unbind-columns (query)
  (let ((columns (slot-value query 'columns))
        (hstmt (slot-value query 'hstmt)))
    (when columns
      (dotimes (i (length columns))
        (let ((column (aref columns i)))
          (when column
            (with-slots (value-ptr ind-ptr) column
              (when value-ptr
                (cffi:foreign-free value-ptr))
              (when ind-ptr
                (cffi:foreign-free ind-ptr))))))
      (setf (slot-value query 'columns) nil)
      (when hstmt
        (%free-statement hstmt :unbind)))))
  
(defmethod free-query ((query odbc-query))
  (unbind-columns query)
  (%free-statement (hstmt query) :close)
  (%free-statement (hstmt query) :drop)
  (SLOT-MAKUNBOUND query 'hstmt))

(defmethod free-query ((query prepared-statement))
  (free-parameters query)
  (call-next-method))

(defun free-statement (x)
  (free-query x))


(defmethod fetch-query-results ((query odbc-query))
  ;nil
  
  (with-slots (hstmt columns column-count)
      query
      (let ((res nil))
        (loop
          (when (= (%sql-fetch hstmt) $SQL_NO_DATA_FOUND)
            (return-from fetch-query-results (nreverse res)))
          (let ((row nil))
            (dotimes (i column-count)
              ;(get-column-value (aref columns i))
              (push (get-column-value (aref columns i)) row))
            (push (nreverse row) res)))))) 

;;; on hold
#+ignore
(defmethod fetch-row ((query odbc-query))
  (with-slots (hstmt columns column-count)
      query
    (if (= (%sql-fetch hstmt) $SQL_NO_DATA_FOUND)
      nil
      (let ((row nil))
        (dotimes (i column-count)
          (push (get-column-value (aref columns i)) row))
        (nreverse row)))))

;; convert a obj to a parameter specification
;; a parameter specification is a list of 
;; parameter-type on of :integer :string :clob .....
;; parameter direction :in or :out :in-out
;; further parameter for the parameter, ex. for a string this is the length
;;  
;; we accept normal objects i.e. non cons the parameter specification s derived
;; or a list -->the first element is the value for the parameter
;; the cdr is (keyword direction .....) -> (keyword direction ....)
;;            (keyword )    -> (keyword :in)
;;            ()            -> (:string :in)
;;  this function returns four values: 
;; the value, parameter-type direction args 

;; QSS 2026-10-13: Textparameter ohne Laengenangabe bekommen die Laenge des
;; Werts statt *DEFAULT-STRING-PARAMETER-SIZE* (8001 ist fuer Access-Textfelder
;; mit hoechstens 255 Zeichen unpassend).
;; QSS: Laengen in den Einheiten, die der Treiber zaehlt: UTF-16-Einheiten fuer
;; Unicode-Typen (ein Zeichen jenseits U+FFFF zaehlt doppelt), Bytes der
;; ANSI-Kodierung fuer :string/:clob, Bytes fuer :blob. Nie 0.
(defun parameter-length (value type)
  (max 1 (ecase type
           ((:unicode-string :uclob :unicode-clob) (/ (wide-octets value) 2))
           ((:string :clob) (babel:string-size-in-octets value :encoding *ansi-encoding*))
           ((:blob :binary) (length value)))))

(defun default-length-args (value type args)
  (cond ((and (null args)
              (member type '(:string :unicode-string :clob :uclob :unicode-clob))
              (stringp value))
         (list (parameter-length value type)))
        ((and (null args) (member type '(:blob :binary)) (vectorp value))
         (list (parameter-length value type)))
        ;; QSS: Genauigkeit und Nachkommastellen aus dem Wert
        ((and (null args) (eq type :decimal) (rationalp value))
         (decimal-precision-and-scale value))
        (t args)))

(defun object-to-parameter-spec (obj)
  (typecase obj
    (list
      (if (not obj)
        (values nil :string :in '(1))
        (let ((value (car obj))
              (rest (cdr obj)))
          (cond 
            ((not rest) (error "parameter without a valid type spec: ~A" obj))
            ((not (cdr rest)) (values value (car rest) :in (default-length-args value (car rest) nil)))
            (t (values value (first rest) (second rest)
                       (default-length-args value (first rest) (cddr rest))))))))
    ;; QSS 2026-10-13: Texte immer als UTF-16 (SQL_WVARCHAR) binden, nie mit
    ;; Spaltengroesse 0 (Access: HY104 "Invalid precision value").
    (string (values obj :unicode-string :in (list (parameter-length obj :unicode-string))))
    ;; QSS: Ganzzahlen ueber 32 Bit und Brueche exakt als Dezimalzahl
    ;; (vorher: 32-Bit-Ueberlauf bzw. Fehler)
    ((signed-byte 32) (values obj :integer :in nil))
    (rational (values obj :decimal :in (decimal-precision-and-scale obj)))
    (float (values (coerce obj 'double-float) :double :in nil))
    (array (values obj  :binary :in (list (max 1 (length obj)))))
    (t (if (funcall *date-type-predicate* obj)
         (values obj :date :in)
         (error "not able to deduce parameter specification for ~A" obj)))))

;; QSS 2026-10-14: Parameter binden und ausfuehren als eigene Funktion, damit
;; EXEC-SQL-STATEMENT (alles auf einmal) und MAP-QUERY (zeilenweise) sie teilen.
(defun %bind-and-execute (query connection sql parameter-list)
  (setf (parameters query) (make-array (length parameter-list) 
                                       :initial-element nil))
  (let ((pos 0))
    (dolist (param parameter-list)
      (multiple-value-bind (value type direction args) 
          (object-to-parameter-spec param)
        (let ((parameter (create-parameter query pos type direction args)))
          (bind-parameter (hstmt query) pos parameter)
          (setf (aref (parameters query) pos) parameter)
          (unless (eql direction :out)
            (set-parameter-value parameter value))
          (incf pos)))))
  (let ((res (%sql-exec-direct sql (hstmt query) (henv connection) (hdbc connection)))
        (last-pos nil))
    (when (= res $SQL_NEED_DATA)
      (loop
        (multiple-value-bind (res pos)
            (sql-param-data-position (hstmt query))
          (unless (= res $SQL_NEED_DATA)
            (return))
          (when (eql pos last-pos)
            (error "paramter ~A is not filled yet" pos))
          (setf last-pos pos)
          (let ((param (aref (slot-value query 'parameters) pos)))
            (send-parameter-data param (hstmt query))))))))

(defun %free-direct-query (query)
  ;; QSS: auch die Parameter freigeben -- vorher verlor jede Abfrage mit
  ;; Parametern ihren Fremdspeicher (nur vorbereitete Anweisungen gaben ihn frei)
  (unwind-protect (free-parameters query)
    (free-query query)))

(defun exec-sql-statement (connection sql parameter-list)
  (with-connection-encoding (connection)
    (%exec-sql-statement connection sql parameter-list)))

(defun %exec-sql-statement (connection sql parameter-list)
  (let ((query (make-query connection)))
    (unwind-protect
      (progn
        (%bind-and-execute query connection sql parameter-list)
        (let ((res-list nil)
              (row-count (result-rows-count (hstmt query))))
          (loop
            (let ((column-count (result-columns-count (hstmt query))))
            (when (zerop column-count) (return))
            (bind-columns query column-count)
            (let ((res (fetch-query-results query ))
                  (names (coerce (column-names query) 'list)))
              (push (list res names) res-list)
              (unbind-columns query)
              (unless (%sql-more-results (hstmt query))
                (return)))))
          (let ((return-parameters (get-parameters query)))
            (values row-count (nreverse res-list) return-parameters))))
      (%free-direct-query query))))

;;; ----------------------------------------------------------------
;;; QSS 2026-10-14: zeilenweise Abfrage
;;; Statt alle Zeilen als Liste zu sammeln, wird FUNCTION fuer jede Zeile
;;; aufgerufen; im Speicher liegt immer nur die aktuelle Zeile. Gedacht fuer
;;; grosse Tabellen (Uebertragung Access -> SQLite). Nur die erste Ergebnismenge.
;;; Verlaesst FUNCTION die Schleife vorzeitig (RETURN, Fehler, THROW), wird die
;;; Anweisung trotzdem freigegeben.

(defun map-query* (function connection sql parameter-list)
  "Ruft FUNCTION mit jeder Ergebniszeile (Liste) auf. Rueckgabe: Anzahl der
Zeilen und die Spaltennamen."
  (with-connection-encoding (connection)
    (%map-query* function connection sql parameter-list)))

(defun %map-query* (function connection sql parameter-list)
  (let ((query (make-query connection))
        (count 0))
    (unwind-protect
         (progn
           (%bind-and-execute query connection sql parameter-list)
           (let ((column-count (result-columns-count (hstmt query))))
             (when (zerop column-count)
               (error "map-query: statement returns no result set: ~A" sql))
             (bind-columns query column-count)
             (let ((names (coerce (column-names query) 'list)))
               (with-slots (hstmt columns) query
                 (loop until (= (%sql-fetch hstmt) $SQL_NO_DATA_FOUND)
                       do (let ((row nil))
                            (dotimes (i column-count)
                              (push (get-column-value (aref columns i)) row))
                            (funcall function (nreverse row))
                            (incf count))))
               (values count names))))
      (%free-direct-query query))))

(defun map-query (function connection sql &rest parameter-list)
  (map-query* function connection sql parameter-list))

(defmacro do-query (((&rest vars) connection sql &rest parameters) &body body)
  "Fuehrt BODY fuer jede Ergebniszeile aus; VARS werden an die Spalten gebunden
(fehlende Spalten NIL). Mit (RETURN) vorzeitig verlassen. Rueckgabe: Anzahl
der Zeilen, bei RETURN dessen Wert.
  (do-query ((nr name) con \"SELECT id, name FROM t WHERE x = ?\" 5)
    (print (list nr name)))"
  (let ((row (gensym "ROW")))
    `(block nil
       (map-query* (lambda (,row)
                     (destructuring-bind (&optional ,@vars &rest ignore) ,row
                       (declare (ignore ignore))
                       ,@body))
                   ,connection ,sql (list ,@parameters)))))

(defun exec-sql* (connection sql parameter-list)
  (exec-sql-statement connection sql parameter-list))

(defun exec-sql (connection sql &rest parameter-list)
  (exec-sql* connection sql parameter-list))



(defun exec-query* (connection sql parameter-list)
  (multiple-value-bind (rows result-sets out-params)
      (exec-sql-statement connection sql parameter-list)
    (declare (ignore rows) (ignore out-params))
    (let ((res nil))
      (dolist (result-set result-sets)
        (push (first result-set) res)
        (push (second result-set) res))
      (values-list (nreverse res)))))

(defun exec-query (connection sql &rest parameter-list)
  (exec-query* connection sql parameter-list))

(defun exec-update* (connection sql parameter-list)
  (multiple-value-bind (rows result-sets out-params)
      (exec-sql-statement connection sql parameter-list)
    (declare (ignore result-sets out-params))
    rows))

(defun exec-update (connection sql &rest parameter-list)
  (exec-update* connection sql parameter-list))

(defun exec-command* (connection sql parameter-list)
  (multiple-value-bind (rows result-sets out-params)
      (exec-sql-statement connection sql parameter-list)
    (declare (ignore rows result-sets))
    (values-list out-params)))

(defun exec-command (connection sql &rest parameter-list)
  (exec-command* connection sql parameter-list))

(defun call-metadata-func (connection fun)
  (with-connection-encoding (connection)
    (%call-metadata-func connection fun)))

(defun %call-metadata-func (connection fun)
  (let ((query (make-query connection)))
    (unwind-protect 
        (progn
          (funcall fun (hstmt query))
          (let ((column-count (result-columns-count (hstmt query))))
            (when (zerop column-count) 
              (return-from %call-metadata-func nil))
            (bind-columns query column-count)
            (values (fetch-query-results query) (coerce (column-names query) 'list))))
      (free-query query))))

(defun get-primary-keys (connection catalog-name schema-name table-name)
  (check-type catalog-name (or null string))
  (check-type schema-name (or null string))
  (check-type table-name (or null string))
  (call-metadata-func connection
   (lambda (hstmt)
     (%sql-primary-keys hstmt catalog-name schema-name table-name))))

(defun get-tables (connection catalog-name schema-name table-name table-type)
  (check-type catalog-name (or null string))
  (check-type schema-name (or null string))
  (check-type table-name (or null string))
  (check-type table-type (or null string))
  (call-metadata-func connection
   (lambda (hstmt)
     (%sql-tables hstmt catalog-name schema-name table-name table-type))))

(defun get-columns (connection catalog-name schema-name table-name column-name)
  (check-type catalog-name (or null string))
  (check-type schema-name (or null string))
  (check-type table-name (or null string))
  (check-type column-name (or null string))
  (call-metadata-func connection
   (lambda (hstmt)
     (%sql-columns hstmt catalog-name schema-name table-name column-name))))

(defun get-foreign-keys (connection 
                         catalog-name1 schema-name1 table-name1
                         catalog-name2 schema-name2 table-name2)
  (check-type catalog-name1 (or null string))
  (check-type schema-name1 (or null string))
  (check-type table-name1 (or null string))
  (check-type catalog-name2 (or null string))
  (check-type schema-name2 (or null string))
  (check-type table-name2 (or null string))
  (call-metadata-func connection
   (lambda (hstmt)
     (%sql-foreign-keys hstmt 
                        catalog-name1 schema-name1 table-name1
                        catalog-name2 schema-name2 table-name2))))



(defmethod prepare-statement :around ((connection odbc-connection) sql &rest parameter-list)
  (declare (ignore sql parameter-list))
  (with-connection-encoding (connection) (call-next-method)))

(defmethod prepare-statement ((connection odbc-connection) sql &rest parameter-list)
  (with-slots (hdbc) connection
    (let* ((query (make-prepared-statement connection))
           (pos 0)
           (hstmt (hstmt query)))
      (%sql-prepare hstmt sql)
      (setf (parameters query) (make-array (length parameter-list) 
                                           :initial-element nil))
      (dolist (param parameter-list)
        (multiple-value-bind (type direction args)
            (if (and param (symbolp param))
                (values param :in nil)
              (values (first param)
                      (second param)
                      (rest (rest param))))
          (let ((parameter (create-parameter query pos type direction args)))
            (bind-parameter hstmt pos parameter)
            (setf (aref (parameters query) pos) parameter) 
            (incf pos))))
      query)))

;; QSS 2026-10-14: vorbereitete Anweisungen kodieren wie ihre Verbindung
(defmethod set-parameters :around ((query odbc-query) parameter-values)
  (declare (ignore parameter-values))
  (with-connection-encoding ((connection query)) (call-next-method)))

(defmethod get-parameters :around ((query odbc-query))
  (with-connection-encoding ((connection query)) (call-next-method)))

(defmethod set-params-and-exec :around ((query odbc-query) parameters)
  (declare (ignore parameters))
  (with-connection-encoding ((connection query)) (call-next-method)))

(defmethod exec-prepared-query :around ((query prepared-statement) &rest parameters)
  (declare (ignore parameters))
  (with-connection-encoding ((connection query)) (call-next-method)))

(defmethod set-parameters ((query odbc-query) parameter-values)
  (let ((i 0)
        (parameters (parameters query)))
    (dolist (pval parameter-values)
      (loop
        (when (>= i (length parameters))
          (error "too many actual parameters"))
        (if (eql (slot-value (aref parameters i) 'direction) :out)
          (incf i)
          (return)))
      (let* ((parameter (aref parameters i))) 
        (set-parameter-value parameter pval)
        (incf i)))
    (do ((j i (+ j 1)))
        ((>= j (length parameters)))
      (unless (eql :out (slot-value (aref parameters j) 'direction))
        (error "not enough actual parameters")))))

(defmethod get-parameters ((query odbc-query))
  (let ((res nil)
        (parameters (parameters query)))
    (dotimes (i (length parameters))
      (let* ((parameter (aref parameters i))
             (direction (slot-value parameter 'direction)))
        (when (or (eq direction :out) (eq direction :inout))
          (push (get-parameter-value parameter) res))))
    (nreverse res)))


(defmethod set-params-and-exec ((query odbc-query) parameters)
  (set-parameters query parameters)
  (let ((hstmt (hstmt query)))
    (unwind-protect
      (let ((res (%sql-execute (hstmt query)))
            (last-pos nil))
        (if (= res $SQL_NEED_DATA)
          (loop
            (multiple-value-bind (res pos)
                (sql-param-data-position hstmt)
              (unless (= res $SQL_NEED_DATA)
                (return)) ; from the loop
              (when (eql pos last-pos)
                (error "paramter ~A is not filled yet" pos))
              (setf last-pos pos)
              (let ((param (aref (slot-value query 'parameters) pos)))
                (send-parameter-data param hstmt)))))))))

;; this functions works only, since we store at value-ptr the position
;; of the parameter
(defun sql-param-data-position (hstmt)
  (with-temporary-allocations
      ((ptr (cffi:foreign-alloc :pointer)))
    (let ((res (with-error-handling (:hstmt hstmt)
                   (%sql-param-data hstmt ptr))))
      (values res (if (= res $SQL_NEED_DATA)
                      (cffi:mem-ref (cffi:mem-ref ptr :pointer) :long))))))



(defmethod exec-prepared-query ((query prepared-statement) &rest parameters)
  (let ((hstmt (hstmt query)))
    (unwind-protect 
      (progn
        (set-params-and-exec query parameters)
    ;;; fixme
        ;; if this query has already been used, we reuse the old
        ;; column binding. This breaks down if the schema has been changed
        ;; in the mean time, ex. the query is "select * from table1" and then
        ;; a column of table1 is dropped.
        (let ((no-of-columns (result-columns-count hstmt)))
          (when (zerop no-of-columns )
            (error "there is no result set"))
          (if (column-count query)
            (unless (= (column-count query) no-of-columns)
              (error "the number of columns has changed"))
            (bind-columns query no-of-columns)))
        (values (fetch-query-results query)
                (coerce (column-names query) 'list)))
    (%free-statement hstmt :close))))


(defmethod exec-prepared-update ((query prepared-statement) &rest parameters)
  (let ((hstmt (hstmt query)))
    (unwind-protect 
      (progn
        (set-params-and-exec query parameters)
        (let ((rowcount (result-rows-count hstmt)))
          (if (= rowcount -1) nil rowcount)))
      (%free-statement hstmt :close))))

(defmethod exec-prepared-command ((query prepared-statement) &rest parameters)
  (let ((hstmt (hstmt query)))
    (unwind-protect 
      (progn
        (set-params-and-exec query parameters)
        (get-parameters query))
      (%free-statement hstmt :close))))

