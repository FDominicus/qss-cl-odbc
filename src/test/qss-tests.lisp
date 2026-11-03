;;;; qss-tests.lisp -- Tests fuer qss-cl-odbc, FiveAM
;;;;
;;;; Zwei Teile:
;;;;   :unit         ohne Datenbank, laeuft ueberall
;;;;   :integration  gegen eine echte Verbindung; jeder Fehler, der am
;;;;                 13.10.2026 gefunden wurde, hat hier mindestens einen Test
;;;;
;;;; Aufruf:
;;;;   (asdf:test-system :qss-cl-odbc)
;;;; oder gezielt:
;;;;   (qss-cl-odbc-test:run-all :dsn "TESTDB")
;;;;   (qss-cl-odbc-test:run-unit)
;;;;
;;;; Die Verbindung kommt aus :DSN, sonst aus der Umgebungsvariable
;;;; QSS_ODBC_DSN. Ohne DSN laufen nur die Unit-Tests.
;;;;
;;;; Wie in racket/db haengen einzelne Tests an Merkmalen der Datenbank
;;;; (*FEATURES-OF-DB*), die aus dem DBMS-Namen der Verbindung abgeleitet werden.
;;;; Bekannt: Access, SQLite. Andere Datenbanken laufen mit der SQLite-DDL und
;;;; ohne die Merkmale.
;;;;
;;;; Die Integrationstests legen die Tabelle QSS_ODBC_T an und loeschen sie
;;;; wieder. Access lehnt CREATE TABLE ueber ODBC bei migrierten Dateien mit
;;;; alten Objektrechten ab (-1907) -- dann eine frische .accdb nehmen.

(defpackage :qss-cl-odbc-test
  (:use :cl :fiveam)
  (:export #:run-all #:run-unit #:run-integration #:run-chinook #:*dsn*))

(in-package :qss-cl-odbc-test)

(defvar *dsn* nil "DSN fuer die Integrationstests; NIL = Umgebungsvariable QSS_ODBC_DSN.")
(defvar *con* nil)
(defvar *features-of-db* '())

(def-suite :qss-cl-odbc :description "qss-cl-odbc")
(def-suite :unit :in :qss-cl-odbc :description "ohne Datenbank")
(def-suite :integration :in :qss-cl-odbc :description "gegen eine ODBC-Verbindung")

;;; ------------------------------------------------------------------
;;; Testdaten

(defparameter +cp1252-text+ "Müller Straße ÄÖÜ äöü ß €"
  "Nur Zeichen aus Windows-1252, geht auch ueber die ANSI-Schnittstelle.")

(defparameter +unicode-text+ "Łódź ✓ Ωmega Ж €"
  "Zeichen ausserhalb von Windows-1252, braucht durchgehend UTF-16.")

(defun long-text (n)
  (let ((unit "Größe ÄÖÜ äöü ß € 0123456789|"))
    (with-output-to-string (s)
      (loop for i from 0 below n by (length unit)
            do (write-string unit s :end (min (length unit) (- n i)))))))

;;; ==================================================================
;;; Unit-Tests

(in-suite :unit)

(test sql-len-is-pointer-sized
  "SQLLEN/SQLULEN muessen zeigerbreit sein (Windows 64 Bit: 8 Byte)."
  (is (= (cffi:foreign-type-size :pointer) (cffi:foreign-type-size 'qss-cl-odbc::sql-len)))
  (is (= (cffi:foreign-type-size :pointer) (cffi:foreign-type-size 'qss-cl-odbc::sql-u-len))))

(test civil-days-known-values
  (is (= 0 (qss-cl-odbc::days-from-civil 1970 1 1)))
  (is (= -25567 (qss-cl-odbc::days-from-civil 1900 1 1)))
  (is (= -25202 (qss-cl-odbc::days-from-civil 1901 1 1)))
  (is (= -25569 (qss-cl-odbc::days-from-civil 1899 12 30))))

(test civil-days-roundtrip
  (is-true (loop for d from -800000 to 800000 by 997
                 always (= d (multiple-value-call #'qss-cl-odbc::days-from-civil
                               (qss-cl-odbc::civil-from-days d))))))

(test universal-time-before-1900
  "Access legt reine Uhrzeiten auf 1899-12-30; SBCL kann das selbst nicht."
  (let ((ut (qss-cl-odbc::encode-universal-time* 0 15 8 30 12 1899)))
    (is (minusp ut))
    (is (equal '(0 15 8 30 12 1899)
               (subseq (multiple-value-list (qss-cl-odbc::decode-universal-time* ut)) 0 6)))))

(test universal-time-roundtrip
  (is-true (loop for ut from -5000000000 to 4000000000 by 3333331
                 always (= ut (multiple-value-call #'qss-cl-odbc::encode-universal-time*
                                (qss-cl-odbc::decode-universal-time* ut))))))

(test universal-time-early-1900
  "Oestlich von UTC liegt 1900-01-01 00:xx Ortszeit vor Universal Time 0."
  (let ((ut (qss-cl-odbc::encode-universal-time* 0 30 0 1 1 1900)))
    (is (equal '(0 30 0 1 1 1900)
               (subseq (multiple-value-list (qss-cl-odbc::decode-universal-time* ut)) 0 6)))))

(test date-time-zone-wall-clock
  "Mit *DATE-TIME-ZONE* = 0 kommt jede Wanduhrzeit unveraendert zurueck, auch an
den Tagen der Zeitumstellung."
  (let ((qss-cl-odbc:*date-time-zone* 0))
    (dolist (wall '((2026 3 29 2 30 0) (2026 10 25 2 30 0) (1899 12 30 8 15 0)
                    (1776 7 4 12 34 56) (2026 12 31 23 59 59)))
      (is (equal wall (multiple-value-list
                       (qss-cl-odbc:decode-date (apply #'qss-cl-odbc:encode-date wall))))))))

(test encode-date-differs-from-local-encode
  "Falle: ENCODE-UNIVERSAL-TIME rechnet in lokaler Zeit. Nur ENCODE-DATE passt
zu dem, was qss-cl-odbc schreibt (ausser die lokale Zone ist UTC)."
  (let ((qss-cl-odbc:*date-time-zone* 0))
    (is (= (encode-universal-time 0 0 12 1 7 2026 0) (qss-cl-odbc:encode-date 2026 7 1 12 0 0)))))

(test parse-decimal-string
  (is (= 5474/5 (qss-cl-odbc::parse-decimal-string "1094.8000")))
  (is (= -1/2 (qss-cl-odbc::parse-decimal-string "-0.5")))
  (is (= 9223372036854775/10000 (qss-cl-odbc::parse-decimal-string "922337203685.4775")))
  (is (= 12 (qss-cl-odbc::parse-decimal-string "12")))
  (is (= 13/4 (qss-cl-odbc::parse-decimal-string "3,25")))
  (is (= 1/2 (qss-cl-odbc::parse-decimal-string ".5")))
  (signals error (qss-cl-odbc::parse-decimal-string "abc")))

(test parameter-length-never-zero
  "Spaltengroesse 0 fuehrt bei Access zu HY104 bzw. abgeschnittenen LOBs."
  (is (equal '(1) (qss-cl-odbc::default-length-args "" :unicode-string nil)))
  (is (equal '(3) (qss-cl-odbc::default-length-args "abc" :string nil)))
  (is (equal '(5) (qss-cl-odbc::default-length-args "abcde" :uclob nil)))
  (is (equal '(5) (qss-cl-odbc::default-length-args "abcde" :clob nil)))
  (is (equal '(100) (qss-cl-odbc::default-length-args "abc" :string '(100))))
  (is (null (qss-cl-odbc::default-length-args 42 :integer nil)))
  ;; Laenge in UTF-16-Einheiten: ein Zeichen jenseits U+FFFF zaehlt doppelt
  (is (equal '(4) (qss-cl-odbc::default-length-args "x😀y" :unicode-string nil)))
  (is (equal '(4) (qss-cl-odbc::default-length-args "x😀y" :uclob nil)))
  (let ((qss-cl-odbc::*ansi-encoding* :utf-8))
    (is (equal '(7) (qss-cl-odbc::default-length-args "Größe" :string nil)))))

(test implicit-string-is-unicode
  (multiple-value-bind (value type direction args) (qss-cl-odbc::object-to-parameter-spec "Größe")
    (is (equal "Größe" value))
    (is (eq :unicode-string type))
    (is (eq :in direction))
    (is (equal '(5) args)))
  (multiple-value-bind (value type direction args) (qss-cl-odbc::object-to-parameter-spec "x😀y")
    (declare (ignore value direction))
    (is (eq :unicode-string type))
    (is (equal '(4) args)))
  (multiple-value-bind (value type direction args) (qss-cl-odbc::object-to-parameter-spec "")
    (declare (ignore value direction))
    (is (eq :unicode-string type))
    (is (equal '(1) args))))

(test split-connect-attributes
  ":ansi-encoding gehoert nicht in die Verbindungszeichenfolge."
  (multiple-value-bind (pairs enc)
      (qss-cl-odbc::split-connect-attributes '(:dsn "X" :ansi-encoding :utf-8 :uid "u"))
    (is (equal '((:dsn . "X") (:uid . "u")) pairs))
    (is (eq :utf-8 enc))
    (is (equal "DSN=X;UID=u" (qss-cl-odbc::build-connection-string pairs))))
  (multiple-value-bind (pairs enc) (qss-cl-odbc::split-connect-attributes '(:dsn "X"))
    (is (equal '((:dsn . "X")) pairs))
    (is (null enc)))
  (signals error (qss-cl-odbc::split-connect-attributes '(:dsn)))
  (signals error (qss-cl-odbc::split-connect-attributes '(:dsn 5)))
  (signals error (qss-cl-odbc::split-connect-attributes '("dsn" "X"))))

(test check-ansi-encoding
  (is (eq :utf-8 (qss-cl-odbc::check-ansi-encoding :utf-8)))
  (is (eq :cp1252 (qss-cl-odbc::check-ansi-encoding :cp1252)))
  (signals error (qss-cl-odbc::check-ansi-encoding :cp-1252))
  (signals error (qss-cl-odbc::check-ansi-encoding "utf-8")))

(test feature-mismatch-text
  (let ((*features-of-db* '(:sqlite :driver-rounds-doubles)))
    (is (equal "ohne EXACT-NUMERIC" (feature-mismatch '(:exact-numeric))))
    (is (equal "hat DRIVER-ROUNDS-DOUBLES" (feature-mismatch '(:without :driver-rounds-doubles))))
    (is (equal "ohne ACCESS; hat SQLITE" (feature-mismatch '(:access :without :sqlite))))))

(test generated-file-is-current
  "src/odbc/odbc-api-generated.lisp entspricht dem, was der Generator aus
odbc-headers/ und tools/odbc-api-spec.lisp erzeugt (sonst: neu erzeugen)."
  (let ((file (asdf:system-relative-pathname :qss-cl-odbc "src/odbc/odbc-api-generated.lisp")))
    (is (equal (remove #\Return (uiop:read-file-string file :external-format :utf-8))
               (remove #\Return (qss-cl-odbc-generate:generated-text)))
        "odbc-api-generated.lisp ist veraltet: (qss-cl-odbc-generate:generate)")))

(test generator-lisp-names
  (is (equal "%%sql-get-diag-rec-w" (qss-cl-odbc-generate:lisp-function-name "SQLGetDiagRecW")))
  (is (equal "%%sql-fetch" (qss-cl-odbc-generate:lisp-function-name "SQLFetch")))
  (is (equal "%%sql-num-result-cols" (qss-cl-odbc-generate:lisp-function-name "SQLNumResultCols"))))

(test pointer-sized-types-match-platform
  "SQLLEN/SQLULEN zeigerbreit, wie die Header es unter _WIN64 verlangen."
  (is (= (cffi:foreign-type-size :pointer) (cffi:foreign-type-size 'qss-cl-odbc::sql-len)))
  (is (= (cffi:foreign-type-size :pointer) (cffi:foreign-type-size 'qss-cl-odbc::sql-u-len)))
  (is (= 4 (cffi:foreign-type-size 'qss-cl-odbc::sql-integer)))
  (is (= 2 (cffi:foreign-type-size 'qss-cl-odbc::sql-small-int)))
  (is (= 16 (cffi:foreign-type-size '(:struct qss-cl-odbc::sql-c-timestamp))))
  (is (= (if (= 8 (cffi:foreign-type-size :pointer)) -27 -18) qss-cl-odbc::$SQL_C_BOOKMARK)))

(test return-code-classification
  "ERROR-HANDLING-FUN / CHECK-RESULT ohne Handle (keine Diagnose abrufbar)."
  (flet ((cond-of (code) (nth-value 1 (qss-cl-odbc::error-handling-fun code nil nil nil))))
    (is (null (cond-of qss-cl-odbc::$SQL_SUCCESS)))
    (is (null (cond-of qss-cl-odbc::$SQL_NO_DATA)))
    (is (null (cond-of qss-cl-odbc::$SQL_NEED_DATA)))
    (is (null (cond-of qss-cl-odbc::$SQL_SUCCESS_WITH_INFO)) "Info ohne Diagnosesatz: nichts melden")
    (is (typep (cond-of qss-cl-odbc::$SQL_ERROR) 'qss-cl-odbc::sql-error))
    (is (search "no diagnostic record" (princ-to-string (cond-of qss-cl-odbc::$SQL_ERROR))))
    (is (typep (cond-of qss-cl-odbc::$SQL_INVALID_HANDLE) 'qss-cl-odbc::sql-error))
    (is (typep (cond-of qss-cl-odbc::$SQL_STILL_EXECUTING) 'qss-cl-odbc::sql-error))
    (is (typep (cond-of 4711) 'qss-cl-odbc::sql-error)))
  (is (eql qss-cl-odbc::$SQL_NO_DATA (qss-cl-odbc::check-result qss-cl-odbc::$SQL_NO_DATA)))
  (signals qss-cl-odbc::sql-error (qss-cl-odbc::check-result qss-cl-odbc::$SQL_ERROR)))

(test next-capacity
  "Puffer fuer Texte des Treibers wachsen, wenn sie voll sind."
  (is (null (qss-cl-odbc::next-capacity 512 100)))
  (is (null (qss-cl-odbc::next-capacity 512 511)))
  (is (eql 1024 (qss-cl-odbc::next-capacity 512 512)))  ; voll: vielleicht gekuerzt (psqlODBC)
  (is (eql 3041 (qss-cl-odbc::next-capacity 512 3040))) ; volle Laenge gemeldet (ODBC)
  (is (eql 32000 (qss-cl-odbc::next-capacity 20000 20000)))
  (is (null (qss-cl-odbc::next-capacity 32000 32000))))

(test oversized-columns-read-as-lob
  "MySQL/MariaDB melden TEXT- und Katalogspalten mit 16 MB: solche Spalten werden
mit SQLGetData gelesen statt mit einem gebundenen Puffer (vorher: Fehler)."
  (flet ((cls (type size) (qss-cl-odbc::column-info-to-class-and-args type size 0)))
    (let ((big (1+ qss-cl-odbc::*max-precision*)))
      (is (eq 'qss-cl-odbc::uclob-column (cls qss-cl-odbc::$SQL_WVARCHAR big)))
      (is (eq 'qss-cl-odbc::uclob-column (cls qss-cl-odbc::$SQL_VARCHAR 16777216)))
      (is (eq 'qss-cl-odbc::blob-column (cls qss-cl-odbc::$SQL_VARBINARY big)))
      (is (not (eq 'qss-cl-odbc::uclob-column (cls qss-cl-odbc::$SQL_WVARCHAR 100)))))))

(test sql-len-data-at-exec
  "Wie das Makro SQL_LEN_DATA_AT_EXEC(length) in sqlext.h."
  (is (= -100 (qss-cl-odbc::%sql-len-data-at-exec 0)))
  (is (= -110 (qss-cl-odbc::%sql-len-data-at-exec 10))))

(test ansi-octet-lengths
  "Laengen fuer die ANSI-Schnittstelle in Bytes, nicht in Zeichen."
  (let ((qss-cl-odbc::*ansi-encoding* :utf-8))
    (is (= 7 (babel:string-size-in-octets "Größe" :encoding qss-cl-odbc::*ansi-encoding*))))
  (let ((qss-cl-odbc::*ansi-encoding* :cp1252))
    (is (= 5 (babel:string-size-in-octets "Größe" :encoding qss-cl-odbc::*ansi-encoding*)))
    (is (= 1 (babel:string-size-in-octets "€" :encoding qss-cl-odbc::*ansi-encoding*)))))

;; ODBC 3.5: UTF-16 einschliesslich Surrogatpaaren
(test utf-16-roundtrip
  (dolist (str (list "" "abc" +cp1252-text+ +unicode-text+ "x😀y" (long-text 300)))
    (is (equal str (qss-cl-odbc::wchar-bytes-to-string (qss-cl-odbc::string-to-wchar-bytes str))))
    (is (= (length (qss-cl-odbc::string-to-wchar-bytes str)) (qss-cl-odbc::wide-octets str))))
  (is (= 8 (qss-cl-odbc::wide-octets "x😀y"))))

(test wide-foreign-string
  (dolist (str (list "" "Größe" "x😀y" +unicode-text+))
    (let ((ptr (qss-cl-odbc::alloc-wide-string str)))
      (unwind-protect (is (equal str (qss-cl-odbc::get-wide-string-nts ptr)))
        (cffi:foreign-free ptr)))))

;;; ==================================================================
;;; Integration

(defun db-feature-p (f) (member f *features-of-db*))

;; MariaDB Connector/ODBC bis 3.1.23 und 3.2.9 (Fehler ODBC-502, behoben im
;; August 2026): die Laenge eines UTF-16-Textes wird nach dem niederwertigen
;; Byte jeder Einheit berechnet; U+xxD8..U+xxDB (z.B. "Ø" = U+00D8) gilt als
;; Beginn eines Surrogatpaars. Folge: beim Schreiben haengt der Treiber
;; Nullzeichen an, beim Lesen meldet er zu wenige Bytes. Hier am Verhalten
;; erkannt, nicht an der Versionsnummer.
(defun utf-16-d8-bug-p (con)
  (let ((back (ignore-errors
               (first (first (qss-cl-odbc:exec-query
                              con "SELECT CONVERT(UNHEX('61C39862') USING utf8mb4)"))))))
    (and (stringp back) (not (equal back (coerce (list #\a (code-char #xD8) #\b) 'string))))))

(defun detect-features (con)
  (let ((dbms (string-downcase (or (ignore-errors (slot-value con 'qss-cl-odbc::dbms-name)) ""))))
    ;; Access BINARY(n) hat feste Laenge: kuerzere Werte kommen mit Nullen aufgefuellt
    (cond ((search "access" dbms) '(:access :exact-numeric :ddl-in-autocommit :binary-fixed-length
                                    :empty-login-ok))
          ;; SQLite-ODBC meldet BLOB als BINARY(255): gebunden nur 255 Bytes
          ;; Eigenheiten des SQLite-ODBC-Treibers (am gespeicherten Wert geprueft):
          ;; meldet BLOB als BINARY(255); schneidet Text am Nullzeichen ab; speichert
          ;; Double mit 15 Stellen; kennt {fn UCASE} usw. nicht; erzwingt keine VARCHAR-Laenge
          ;; PostgreSQL ueber psqlODBC: kein Nullzeichen in TEXT. Fehler in einer
          ;; Transaktion: psqlODBC rollt per Vorgabe nur die Anweisung zurueck
          ;; (Protocol 7.4-2), also wie Access -- nicht :error-aborts-transaction.
          ;; Unter Linux nimmt der Unicode-Treiber keine Nicht-ASCII-Zeichen ueber
          ;; SQL_C_CHAR an ("Could not convert ... to wide chars", auch mit
          ;; setlocale); unter Windows ungeprueft.
          ;; Syntaxfehler: psqlODBC vor 18 verwirft die ganze Transaktion (psqlodbc
          ;; Issue #203, rollback-syntax-error-test; hier mit 16.00 beobachtet).
          ((search "postgres" dbms)
           (append '(:postgresql :exact-numeric :ddl-in-autocommit :driver-truncates-at-nul
                     :out-parameters :multiple-result-sets :foreign-keys :bigint)
                   #-windows '(:ansi-non-ascii-unsupported)
                   (let ((ver (ignore-errors (qss-cl-odbc::get-odbc-info con qss-cl-odbc::$SQL_DRIVER_VER))))
                     (when (and (stringp ver) (string< ver "18"))
                       '(:syntax-error-aborts-transaction)))))
          ;; ANSI-Text (SQL_C_CHAR) nimmt der Treiber immer als UTF-8, auch unter
          ;; Windows (14.10.2026: Bytes von Windows-1252 landeten roh in der Datei).
          ((search "sqlite" dbms) '(:sqlite :ddl-in-autocommit :binary-size-misreported
                                    :driver-truncates-at-nul :driver-rounds-doubles
                                    :no-odbc-fn-escapes :varchar-length-not-enforced
                                    :ansi-is-utf-8 :foreign-keys :bigint :empty-login-ok))
          ;; MySQL / MariaDB (Connector/ODBC): DDL schreibt implizit fest; Text
          ;; wird beim Lesen am Nullzeichen abgeschnitten (Laengenanzeige ebenso,
          ;; in C nachgeprueft, MariaDB Connector/ODBC 3.1.15)
          ((or (search "mysql" dbms) (search "mariadb" dbms))
           (append '(:mysql :exact-numeric :ddl-in-autocommit :bigint :foreign-keys
                     :driver-truncates-at-nul)
                   (when (utf-16-d8-bug-p con) '(:driver-utf-16-d8-bug))))
          ;; SQL Server (ODBC Driver 17/18): TINYINT ohne Vorzeichen
          ((search "microsoft sql server" dbms)
           '(:sqlserver :exact-numeric :ddl-in-autocommit :bigint :foreign-keys
             :unsigned-tinyint :multiple-result-sets))
          (t '()))))

(defun test-table-ddl ()
  (cond
    ((db-feature-p :postgresql)
     "CREATE TABLE qss_odbc_t (id INTEGER PRIMARY KEY, txt VARCHAR(100), amount NUMERIC(19,4), flag SMALLINT, dt TIMESTAMP, dbl DOUBLE PRECISION, n INTEGER, long_text TEXT)")
    ;; MySQL: TEXT nur 64 KB -> LONGTEXT; TIMESTAMP erst ab 1970 -> DATETIME
    ((db-feature-p :mysql)
     "CREATE TABLE qss_odbc_t (id INTEGER PRIMARY KEY, txt VARCHAR(100), amount DECIMAL(19,4), flag SMALLINT, dt DATETIME, dbl DOUBLE, n INTEGER, long_text LONGTEXT)")
    ;; SQL Server: N-Typen fuer Unicode; DATETIME erst ab 1753 -> DATETIME2
    ((db-feature-p :sqlserver)
     "CREATE TABLE qss_odbc_t (id INT PRIMARY KEY, txt NVARCHAR(100), amount DECIMAL(19,4), flag SMALLINT, dt DATETIME2(0), dbl FLOAT, n INT, long_text NVARCHAR(MAX))")
    (t (test-table-ddl-access-or-sqlite))))

(defun test-table-ddl-access-or-sqlite ()
  (if (db-feature-p :access)
      "CREATE TABLE qss_odbc_t (id INTEGER CONSTRAINT pk_qss_odbc_t PRIMARY KEY, txt VARCHAR(100), amount CURRENCY, flag BIT, dt DATETIME, dbl DOUBLE, n INTEGER, long_text LONGTEXT)"
      "CREATE TABLE qss_odbc_t (id INTEGER PRIMARY KEY, txt VARCHAR(100), amount NUMERIC(19,4), flag INTEGER, dt TIMESTAMP, dbl DOUBLE, n INTEGER, long_text TEXT)"))

(defun ddl (sql)
  "DDL mit Autocommit, danach wieder Transaktionen wie bei qss-cl-odbc ueblich."
  (let ((hdbc (qss-cl-odbc::hdbc *con*)))
    (qss-cl-odbc::enable-autocommit hdbc)
    (unwind-protect (qss-cl-odbc:exec-command *con* sql)
      (qss-cl-odbc::disable-autocommit hdbc))))

(defun drop-test-table ()
  (ignore-errors (qss-cl-odbc:rollback *con*))
  (ignore-errors (ddl "DROP TABLE qss_odbc_t")))

(defmacro with-test-table (&body body)
  `(progn
     (drop-test-table)
     (ddl (test-table-ddl))
     (unwind-protect (progn ,@body)
       (drop-test-table))))

(defun q (sql &rest params) (qss-cl-odbc:exec-query* *con* sql params))
(defun q1 (sql &rest params) (caar (apply #'q sql params)))
(defun u (sql &rest params) (qss-cl-odbc:exec-update* *con* sql params))

(defun feature-mismatch (spec)
  "Text fuer SKIP: welche verlangten Merkmale fehlen, welche ausgeschlossenen da sind."
  (let* ((pos (position :without spec))
         (need (subseq spec 0 pos))
         (forbid (and pos (subseq spec (1+ pos))))
         (missing (remove-if #'db-feature-p need))
         (present (remove-if-not #'db-feature-p forbid)))
    (format nil "~@[ohne ~{~A~^, ~}~]~:[~;; ~]~@[hat ~{~A~^, ~}~]"
            missing (and missing present) present)))

(defun features-ok-p (spec)
  "SPEC: Merkmale, die vorhanden sein muessen; nach :WITHOUT solche, die fehlen muessen."
  (let* ((pos (position :without spec))
         (need (subseq spec 0 pos))
         (forbid (and pos (subseq spec (1+ pos)))))
    (and (every #'db-feature-p need)
         (notany #'db-feature-p forbid))))

(defmacro def-db-test (name (&rest required-features) &body body)
  "Test, der nur laeuft, wenn die Datenbank alle REQUIRED-FEATURES hat
(nach :WITHOUT: Merkmale, die fehlen muessen)."
  `(test ,name
     (if (features-ok-p ',required-features)
         (with-test-table ,@body)
         (skip "Datenbank ~A" (feature-mismatch ',required-features)))))

(in-suite :integration)

(test connection-info
  (is (stringp (slot-value *con* 'qss-cl-odbc::dbms-name)))
  (is (plusp (length (slot-value *con* 'qss-cl-odbc::dbms-name)))))

(def-db-test create-alter-drop ()
  "DDL ueber ODBC (Access: scheiterte bei migrierten Dateien an alten Rechten)."
  (is (eql 0 (q1 "SELECT COUNT(*) FROM qss_odbc_t")))
  (finishes (ddl "ALTER TABLE qss_odbc_t ADD COLUMN extra INTEGER"))
  (is (eql 0 (q1 "SELECT COUNT(extra) FROM qss_odbc_t"))))

(def-db-test text-implicit-roundtrip ()
  (is (eql 1 (u "INSERT INTO qss_odbc_t (id, txt) VALUES (?, ?)" 1 +cp1252-text+)))
  (qss-cl-odbc:commit *con*)
  (is (equal +cp1252-text+ (q1 "SELECT txt FROM qss_odbc_t WHERE id = ?" 1))))

(def-db-test text-explicit-unicode-roundtrip ()
  (u "INSERT INTO qss_odbc_t (id, txt) VALUES (?, ?)" 1 (list +cp1252-text+ :unicode-string :in 100))
  (qss-cl-odbc:commit *con*)
  (is (equal +cp1252-text+ (q1 "SELECT txt FROM qss_odbc_t WHERE id = ?" 1))))

(def-db-test text-ansi-roundtrip (:without :ansi-non-ascii-unsupported)
  (u "INSERT INTO qss_odbc_t (id, txt) VALUES (?, ?)" 1 (list +cp1252-text+ :string :in 100))
  (qss-cl-odbc:commit *con*)
  (is (equal +cp1252-text+ (q1 "SELECT txt FROM qss_odbc_t WHERE id = ?" 1))))

(def-db-test text-beyond-cp1252-roundtrip ()
  "Zeichen ausserhalb von Windows-1252 muessen auch beim Lesen heil bleiben."
  (u "INSERT INTO qss_odbc_t (id, txt) VALUES (?, ?)" 1 +unicode-text+)
  (qss-cl-odbc:commit *con*)
  (is (equal +unicode-text+ (q1 "SELECT txt FROM qss_odbc_t WHERE id = ?" 1))))

(def-db-test text-surrogate-pair-roundtrip ()
  "Zeichen jenseits U+FFFF (Surrogatpaar in UTF-16) ueber Parameter und Spalte."
  (u "INSERT INTO qss_odbc_t (id, txt) VALUES (?, ?)" 1 "Wiegung 😀 ok")
  (qss-cl-odbc:commit *con*)
  (is (equal "Wiegung 😀 ok" (q1 "SELECT txt FROM qss_odbc_t WHERE id = ?" 1))))

(def-db-test long-text-unicode-roundtrip ()
  "LOB mit Zeichen ausserhalb Windows-1252 und Surrogatpaaren, mehrere Teile."
  (let ((text (with-output-to-string (s)
                (dotimes (i 4000) (format s "~D ~A 😀|" i +unicode-text+)))))
    (u "INSERT INTO qss_odbc_t (id, long_text) VALUES (?, ?)" 1 (list text :uclob :in))
    (qss-cl-odbc:commit *con*)
    (is (equal text (q1 "SELECT long_text FROM qss_odbc_t WHERE id = ?" 1)))))

(def-db-test sqlstate-is-odbc3 ()
  "ODBC 3: SQLSTATE nach ODBC 3 (HY..., 42...), nicht mehr S1... aus ODBC 2."
  (let ((state (handler-case (progn (q "SELECT * FROM qss_no_such_table") nil)
                 (qss-cl-odbc::sql-error (c) (slot-value c 'qss-cl-odbc::sql-state)))))
    (is (stringp state))
    (is (= 5 (length state)))
    (is (not (string= "S1" (subseq state 0 2))))))

(def-db-test empty-string-parameter ()
  "Leerer String als Parameter (Spaltengroesse 0 -> HY104 bei Access)."
  (is (eql 0 (q1 "SELECT COUNT(*) FROM qss_odbc_t WHERE txt = ?" ""))))

(def-db-test ansi-encoding-follows-connection (:without :ansi-non-ascii-unsupported)
  "QSS 2026-10-14: :string-Parameter und -Spalten kodieren nach der Verbindung,
nicht nach dem aktuellen Wert von *ANSI-ENCODING* (Access und SQLite gleichzeitig)."
  (let* ((own (qss-cl-odbc:connection-ansi-encoding *con*))
         (wrong (if (eq own :utf-8) :cp1252 :utf-8)))
    (let ((qss-cl-odbc:*ansi-encoding* wrong))
      ;; direkte Abfrage
      (u "INSERT INTO qss_odbc_t (id, txt) VALUES (?, ?)" 1 (list +cp1252-text+ :string :in 100))
      (is (equal +cp1252-text+ (q1 "SELECT txt FROM qss_odbc_t WHERE id = ?" 1)))
      (is (eql 1 (q1 "SELECT COUNT(*) FROM qss_odbc_t WHERE txt = ?" (list +cp1252-text+ :string :in 100))))
      ;; vorbereitete Anweisung
      (qss-cl-odbc:with-prepared-statement (st *con* "INSERT INTO qss_odbc_t (id, txt) VALUES (?, ?)"
                                              '(:integer :in) '(:string :in 100))
        (qss-cl-odbc:exec-prepared-update st 2 +cp1252-text+))
      (is (equal +cp1252-text+ (q1 "SELECT txt FROM qss_odbc_t WHERE id = ?" 2)))
      ;; zeilenweise
      (is (equal +cp1252-text+
                 (qss-cl-odbc:do-query ((txt) *con* "SELECT txt FROM qss_odbc_t WHERE txt = ?"
                                       (list +cp1252-text+ :string :in 100))
                   (return txt)))))
    ;; neue Verbindung: Kodierung beim Verbinden festgelegt, spaetere Aenderung wirkt nicht
    (qss-cl-odbc:commit *con*)           ; sonst sieht c2 nichts (SQLite: Sperre)
    (let ((c2 (let ((qss-cl-odbc:*ansi-encoding* wrong))
                (qss-cl-odbc:connect-generic :dsn *dsn* :ansi-encoding own))))
      (unwind-protect
           (progn
             (is (eq own (qss-cl-odbc:connection-ansi-encoding c2)))
             ;; c2 schreibt ueber :string und liest selbst als Unicode zurueck (nicht
             ;; ueber *con*: Access zeigt Aenderungen einer anderen Verbindung verzoegert)
             (let ((qss-cl-odbc:*ansi-encoding* wrong))
               (qss-cl-odbc:exec-update c2 "INSERT INTO qss_odbc_t (id, txt) VALUES (?, ?)"
                                       3 (list +cp1252-text+ :string :in 100)))
             (is (equal +cp1252-text+
                        (caar (qss-cl-odbc:exec-query c2 "SELECT txt FROM qss_odbc_t WHERE id = ?" 3))))
             (qss-cl-odbc:commit c2))
        (ignore-errors (qss-cl-odbc:close-connection c2))))))

(def-db-test text-where-and-like ()
  (u "INSERT INTO qss_odbc_t (id, txt) VALUES (?, ?)" 1 +cp1252-text+)
  (u "INSERT INTO qss_odbc_t (id, txt) VALUES (?, ?)" 2 "anders")
  (qss-cl-odbc:commit *con*)
  (is (eql 1 (q1 "SELECT COUNT(*) FROM qss_odbc_t WHERE txt = ?" +cp1252-text+)))
  (unless (db-feature-p :ansi-non-ascii-unsupported)
    (is (eql 1 (q1 "SELECT COUNT(*) FROM qss_odbc_t WHERE txt = ?" (list +cp1252-text+ :string :in 100)))))
  (is (eql 1 (q1 "SELECT COUNT(*) FROM qss_odbc_t WHERE txt LIKE ?" "Müll%"))))

(def-db-test null-parameter ()
  (u "INSERT INTO qss_odbc_t (id, txt, n) VALUES (?, ?, ?)" 1 nil (list nil :integer :in))
  (qss-cl-odbc:commit *con*)
  (is (equal '((nil nil)) (q "SELECT txt, n FROM qss_odbc_t WHERE id = ?" 1))))

(def-db-test numbers-roundtrip ()
  (u "INSERT INTO qss_odbc_t (id, n, dbl) VALUES (?, ?, ?)" 1 2147483647 0.1d0)
  (u "INSERT INTO qss_odbc_t (id, n, dbl) VALUES (?, ?, ?)" 2 -2147483648 -1.5d300)
  (qss-cl-odbc:commit *con*)
  (is (equal '((2147483647 0.1d0)) (q "SELECT n, dbl FROM qss_odbc_t WHERE id = ?" 1)))
  (is (equal '((-2147483648 -1.5d300)) (q "SELECT n, dbl FROM qss_odbc_t WHERE id = ?" 2))))

(def-db-test flag-roundtrip ()
  (u "INSERT INTO qss_odbc_t (id, flag) VALUES (?, ?)" 1 -1)
  (u "INSERT INTO qss_odbc_t (id, flag) VALUES (?, ?)" 2 0)
  (qss-cl-odbc:commit *con*)
  (is (/= 0 (q1 "SELECT flag FROM qss_odbc_t WHERE id = ?" 1)))
  (is (= 0 (q1 "SELECT flag FROM qss_odbc_t WHERE id = ?" 2))))

(def-db-test timestamp-roundtrip ()
  (let ((dt (qss-cl-odbc:encode-date 2026 10 13 14 30 5)))
    (u "INSERT INTO qss_odbc_t (id, dt) VALUES (?, ?)" 1 (list dt :date :in))
    (qss-cl-odbc:commit *con*)
    (is (eql dt (q1 "SELECT dt FROM qss_odbc_t WHERE id = ?" 1)))
    (is (eql 1 (q1 "SELECT COUNT(*) FROM qss_odbc_t WHERE dt = ?" (list dt :date :in))))))

(def-db-test time-only-roundtrip ()
  "Reine Uhrzeit auf 1899-12-30 (Access-Konvention) hin und zurueck."
  (let ((tm (qss-cl-odbc::encode-universal-time* 0 15 8 30 12 1899)))
    (u "INSERT INTO qss_odbc_t (id, dt) VALUES (?, ?)" 1 (list tm :date :in))
    (qss-cl-odbc:commit *con*)
    (is (eql tm (q1 "SELECT dt FROM qss_odbc_t WHERE id = ?" 1)))))

(def-db-test long-text-roundtrip ()
  "LOB ueber Data-at-exec (vorher: 32-Bit-SQLLEN, Spaltengroesse 0)."
  (let ((text (long-text 20000)))
    (is (eql 1 (u "INSERT INTO qss_odbc_t (id, long_text) VALUES (?, ?)" 1 (list text :uclob :in))))
    (qss-cl-odbc:commit *con*)
    (let ((back (q1 "SELECT long_text FROM qss_odbc_t WHERE id = ?" 1)))
      (is (eql (length text) (length back)))
      (is (equal text back)))))

(def-db-test exact-numeric (:exact-numeric)
  "Waehrung/Decimal exakt als rationale Zahl (vorher Double)."
  (u "INSERT INTO qss_odbc_t (id, amount) VALUES (1, 1094.80)")
  (u "INSERT INTO qss_odbc_t (id, amount) VALUES (2, 922337203685.4775)")
  (qss-cl-odbc:commit *con*)
  (is (eql 5474/5 (q1 "SELECT amount FROM qss_odbc_t WHERE id = 1")))
  (is (eql 9223372036854775/10000 (q1 "SELECT amount FROM qss_odbc_t WHERE id = 2")))
  (is (eql 1 (q1 "SELECT COUNT(*) FROM qss_odbc_t WHERE amount = ?" 1094.8d0))))

(def-db-test row-count ()
  (dotimes (i 3) (u "INSERT INTO qss_odbc_t (id, n) VALUES (?, ?)" (1+ i) 0))
  (qss-cl-odbc:commit *con*)
  (is (eql 3 (u "UPDATE qss_odbc_t SET n = 1")))
  (qss-cl-odbc:commit *con*))

(def-db-test rollback-and-commit ()
  (u "INSERT INTO qss_odbc_t (id, txt) VALUES (?, ?)" 1 "vorher")
  (qss-cl-odbc:commit *con*)
  (u "UPDATE qss_odbc_t SET txt = ? WHERE id = ?" "zurueckgerollt" 1)
  (qss-cl-odbc:rollback *con*)
  (is (equal "vorher" (q1 "SELECT txt FROM qss_odbc_t WHERE id = ?" 1)))
  (u "UPDATE qss_odbc_t SET txt = ? WHERE id = ?" "festgeschrieben" 1)
  (qss-cl-odbc:commit *con*)
  (is (equal "festgeschrieben" (q1 "SELECT txt FROM qss_odbc_t WHERE id = ?" 1))))

(def-db-test error-message-has-diagnostics ()
  "Fehlermeldung mit Text des Treibers (vorher: leer, Speichermuell als Code)."
  (let ((msg (handler-case (progn (q "SELECT * FROM qss_no_such_table") nil)
               (error (c) (princ-to-string c)))))
    (is (stringp msg))
    (is (search "qss_no_such_table" msg :test #'char-equal))
    (is (not (search "no diagnostic record" msg)))))

(def-db-test catalog-columns ()
  (multiple-value-bind (rows cols) (qss-cl-odbc:get-columns *con* nil nil "qss_odbc_t" "%")
    (let* ((i (position "COLUMN_NAME" cols :test #'string-equal))
           (names (mapcar (lambda (r) (string-downcase (nth i r))) rows)))
      (is (member "txt" names :test #'string=))
      (is (member "long_text" names :test #'string=)))))

;;; ==================================================================
;;; Aufruf

(defun run-unit ()
  (run! :unit))

(defun ansi-encoding-for-features (features)
  "Kodierung der ANSI-Schnittstelle nach den Merkmalen; NIL = Vorgabe."
  (when (member :ansi-is-utf-8 features) :utf-8))

(defun connect-test-dsn (&optional (dsn (or *dsn* (uiop:getenv "QSS_ODBC_DSN"))))
  "Weitere Verbindung zum Test-DSN, mit derselben ANSI-Kodierung wie *CON*."
  (let ((enc (ansi-encoding-for-features *features-of-db*)))
    (if enc
        (qss-cl-odbc:connect-generic :dsn dsn :ansi-encoding enc)
        (qss-cl-odbc:connect-generic :dsn dsn))))

(defun run-integration (&key (dsn (or *dsn* (uiop:getenv "QSS_ODBC_DSN"))))
  (cond ((or (null dsn) (string= dsn ""))
         (format t "~&Integrationstests uebersprungen: kein DSN (:dsn oder QSS_ODBC_DSN).~%")
         t)
        (t
         (let ((*dsn* dsn)                ; fuer Tests mit eigener Verbindung
               (*con* (qss-cl-odbc:connect-generic :dsn dsn)))
           (unwind-protect
                (let* ((*features-of-db* (detect-features *con*)))
                  ;; Treiber, die ANSI-Text anders kodieren als die Vorgabe
                  (let ((enc (ansi-encoding-for-features *features-of-db*)))
                    (when enc (setf (qss-cl-odbc:connection-ansi-encoding *con*) enc)))
                  (format t "~&Integration gegen DSN ~A (~A), ANSI-Kodierung ~S, Merkmale ~S~%"
                          dsn (slot-value *con* 'qss-cl-odbc::dbms-name)
                          (qss-cl-odbc:connection-ansi-encoding *con*) *features-of-db*)
                  (run! :integration))
             (ignore-errors (qss-cl-odbc:close-connection *con*)))))))

(defun run-all (&key dsn)
  (let ((unit (run-unit))
        (integration (run-integration :dsn (or dsn *dsn* (uiop:getenv "QSS_ODBC_DSN"))))
        (chinook (uiop:symbol-call :qss-cl-odbc-test :run-chinook)))   ; chinook-tests.lisp
    (and unit integration chinook)))

;;; ==================================================================
;;; Faelle nach racket/db (db-test/tests/db/db/sql-types.rkt, query.rkt)
;;;
;;; Rundreise je Typ ueber eine Tabelle: Wert als Parameter schreiben, lesen,
;;; vergleichen. Bei Text zusaetzlich die Laenge auf der Serverseite.
;;; Abweichungen von racket/db stehen jeweils dabei (Access-Grenzen).

(def-suite :racket-cases :in :integration :description "Faelle nach racket/db")
(in-suite :racket-cases)

(defun types-table-ddl ()
  (cond
    ((db-feature-p :postgresql)
     "CREATE TABLE qss_odbc_types (id INTEGER PRIMARY KEY, i INTEGER, si SMALLINT, ti SMALLINT, big BIGINT, dbl DOUBLE PRECISION, r REAL, amount NUMERIC(19,4), vc VARCHAR(255), long_text TEXT, bin BYTEA, blb BYTEA, dt TIMESTAMP, flag SMALLINT)")
    ;; MySQL: REAL ist dort DOUBLE -> FLOAT fuer einfache Genauigkeit
    ((db-feature-p :mysql)
     "CREATE TABLE qss_odbc_types (id INTEGER PRIMARY KEY, i INTEGER, si SMALLINT, ti TINYINT, big BIGINT, dbl DOUBLE, r FLOAT, amount DECIMAL(19,4), vc VARCHAR(255), long_text LONGTEXT, bin VARBINARY(255), blb LONGBLOB, dt DATETIME, flag SMALLINT)")
    ((db-feature-p :sqlserver)
     "CREATE TABLE qss_odbc_types (id INT PRIMARY KEY, i INT, si SMALLINT, ti TINYINT, big BIGINT, dbl FLOAT, r REAL, amount DECIMAL(19,4), vc NVARCHAR(255), long_text NVARCHAR(MAX), bin VARBINARY(255), blb VARBINARY(MAX), dt DATETIME2(0), flag SMALLINT)")
    (t (types-table-ddl-access-or-sqlite))))

(defun types-table-ddl-access-or-sqlite ()
  ;; Alle Namen in Klammern und keine Typnamen/reservierten Woerter als
  ;; Spaltennamen (Access: MEMO, DEC, TEXT ... -> "Syntax error in field definition")
  (if (db-feature-p :access)
      "CREATE TABLE [qss_odbc_types] ([id] INTEGER CONSTRAINT [pk_qss_odbc_types] PRIMARY KEY, [i] INTEGER, [si] SMALLINT, [ti] BYTE, [big] CURRENCY, [dbl] DOUBLE, [r] REAL, [amount] CURRENCY, [vc] VARCHAR(255), [long_text] LONGTEXT, [bin] BINARY(255), [blb] LONGBINARY, [dt] DATETIME, [flag] BIT)"
      "CREATE TABLE qss_odbc_types (id INTEGER PRIMARY KEY, i INTEGER, si SMALLINT, ti TINYINT, big BIGINT, dbl DOUBLE, r REAL, amount NUMERIC(19,4), vc VARCHAR(255), long_text TEXT, bin VARBINARY(255), blb BLOB, dt TIMESTAMP, flag INTEGER)"))

(defun drop-types-table ()
  (ignore-errors (qss-cl-odbc:rollback *con*))
  (ignore-errors (ddl "DROP TABLE qss_odbc_types")))

(defmacro def-type-test (name (&rest required-features) &body body)
  `(test ,name
     (if (every #'db-feature-p ',required-features)
         (progn
           (drop-types-table)
           (ddl (types-table-ddl))
           (unwind-protect (progn ,@body)
             (drop-types-table)))
         (skip "Datenbank ~A" (feature-mismatch ',required-features)))))

(defun rt (column value &optional (param value))
  "Rundreise: VALUE (bzw. die Parameterangabe PARAM) in COLUMN schreiben und lesen."
  (u "DELETE FROM qss_odbc_types")
  (u (format nil "INSERT INTO qss_odbc_types (id, ~A) VALUES (1, ?)" column) param)
  (qss-cl-odbc:commit *con*)
  (q1 (format nil "SELECT ~A FROM qss_odbc_types WHERE id = 1" column)))

(defun server-length (column)
  "Laenge in Zeichen auf dem Server. MySQL: LENGTH zaehlt Bytes; SQL Server:
LEN laesst Leerzeichen am Ende weg."
  (q1 (format nil "SELECT ~A FROM qss_odbc_types WHERE id = 1"
              (cond ((db-feature-p :access) (format nil "LEN(~A)" column))
                    ((db-feature-p :mysql) (format nil "CHAR_LENGTH(~A)" column))
                    ((db-feature-p :sqlserver) (format nil "LEN(~A + N'x') - 1" column))
                    (t (format nil "LENGTH(~A)" column))))))

(defmacro check-rt (column value &optional (param value) (test '#'equal))
  `(let ((v ,value))
     (is (funcall ,test v (rt ,column v ,param))
         "Rundreise ~A: ~S" ,column v)))

;;; --- Zahlen

(def-type-test rkt-integer ()
  (dolist (v '(5 -1 123456 2147483647 -2147483648)) (check-rt "i" v)))

(def-type-test rkt-smallint ()
  (dolist (v '(5 -1 12345 32767 -32768)) (check-rt "si" v)))

(def-type-test rkt-tinyint ()
  ;; Access BYTE ist vorzeichenlos (0..255), racket/db prueft -128..127
  (dolist (v (if (or (db-feature-p :access) (db-feature-p :unsigned-tinyint))
                '(0 5 127 255) '(5 -1 127 -128)))
    (check-rt "ti" v)))

(def-type-test rkt-integer-beyond-32-bit ()
  "Ganzzahlen ueber 32 Bit werden exakt als Dezimalzahl gebunden (vorher Ueberlauf)."
  ;; Access: CURRENCY (exakt bis +-922337203685477), SQLite: BIGINT
  (dolist (v (if (db-feature-p :access)
                 (list (expt 2 40) (- (expt 2 40)) 922337203685477)
                 (list (expt 2 40) (- (expt 2 40)) (1- (expt 2 63)) (- (expt 2 63)))))
    (check-rt "big" v v #'=)))

(def-type-test rkt-double ()
  (dolist (v '(1.0d0 1.5d0 -5.5d0 1.1d0 -5.8d0 0d0 1d300 -1d-300)) (check-rt "dbl" v)))

(def-type-test rkt-real ()
  ;; nur in Single exakt darstellbare Werte, gelesen als Double
  (dolist (v '(1.0d0 1.5d0 -5.5d0 0.25d0)) (check-rt "r" v)))

(def-type-test rkt-decimal (:exact-numeric)
  ;; racket/db: numeric/decimal; Access CURRENCY hat 4 Nachkommastellen,
  ;; daher ohne 1/400000 und die 20-stelligen Werte
  (dolist (v (list 0 10 -5 1234567890 -1234567890 1/2 1/40 1/10 -1/2 -1/40 -1/10
                   123456789/10000 -123456789/10000))
    (check-rt "amount" v v #'=)))

(test decimal-parameter-type
  "Dezimalparameter als SQL_NUMERIC (Access kennt SQL_DECIMAL nicht)."
  (is (= 2 qss-cl-odbc::*decimal-parameter-sql-type*))
  (multiple-value-bind (value type) (qss-cl-odbc::object-to-parameter-spec 5474/5)
    (is (= 5474/5 value))
    (is (eq :decimal type))))

(test rkt-decimal-not-representable
  "1/3 hat keine endliche Dezimaldarstellung: Fehler statt stillem Runden."
  (signals error (qss-cl-odbc::decimal-string 1/3))
  (is (string= "1094.8" (qss-cl-odbc::decimal-string 5474/5)))
  (is (string= "-0.025" (qss-cl-odbc::decimal-string -1/40)))
  (is (string= "1099511627776" (qss-cl-odbc::decimal-string (expt 2 40)))))

(def-type-test rkt-boolean ()
  (u "INSERT INTO qss_odbc_types (id, flag) VALUES (1, ?)" -1)
  (u "INSERT INTO qss_odbc_types (id, flag) VALUES (2, ?)" 0)
  (u "INSERT INTO qss_odbc_types (id, flag) VALUES (3, ?)" (list nil :integer :in))
  (qss-cl-odbc:commit *con*)
  (is (/= 0 (q1 "SELECT flag FROM qss_odbc_types WHERE id = 1")))
  (is (= 0 (q1 "SELECT flag FROM qss_odbc_types WHERE id = 2")))
  ;; Access: Ja/Nein kennt kein NULL, wird zu 0
  (is (member (q1 "SELECT flag FROM qss_odbc_types WHERE id = 3") '(nil 0))))

;;; --- Text

(defparameter +basic-strings+
  (list "Az0" "this is the time to remember" "it's like that (and that's the way it is)"
        (string #\\) (coerce '(#\\ #\\) 'string) (string #\') (coerce '(#\\ #\') 'string)
        "λ the ultimate"))

(defparameter +intl-strings+
  (list "αβψδεφγηιξκλμνοπρστθωςχυζ"
        "अब्च्देघिज्क्ल्म्नोप्र्स्तुव्य्"
        "شﻻؤيثبلاهتنمةىخحضقسفعرصءغئ"
        "阿あでいおうわぁ"
        "абцдефгхиклмнопљрстувњџзѕЋч"))

(defun check-varchar (str)
  (is (equal str (rt "vc" str)) "Rundreise vc: ~S" str)
  (unless (string= str "")
    (is (eql (length str) (server-length "vc")) "Laenge auf dem Server: ~S" str)))

(def-type-test rkt-varchar-basic ()
  (check-varchar "")
  (dolist (s +basic-strings+) (check-varchar s)))

(def-type-test rkt-varchar-intl ()
  (dolist (s +intl-strings+)
    (check-varchar s)
    (check-varchar (subseq s 0 1)))
  (check-varchar (string (code-char #x3bb)))          ; λ
  (check-varchar (string (code-char #x2200)))         ; ∀
  (check-varchar (make-string 20 :initial-element (code-char #x2200)))
  (check-varchar (make-string 100 :initial-element (code-char #x2200)))
  (check-varchar (apply #'concatenate 'string +intl-strings+)))

(def-type-test rkt-varchar-max ()
  ;; racket/db: 800 Zeichen; Access-Textfelder fassen hoechstens 255
  (check-varchar (make-string 255 :initial-element #\a))
  (check-varchar (make-string 255 :initial-element (code-char #x2200))))

(def-type-test rkt-long-text ()
  (when (db-feature-p :driver-utf-16-d8-bug)
    (skip "Zeichen 1..800: Treiberfehler bei U+xxD8..U+xxDB, siehe UTF-16-LOW-BYTE-D8"))
  (dolist (s (list (make-string 800 :initial-element #\a)
                   ;; racket/db: alle Zeichen 1..800, auch Steuerzeichen
                   (if (db-feature-p :driver-utf-16-d8-bug)
                       (make-string 800 :initial-element #\b)
                       (let ((x (make-string 800)))
                         (dotimes (i 800 x) (setf (char x i) (code-char (1+ i))))))
                   (make-string 100000 :initial-element #\a)
                   (make-string 1000000 :initial-element #\a)))
    (let ((back (rt "long_text" s (list s :uclob :in))))
      (is (eql (length s) (length back)) "Laenge Memo ~D" (length s))
      (is (equal s back) "Memo ~D Zeichen gleich" (length s)))))

;;; --- Binaer

(defun bytes (&rest xs) (coerce xs '(vector (unsigned-byte 8))))
(defun byte-range (from below)
  (let ((v (make-array (- below from) :element-type '(unsigned-byte 8))))
    (dotimes (i (length v) v) (setf (aref v i) (+ from i)))))

(defun padded-equal-p (value back)
  "BACK ist VALUE, rechts mit Nullbytes aufgefuellt."
  (and (>= (length back) (length value))
       (equalp value (subseq back 0 (length value)))
       (every #'zerop (subseq back (length value)))))

(def-type-test rkt-varbinary ()
  (dolist (b (list (babel:string-to-octets "this is the time to remember")
                   (babel:string-to-octets "that's the way it is")
                   (byte-range 0 255)
                   (bytes 0 0 0) (bytes 255)))
    (let ((back (rt "bin" b)))
      (if (db-feature-p :binary-fixed-length)
          (is (padded-equal-p b back)
              "Rundreise bin (feste Laenge): ~D Bytes geschrieben, zurueck ~D Bytes, Anfang ~S"
              (length b) (length back) (subseq back 0 (min 8 (length back))))
          (is (equalp b back)
              "Rundreise bin: ~D Bytes geschrieben, zurueck ~D Bytes, Anfang ~S"
              (length b) (length back) (subseq back 0 (min 8 (length back))))))))

(def-type-test rkt-blob ()
  (let ((old (qss-cl-odbc:use-bind-column *con*)))
    (unwind-protect
         (progn
           ;; Treiber mit falsch gemeldeter Groesse: ueber SQLGetData lesen
           (when (db-feature-p :binary-size-misreported)
             (setf (qss-cl-odbc:use-bind-column *con*) nil))
           (dolist (b (list (byte-range 0 256)
                            (make-array 1000000 :element-type '(unsigned-byte 8) :initial-element 97)))
             (let ((back (rt "blb" b (list b :blob :in))))
               (is (eql (length b) (length back)) "Laenge blob ~D" (length b))
               (is (equalp b back) "blob ~D Bytes gleich" (length b)))))
      (setf (qss-cl-odbc:use-bind-column *con*) old))))

(def-type-test binary-never-overread (:binary-size-misreported)
  "Zu klein gemeldete Spaltengroesse: warnen und kuerzen statt ueber den Puffer lesen."
  (let ((b (byte-range 0 256)))
    (u "INSERT INTO qss_odbc_types (id, blb) VALUES (1, ?)" (list b :blob :in))
    (qss-cl-odbc:commit *con*)
    (let ((back nil))
      (signals warning (setf back (q1 "SELECT blb FROM qss_odbc_types WHERE id = 1")))
      (let ((back (handler-bind ((warning #'muffle-warning))
                    (q1 "SELECT blb FROM qss_odbc_types WHERE id = 1"))))
        (is (equalp (subseq b 0 (length back)) back))))))

;;; --- Datum und Zeit

(defun ut (y mo d &optional (h 0) (mi 0) (s 0))
  (qss-cl-odbc:encode-date y mo d h mi s))

(defun wall-clock (ut)
  "Wanduhrzeit (jahr monat tag std min sek) einer Universal Time aus qss-cl-odbc."
  (if (integerp ut)
      (multiple-value-list (qss-cl-odbc:decode-date ut))
      ut))

(defun check-datetime (y mo d h mi s)
  (let ((back (rt "dt" (ut y mo d h mi s) (list (ut y mo d h mi s) :date :in))))
    (is (equal (list y mo d h mi s) (wall-clock back))
        "Datum ~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D zurueck als ~S" y mo d h mi s (wall-clock back))))

(def-type-test rkt-date ()
  (check-datetime 1776 7 4 0 0 0)
  (check-datetime 2000 1 1 0 0 0)
  (check-datetime 2012 2 14 0 0 0))

(def-type-test rkt-timestamp ()
  (check-datetime 2000 1 1 12 34 56)
  (check-datetime 1776 7 4 12 34 56)
  (check-datetime 2012 2 14 12 34 56))

(def-type-test rkt-time-only ()
  ;; Access hat keinen TIME-Typ; reine Uhrzeit auf 1899-12-30
  (check-datetime 1899 12 30 1 2 3)
  (check-datetime 1899 12 30 12 34 56)
  (check-datetime 1899 12 30 17 30 1))

(def-type-test dst-spring-forward ()
  "Access speichert Wanduhrzeit ohne Zeitzone. 02:30 am Tag der Umstellung auf
Sommerzeit gibt es in Mitteleuropa nicht; zurueck muss trotzdem 02:30 kommen."
  (check-datetime 2026 3 29 2 30 0))

(def-type-test dst-fall-back ()
  "02:30 am Tag der Rueckstellung gibt es zweimal; zurueck muss 02:30 kommen."
  (check-datetime 2026 10 25 2 30 0))

;;; --- Abfragen (query.rkt)

(def-type-test rkt-nulls-in-correct-order ()
  "racket/db, 18.05.2003: NULL-Werte duerfen die Spalten nicht verschieben."
  (u "INSERT INTO qss_odbc_types (id, i, si, dbl, vc) VALUES (1, NULL, 1, NULL, ?)" "x")
  (qss-cl-odbc:commit *con*)
  (is (equal '((nil 1 nil "x")) (q "SELECT i, si, dbl, vc FROM qss_odbc_types")))
  (let ((cols '(1 2 3 4 nil 6 nil 8 9 10 11 12 nil 14 15 nil nil 18 19 20 nil
                nil nil nil nil nil 27 28 29 30 nil 32 33 nil 35)))
    (is (equal (list cols)
               (q (format nil "SELECT ~{~A~^, ~} FROM qss_odbc_types"
                          (mapcar (lambda (c) (if c c "NULL")) cols)))))))

(def-type-test rkt-nested-queries ()
  "Abfragen innerhalb der Schleife ueber ein anderes Ergebnis stoeren sich nicht."
  (dotimes (k 6) (u "INSERT INTO qss_odbc_types (id, vc) VALUES (?, ?)" k (format nil "Nr ~D" k)))
  (qss-cl-odbc:commit *con*)
  (let ((outer (mapcar #'first (q "SELECT id FROM qss_odbc_types WHERE id > 0 AND id < 3 ORDER BY id"))))
    (is (equal (mapcar (lambda (id) (q1 "SELECT vc FROM qss_odbc_types WHERE id = ?" id)) outer)
               (mapcar #'first (q "SELECT vc FROM qss_odbc_types WHERE id > 0 AND id < 3 ORDER BY id"))))))

(def-type-test rkt-errors-nonfatal ()
  "Nach einem Fehler geht die Verbindung weiter."
  (u "INSERT INTO qss_odbc_types (id, i) VALUES (1, 17)")
  (qss-cl-odbc:commit *con*)
  (signals error (q "SELECT nonsuch FROM notthere"))
  (is (eql 17 (q1 "SELECT i FROM qss_odbc_types WHERE id = 1"))))

(def-type-test rkt-multiple-statements (:access)
  "Zwei Anweisungen in einem String: Access lehnt ab (racket/db erwartet einen Fehler)."
  (signals error (q "SELECT id FROM qss_odbc_types; SELECT id FROM qss_odbc_types")))

(def-type-test many-statements-no-leak ()
  "2000 Abfragen hintereinander: Statement-Handles werden freigegeben."
  (u "INSERT INTO qss_odbc_types (id, i) VALUES (1, 1)")
  (qss-cl-odbc:commit *con*)
  (is (eql 2000 (loop repeat 2000 sum (q1 "SELECT i FROM qss_odbc_types WHERE id = ?" 1)))))

(def-type-test rkt-commit-rollback-without-transaction ()
  (finishes (qss-cl-odbc:commit *con*))
  (finishes (qss-cl-odbc:rollback *con*)))

;;; --- vorbereitete Anweisungen

(def-type-test prepared-statements ()
  (let ((ins (qss-cl-odbc:prepare-statement
              *con* "INSERT INTO qss_odbc_types (id, i, vc) VALUES (?, ?, ?)"
              '(:integer :in) '(:integer :in) '(:unicode-string :in 255))))
    (unwind-protect
         (dotimes (k 100)
           (qss-cl-odbc:exec-prepared-update ins k (* k k) (format nil "Größe ~D ✓" k)))
      (qss-cl-odbc:free-statement ins)))
  (qss-cl-odbc:commit *con*)
  (is (eql 100 (q1 "SELECT COUNT(*) FROM qss_odbc_types")))
  (let ((sel (qss-cl-odbc:prepare-statement
              *con* "SELECT i, vc FROM qss_odbc_types WHERE id = ?" '(:integer :in))))
    (unwind-protect
         (progn
           (is (equal '((49 "Größe 7 ✓")) (qss-cl-odbc:exec-prepared-query sel 7)))
           (is (equal '((9801 "Größe 99 ✓")) (qss-cl-odbc:exec-prepared-query sel 99)))
           (is (null (qss-cl-odbc:exec-prepared-query sel 1000))))
      (qss-cl-odbc:free-statement sel))))

(def-type-test prepared-statement-date-and-null ()
  (let ((ins (qss-cl-odbc:prepare-statement
              *con* "INSERT INTO qss_odbc_types (id, dt, vc) VALUES (?, ?, ?)"
              '(:integer :in) '(:date :in) '(:unicode-string :in 50))))
    (unwind-protect
         (progn
           (qss-cl-odbc:exec-prepared-update ins 1 (ut 2026 10 13 18 30 0) "a")
           (qss-cl-odbc:exec-prepared-update ins 2 nil nil))
      (qss-cl-odbc:free-statement ins)))
  (qss-cl-odbc:commit *con*)
  (is (equal '(2026 10 13 18 30 0) (wall-clock (q1 "SELECT dt FROM qss_odbc_types WHERE id = 1"))))
  (is (equal '((nil nil)) (q "SELECT dt, vc FROM qss_odbc_types WHERE id = 2"))))

;;; --- Katalog

(def-type-test catalog-tables ()
  (multiple-value-bind (rows cols) (qss-cl-odbc:get-tables *con* nil nil "qss_odbc_types" nil)
    (let ((i (position "TABLE_NAME" cols :test #'string-equal)))
      (is (member "qss_odbc_types" (mapcar (lambda (r) (nth i r)) rows) :test #'string-equal)))))

(def-type-test catalog-primary-keys ()
  "SQLPrimaryKeys: entweder Ergebnis mit id oder ein sauberer ODBC-Fehler
(der Access-Treiber unterstuetzt die Funktion moeglicherweise nicht)."
  (let ((result (handler-case (multiple-value-list
                               (qss-cl-odbc:get-primary-keys *con* nil nil "qss_odbc_types"))
                  (qss-cl-odbc::sql-error (c) c))))
    (if (typep result 'qss-cl-odbc::sql-error)
        (pass "nicht unterstuetzt: ~A" result)
        (destructuring-bind (rows cols) result
          (let ((i (position "COLUMN_NAME" cols :test #'string-equal)))
            (is (member "id" (mapcar (lambda (r) (nth i r)) rows) :test #'string-equal)))))))

;;; --- Lesen ohne gebundene Spalten (SQLGetData)

(def-type-test getdata-mode ()
  (let ((old (qss-cl-odbc:use-bind-column *con*)))
    (unwind-protect
         (progn
           (setf (qss-cl-odbc:use-bind-column *con*) nil)
           (dolist (s (list "" "Größe ✓" (first +intl-strings+))) (check-varchar s))
           (check-rt "i" 2147483647)
           (check-rt "dbl" 1.5d0)
           (check-datetime 2012 2 14 12 34 56)
           (let ((s (make-string 100000 :initial-element (code-char #x2200))))
             (is (equal s (rt "long_text" s (list s :uclob :in))))))
      (setf (qss-cl-odbc:use-bind-column *con*) old))))

;;; ==================================================================
;;; Zeilenweise Abfrage (MAP-QUERY / DO-QUERY) und Freigabe der Parameter

(def-suite :streaming :in :integration :description "zeilenweise Abfrage")
(in-suite :streaming)

(defvar *freed-parameters* 0)
(defmethod qss-cl-odbc::free-parameter :after ((p qss-cl-odbc::parameter))
  (incf *freed-parameters*))

(defun fill-rows (n)
  (let ((ins (qss-cl-odbc:prepare-statement
              *con* "INSERT INTO qss_odbc_t (id, n, txt) VALUES (?, ?, ?)"
              '(:integer :in) '(:integer :in) '(:unicode-string :in 50))))
    (unwind-protect
         (dotimes (i n) (qss-cl-odbc:exec-prepared-update ins i (mod i 7) (format nil "Zeile ~D ✓" i)))
      (qss-cl-odbc:free-statement ins)))
  (qss-cl-odbc:commit *con*))

(def-db-test map-query-same-as-exec-query ()
  (fill-rows 500)
  (let ((rows '()))
    (multiple-value-bind (count names)
        (qss-cl-odbc:map-query (lambda (r) (push r rows)) *con*
                              "SELECT id, n, txt FROM qss_odbc_t WHERE n = ? ORDER BY id" 3)
      (is (eql (length rows) count))
      (is (equal '("id" "n" "txt") (mapcar #'string-downcase names)))
      (is (equal (q "SELECT id, n, txt FROM qss_odbc_t WHERE n = ? ORDER BY id" 3)
                 (nreverse rows))))))

(def-db-test do-query-binds-columns ()
  (fill-rows 100)
  (let ((sum 0) (last nil))
    (is (eql 100 (qss-cl-odbc:do-query ((id n txt) *con* "SELECT id, n, txt FROM qss_odbc_t ORDER BY id")
                   (incf sum id)
                   (setf last (list id n txt)))))
    (is (eql 4950 sum))
    (is (equal '(99 1 "Zeile 99 ✓") last))))

(def-db-test do-query-early-return ()
  "RETURN verlaesst die Schleife; die Anweisung wird freigegeben, die Verbindung bleibt nutzbar."
  (fill-rows 1000)
  (is (eql 10 (qss-cl-odbc:do-query ((id) *con* "SELECT id FROM qss_odbc_t ORDER BY id")
                (when (= id 10) (return id)))))
  (is (eql 1000 (q1 "SELECT COUNT(*) FROM qss_odbc_t"))))

(def-db-test do-query-error-in-body ()
  (fill-rows 50)
  (signals error (qss-cl-odbc:do-query ((id) *con* "SELECT id FROM qss_odbc_t")
                   (when (= id 5) (error "Abbruch im Rumpf"))))
  (is (eql 50 (q1 "SELECT COUNT(*) FROM qss_odbc_t"))))

(def-db-test map-query-many-rows ()
  "20000 Zeilen zeilenweise, ohne sie zu sammeln."
  (fill-rows 20000)
  (let ((sum 0))
    (is (eql 20000 (qss-cl-odbc:map-query (lambda (r) (incf sum (first r))) *con*
                                         "SELECT n FROM qss_odbc_t")))
    ;; Access liefert SUM() ueber INTEGER als Double -> mit = vergleichen
    (is (= (q1 "SELECT SUM(n) FROM qss_odbc_t") sum))))

(def-db-test map-query-long-text ()
  (let ((s (make-string 50000 :initial-element (code-char #x2200))))
    (u "INSERT INTO qss_odbc_t (id, long_text) VALUES (1, ?)" (list s :uclob :in))
    (qss-cl-odbc:commit *con*)
    (qss-cl-odbc:do-query ((lt) *con* "SELECT long_text FROM qss_odbc_t WHERE id = ?" 1)
      (is (equal s lt)))))

(def-db-test map-query-requires-result-set ()
  (signals error (qss-cl-odbc:map-query #'identity *con* "DELETE FROM qss_odbc_t WHERE id = -1")))

(def-db-test direct-query-frees-parameters ()
  "Jede direkte Abfrage mit Parametern gibt ihre Parameter frei (vorher Speicherverlust)."
  (let ((*freed-parameters* 0))
    (dotimes (i 100) (q "SELECT COUNT(*) FROM qss_odbc_t WHERE id = ? AND txt = ?" i "x"))
    (is (eql 200 *freed-parameters*))
    (setf *freed-parameters* 0)
    (qss-cl-odbc:do-query ((c) *con* "SELECT COUNT(*) FROM qss_odbc_t WHERE id = ?" 1)
      (declare (ignorable c)))
    (is (eql 1 *freed-parameters*))
    (setf *freed-parameters* 0)
    (ignore-errors (q "SELECT * FROM qss_no_such_table WHERE id = ?" 1))
    (is (eql 1 *freed-parameters*) "auch nach einem Fehler")))

;;; ==================================================================
;;; Faelle aus weiteren ODBC-Testsammlungen (14.10.2026)
;;;
;;; Quellen (Treibertests, hier als Faelle fuer qss-cl-odbc uebernommen):
;;;   pyodbc      tests/old/accesstests.py   (Python, Access)
;;;   psqlodbc    test/src/*.c               (C, ODBC-API)
;;;   odbc-api    odbc-api/tests/integration (Rust)
;;;   mysql-connector-odbc test/*.c          (C)
;;; Jeder Test nennt seine Quelle. Die Grenzwerte (Fencepost) treffen ausserdem
;;; die Puffergrenzen von qss-cl-odbc selbst:
;;;   *max-precision* = 65536 Bytes  (Lesepuffer, 32768 UTF-16-Einheiten)
;;;   *lob-fetch-buffer-size* = 100000 (Sendeteile: 25000 Zeichen bzw. 100000 Bytes)

(def-suite :external-cases :in :integration :description "pyodbc, psqlodbc, odbc-api")
(in-suite :external-cases)

(defun ext-table-ddl ()
  (cond
    ((db-feature-p :postgresql)
     "CREATE TABLE qss_odbc_ext (id INTEGER PRIMARY KEY, txt VARCHAR(255), long_text TEXT, blb BYTEA, n INTEGER, dbl DOUBLE PRECISION, amount NUMERIC(19,4), dt TIMESTAMP)")
    ((db-feature-p :mysql)
     "CREATE TABLE qss_odbc_ext (id INTEGER PRIMARY KEY, txt VARCHAR(255), long_text LONGTEXT, blb LONGBLOB, n INTEGER, dbl DOUBLE, amount DECIMAL(19,4), dt DATETIME)")
    ((db-feature-p :sqlserver)
     "CREATE TABLE qss_odbc_ext (id INT PRIMARY KEY, txt NVARCHAR(255), long_text NVARCHAR(MAX), blb VARBINARY(MAX), n INT, dbl FLOAT, amount DECIMAL(19,4), dt DATETIME2(0))")
    (t (ext-table-ddl-access-or-sqlite))))

(defun ext-table-ddl-access-or-sqlite ()
  (if (db-feature-p :access)
      "CREATE TABLE [qss_odbc_ext] ([id] INTEGER CONSTRAINT [pk_qss_odbc_ext] PRIMARY KEY, [txt] VARCHAR(255), [long_text] LONGTEXT, [blb] LONGBINARY, [n] INTEGER, [dbl] DOUBLE, [amount] CURRENCY, [dt] DATETIME)"
      "CREATE TABLE qss_odbc_ext (id INTEGER PRIMARY KEY, txt VARCHAR(255), long_text TEXT, blb BLOB, n INTEGER, dbl DOUBLE, amount NUMERIC(19,4), dt TIMESTAMP)"))

(defmacro def-ext-test (name (&rest required-features) &body body)
  `(test ,name
     (if (features-ok-p ',required-features)
         (progn
           (ignore-errors (qss-cl-odbc:rollback *con*))
           (ignore-errors (ddl "DROP TABLE qss_odbc_ext"))
           (ddl (ext-table-ddl))
           (unwind-protect (progn ,@body)
             (ignore-errors (qss-cl-odbc:rollback *con*))
             (ignore-errors (ddl "DROP TABLE qss_odbc_ext"))))
         (skip "Datenbank ~A" (feature-mismatch ',required-features)))))

(defun ext-rt (column value param)
  (declare (ignore value))
  (u "DELETE FROM qss_odbc_ext")
  (u (format nil "INSERT INTO qss_odbc_ext (id, ~A) VALUES (1, ?)" column) param)
  (qss-cl-odbc:commit *con*)
  (q1 (format nil "SELECT ~A FROM qss_odbc_ext WHERE id = 1" column)))

(defmacro with-getdata ((on) &body body)
  `(let ((old (qss-cl-odbc:use-bind-column *con*)))
     (unwind-protect
          (progn (setf (qss-cl-odbc:use-bind-column *con*) (not ,on)) ,@body)
       (setf (qss-cl-odbc:use-bind-column *con*) old))))

;;; --- Grenzwerte (pyodbc: SMALL/LARGE_FENCEPOST_SIZES, _generate_test_string)

(defparameter +fence-seed+ "0123456789-abcdefghijklmnopqrstuvwxyz-")

(defun fence-string (n &optional (seed +fence-seed+))
  "Erkennbares Muster der Laenge N (pyodbc _generate_test_string)."
  (let ((s (make-string n)))
    (dotimes (i n s) (setf (char s i) (char seed (mod i (length seed)))))))

(defun fence-emoji-string (n)
  "Laenge N in UTF-16-Einheiten, mit Emoji (je 2 Einheiten) an wechselnden Stellen."
  (with-output-to-string (s)
    (let ((units 0) (i 0))
      (loop while (< units n)
            do (if (and (zerop (mod i 7)) (<= (+ units 2) n))
                   (progn (write-char (code-char #x1F600) s) (incf units 2))
                   (progn (write-char (char +fence-seed+ (mod i (length +fence-seed+))) s) (incf units)))
               (incf i)))))

(defparameter +small-fenceposts+ '(0 1 254 255))
(defparameter +large-fenceposts+
  (append '(256 270 304 508 510 511 512 1023 1024 2047 2048 4000 4095 4096 4097 10240 20480)
          ;; qss-cl-odbc: Sendeteile 25000 Zeichen, Lesepuffer 32768 UTF-16-Einheiten
          '(24999 25000 25001 32766 32767 32768 32769 50000 65535 65536 65537)))

(defun check-text-fence (column sizes param-type &key (maker #'fence-string))
  (dolist (n sizes)
    (let* ((s (funcall maker n))
           (back (ext-rt column s (if param-type (list s param-type :in) s))))
      (is (equal s back) "~A Laenge ~D: zurueck ~A Zeichen~@[, erste Abweichung bei ~D~]"
          column n (and (stringp back) (length back))
          (and (stringp back) (mismatch s back))))))

(def-ext-test fence-varchar ()
  "pyodbc test_varchar_N: Text 0, 1, 254, 255 Zeichen."
  (check-text-fence "txt" +small-fenceposts+ nil)
  (with-getdata (t) (check-text-fence "txt" +small-fenceposts+ nil)))

(def-ext-test fence-memo ()
  "pyodbc test_memo_N plus Puffergrenzen von qss-cl-odbc; gebunden und ueber SQLGetData."
  (check-text-fence "long_text" (append +small-fenceposts+ +large-fenceposts+) :uclob)
  (with-getdata (t) (check-text-fence "long_text" +large-fenceposts+ :uclob)))

(def-ext-test fence-memo-surrogates ()
  "Wie fence-memo, mit Emoji, damit Surrogatpaare auf Teil- und Puffergrenzen fallen."
  (check-text-fence "long_text" +large-fenceposts+ :uclob :maker #'fence-emoji-string)
  (with-getdata (t) (check-text-fence "long_text" +large-fenceposts+ :uclob :maker #'fence-emoji-string)))

(defun fence-bytes (n)
  (let ((v (make-array n :element-type '(unsigned-byte 8))))
    (dotimes (i n v) (setf (aref v i) (mod (* i 7) 251)))))

(def-ext-test fence-blob ()
  "pyodbc test_image_N plus Puffergrenzen (65536 Bytes, Sendeteil 100000)."
  (flet ((check (sizes)
           (dolist (n sizes)
             (let* ((b (fence-bytes n))
                    (back (ext-rt "blb" b (list b :blob :in))))
               (is (equalp b back) "blb Laenge ~D: zurueck ~A Bytes" n (and back (length back)))))))
    (let ((sizes (append '(1 254 255 256 4096 4097 20480 65535 65536 65537 99999 100000 100001 200000))))
      ;; gebunden nur, wenn der Treiber die Groesse richtig meldet
      (unless (db-feature-p :binary-size-misreported) (check sizes))
      (with-getdata (t) (check sizes)))))

;;; --- NULL in jeder Spaltenart (pyodbc test_null_memo/_binary/_varchar)

(def-ext-test null-in-every-type ()
  (u "INSERT INTO qss_odbc_ext (id, txt, long_text, blb, n, dbl, amount, dt) VALUES (1, ?, ?, ?, ?, ?, ?, ?)"
     nil (list nil :uclob :in) (list nil :blob :in) (list nil :integer :in)
     (list nil :double :in) (list nil :decimal :in) (list nil :date :in))
  (qss-cl-odbc:commit *con*)
  (is (equal '((nil nil nil nil nil nil nil))
             (q "SELECT txt, long_text, blb, n, dbl, amount, dt FROM qss_odbc_ext WHERE id = 1")))
  (with-getdata (t)
    (is (equal '((nil nil nil nil nil nil nil))
               (q "SELECT txt, long_text, blb, n, dbl, amount, dt FROM qss_odbc_ext WHERE id = 1")))))

;;; --- Zahlen (pyodbc test_negative_*, test_money, test_negative_decimal_scale)

(def-ext-test negative-numbers ()
  (dolist (v '(-1 -2147483648 -123456)) (is (eql v (ext-rt "n" v v))))
  (dolist (v '(-1.5d0 -1d300)) (is (eql v (ext-rt "dbl" v v)))))

(def-ext-test double-full-precision (:without :driver-rounds-doubles)
  "Double mit allen 17 Stellen (racket/db check-roundtrip 1.1, odbc-api fetch_double_precision)."
  (dolist (v '(0.30000000000000004d0 0.1d0 -2.2250738585072014d-308 1.7976931348623157d308
               4.9406564584124654d-324 123456789.12345678d0))
    (is (eql v (ext-rt "dbl" v v)) "~A" v)))

(def-ext-test money-and-negative-scale (:exact-numeric)
  "pyodbc test_money, test_negative_decimal_scale: -10.0010 exakt."
  (dolist (v (list -100010/10000 1234567/100 -1/10000 922337203685477 -922337203685477))
    (is (= v (ext-rt "amount" v v)) "amount ~A" v)))

;;; --- Parameter

(def-ext-test subquery-params ()
  "pyodbc test_subquery_params: Parameter in einer Unterabfrage."
  (u "INSERT INTO qss_odbc_ext (id, txt) VALUES (?, ?)" 1 "test")
  (qss-cl-odbc:commit *con*)
  (is (equal '((1)) (q "SELECT x.id FROM (SELECT id FROM qss_odbc_ext WHERE txt = ? AND id BETWEEN ? AND ?) AS x"
                       "test" 1 10))))

(def-ext-test params-with-quotes-and-backslashes ()
  "psqlodbc quotes-test: Anfuehrungszeichen und Backslash in Parametern."
  (dolist (s (list "param'quote" "param\\backslash" "ends with backslash\\" "doubled '' quotes"
                   "\"double\"" "'" "''" "%_[*?#" "-- kein Kommentar" "a;b"))
    (is (equal s (ext-rt "txt" s s)) "~S" s)))

(def-ext-test control-characters ()
  "psqlodbc lfconversion-test: LF, CR LF, Tab bleiben unveraendert."
  (dolist (s (list (format nil "a~%b") (coerce '(#\a #\Return #\Newline #\b) 'string)
                   (coerce '(#\a #\Tab #\b) 'string) (format nil "~%~%")))
    (is (equal s (ext-rt "txt" s s)) "~S" s)
    (is (equal s (ext-rt "long_text" s (list s :uclob :in))) "Memo ~S" s)))

(def-ext-test interior-nul (:without :driver-truncates-at-nul)
  "odbc-api interior_nul / insert_string_ending_with_nul: Nullzeichen im Text."
  (dolist (s (list (coerce (list #\a (code-char 0) #\b) 'string)
                   (coerce (list #\a #\b (code-char 0)) 'string)))
    (let ((back (ext-rt "txt" s s)))
      (is (equal s back) "~S zurueck als ~S" s back))))

(def-ext-test utf-16-low-byte-d8 (:without :driver-utf-16-d8-bug)
  "Zeichen, deren UTF-16-Einheit im niederwertigen Byte D8..DB hat (MariaDB
Connector/ODBC ODBC-502): kein Surrogat, ein ganz gewoehnliches Zeichen."
  (dolist (code '(#xD8 #xD9 #xDA #xDB #x1D8 #x2DB #x20DA #xFFD8))
    (let ((s (coerce (list #\a (code-char code) #\b) 'string)))
      (is (equal s (ext-rt "txt" s s)) "U+~4,'0X" code)
      (is (equal s (ext-rt "long_text" s (list s :uclob :in))) "Memo U+~4,'0X" code))))

(def-ext-test many-params ()
  "psqlodbc many-params-test: eine Anweisung mit 100 Parametern."
  (u "INSERT INTO qss_odbc_ext (id, n) VALUES (1, 7)")
  (qss-cl-odbc:commit *con*)
  (let* ((ids (loop for i from 1 to 100 collect i))
         (sql (format nil "SELECT COUNT(*) FROM qss_odbc_ext WHERE id IN (~{?~*~^, ~})" ids)))
    (is (= 1 (first (first (qss-cl-odbc:exec-query* *con* sql ids)))))))

(def-ext-test same-prepared-statement-repeated ()
  "pyodbc test_multiple_bindings: dieselbe Anweisung mit wechselnden Werten, auch NULL."
  (let ((ins (qss-cl-odbc:prepare-statement *con* "INSERT INTO qss_odbc_ext (id, txt, n) VALUES (?, ?, ?)"
                                           '(:integer :in) '(:unicode-string :in 255) '(:integer :in))))
    (unwind-protect
         (progn
           (qss-cl-odbc:exec-prepared-update ins 1 "eins" 1)
           (qss-cl-odbc:exec-prepared-update ins 2 nil nil)
           (qss-cl-odbc:exec-prepared-update ins 3 (fence-string 255) -3)
           (qss-cl-odbc:exec-prepared-update ins 4 "" 0))
      (qss-cl-odbc:free-statement ins)))
  (qss-cl-odbc:commit *con*)
  (is (equal (list (list 1 "eins" 1) (list 2 nil nil) (list 3 (fence-string 255) -3) (list 4 "" 0))
             (q "SELECT id, txt, n FROM qss_odbc_ext ORDER BY id"))))

(def-ext-test prepared-failure-midway ()
  "pyodbc test_executemany_failure: Fehler mitten in einer Reihe; danach geht es weiter."
  (let ((ins (qss-cl-odbc:prepare-statement *con* "INSERT INTO qss_odbc_ext (id, txt) VALUES (?, ?)"
                                           '(:integer :in) '(:unicode-string :in 50))))
    (unwind-protect
         (progn
           (qss-cl-odbc:exec-prepared-update ins 1 "a")
           (signals error (qss-cl-odbc:exec-prepared-update ins 1 "doppelt"))
           (qss-cl-odbc:exec-prepared-update ins 2 "b"))
      (qss-cl-odbc:free-statement ins)))
  (qss-cl-odbc:commit *con*)
  (is (equal '((1 "a") (2 "b")) (q "SELECT id, txt FROM qss_odbc_ext ORDER BY id"))))

(def-ext-test text-too-long-for-column (:without :varchar-length-not-enforced)
  "odbc-api insert_truncated_value / recover_from_truncation: 256 Zeichen in VARCHAR(255)
geben einen Fehler, die Verbindung bleibt nutzbar."
  (signals error (u "INSERT INTO qss_odbc_ext (id, txt) VALUES (1, ?)" (fence-string 256)))
  (ignore-errors (qss-cl-odbc:rollback *con*))
  (is (equal (fence-string 255) (ext-rt "txt" (fence-string 255) (fence-string 255)))))

;;; --- Zeilenzahlen (pyodbc test_rowcount_*, odbc-api row_count_*)

(def-ext-test row-counts ()
  (dotimes (i 4) (u "INSERT INTO qss_odbc_ext (id, n) VALUES (?, ?)" i i))
  (qss-cl-odbc:commit *con*)
  ;; n = n + 10 aendert jede getroffene Zeile: MySQL zaehlt ohne FOUND_ROWS
  ;; nur geaenderte, nicht gefundene Zeilen
  (is (eql 2 (u "UPDATE qss_odbc_ext SET n = n + 10 WHERE id < 2")))
  (is (eql 0 (u "UPDATE qss_odbc_ext SET n = 0 WHERE id > 100")) "nichts getroffen")
  (is (eql 4 (u "DELETE FROM qss_odbc_ext")))
  (is (eql 0 (u "DELETE FROM qss_odbc_ext")) "pyodbc test_rowcount_nodata")
  (qss-cl-odbc:commit *con*))

;;; --- Transaktionen (psqlodbc error-rollback-test, rollback-syntax-error-test)

(def-ext-test error-inside-transaction-aborts (:error-aborts-transaction)
  "PostgreSQL: nach einem Fehler ist die Transaktion verloren; COMMIT verwirft alles,
und bis dahin scheitert jede Anweisung (25P02)."
  (u "INSERT INTO qss_odbc_ext (id, n) VALUES (1, 1)")
  (signals error (u "INSERT INTO qss_odbc_ext (id, n) VALUES (1, 2)"))
  (signals error (q "SELECT COUNT(*) FROM qss_odbc_ext"))
  (qss-cl-odbc:commit *con*)
  (is (null (q "SELECT id, n FROM qss_odbc_ext")))
  (u "INSERT INTO qss_odbc_ext (id, n) VALUES (2, 2)")
  (qss-cl-odbc:commit *con*)
  (is (equal '((2 2)) (q "SELECT id, n FROM qss_odbc_ext"))))

(def-ext-test syntax-error-inside-transaction (:without :error-aborts-transaction :syntax-error-aborts-transaction)
  "psqlodbc rollback-syntax-error-test (Issue #203): ein Syntaxfehler darf fruehere
Aenderungen der Transaktion nicht verwerfen."
  (u "INSERT INTO qss_odbc_ext (id, n) VALUES (1, 1)")
  (signals error (q "SELEC kaputt"))
  (qss-cl-odbc:commit *con*)
  (is (equal '((1 1)) (q "SELECT id, n FROM qss_odbc_ext"))))

(def-ext-test syntax-error-aborts-transaction (:syntax-error-aborts-transaction)
  "Aeltere psqlODBC: ein Syntaxfehler verwirft die ganze Transaktion (Issue #203).
Dokumentiert das Verhalten; mit neuerem Treiber entfaellt der Test."
  (u "INSERT INTO qss_odbc_ext (id, n) VALUES (1, 1)")
  (signals error (q "SELEC kaputt"))
  (qss-cl-odbc:commit *con*)
  (is (null (q "SELECT id, n FROM qss_odbc_ext"))))

(def-ext-test error-inside-transaction (:without :error-aborts-transaction)
  "Ein Fehler in der Transaktion verwirft fruehere Aenderungen nicht von selbst;
COMMIT schreibt sie fest, ROLLBACK verwirft sie."
  (u "INSERT INTO qss_odbc_ext (id, n) VALUES (1, 1)")
  (signals error (u "INSERT INTO qss_odbc_ext (id, n) VALUES (1, 2)"))   ; doppelter Schluessel
  (qss-cl-odbc:commit *con*)
  (is (equal '((1 1)) (q "SELECT id, n FROM qss_odbc_ext")))
  (u "INSERT INTO qss_odbc_ext (id, n) VALUES (2, 2)")
  (signals error (u "INSERT INTO qss_odbc_ext (id, n) VALUES (2, 3)"))
  (qss-cl-odbc:rollback *con*)
  (is (equal '((1 1)) (q "SELECT id, n FROM qss_odbc_ext"))))

;;; --- ODBC-Escapes (psqlodbc odbc-escapes-test)

(def-ext-test odbc-escape-sequences (:without :no-odbc-fn-escapes)
  "{d}, {ts}, {fn ...} werden vom Treiber umgesetzt; qss-cl-odbc reicht sie durch."
  (u "INSERT INTO qss_odbc_ext (id, txt, dt) VALUES (1, ?, {ts '2014-12-21 13:45:10'})" "abc")
  (qss-cl-odbc:commit *con*)
  (is (equal '(2014 12 21 13 45 10)
             (multiple-value-list (qss-cl-odbc:decode-date (q1 "SELECT dt FROM qss_odbc_ext WHERE id = 1")))))
  (is (eql 1 (q1 "SELECT COUNT(*) FROM qss_odbc_ext WHERE dt >= {d '2014-12-21'}")))
  (is (equal "ABC" (q1 "SELECT {fn UCASE(txt)} FROM qss_odbc_ext WHERE id = 1")))
  (is (= 3 (q1 "SELECT {fn LENGTH(txt)} FROM qss_odbc_ext WHERE id = 1")))
  (is (equal "abcxy" (q1 "SELECT {fn CONCAT(txt, ?)} FROM qss_odbc_ext WHERE id = 1" "xy"))))

;;; --- Verbindung und Metadaten (odbc-api, pyodbc test_getinfo_*)

(test connection-metadata
  (let ((info (lambda (k) (qss-cl-odbc::get-odbc-info *con* k))))
    (is (stringp (funcall info qss-cl-odbc::$SQL_DBMS_NAME)))
    (is (stringp (funcall info qss-cl-odbc::$SQL_DBMS_VER)))
    (is (stringp (funcall info qss-cl-odbc::$SQL_DRIVER_NAME)))
    (is (stringp (funcall info qss-cl-odbc::$SQL_DRIVER_ODBC_VER)))
    (is (member (funcall info qss-cl-odbc::$SQL_IDENTIFIER_QUOTE_CHAR) '("`" "\"" "[" " ") :test #'equal)
        "Quote-Zeichen: ~S" (funcall info qss-cl-odbc::$SQL_IDENTIFIER_QUOTE_CHAR))
    (is (integerp (funcall info qss-cl-odbc::$SQL_TXN_CAPABLE)))))

(defun wait-until (seconds thunk)
  "Ruft THUNK alle 100 ms auf, bis es einen Wert ungleich NIL liefert (Fehler zaehlen
als NIL). Rueckgabe: Wert und verstrichene Sekunden, oder NIL nach SECONDS."
  (let ((start (get-internal-real-time)))
    (loop
      (let ((v (ignore-errors (funcall thunk)))
            (elapsed (/ (- (get-internal-real-time) start) internal-time-units-per-second)))
        (when v (return (values v (float elapsed))))
        (when (> elapsed seconds) (return nil))
        (sleep 0.1)))))

(test second-connection-independent
  "Zwei Verbindungen gleichzeitig: Was die eine festschreibt, sieht die andere.
Access puffert je Verbindung (PageTimeout); deshalb wird bis zu 10 s gewartet und
die Verzoegerung ausgegeben."
  (ignore-errors (ddl "DROP TABLE qss_odbc_ext"))
  (ddl (ext-table-ddl))
  (let ((c2 (connect-test-dsn)))
    (flet ((read-n ()
             ;; Leser schliesst seine Transaktion ab (sonst Sperre bzw. alter Stand)
             (unwind-protect (caar (qss-cl-odbc:exec-query c2 "SELECT n FROM qss_odbc_ext WHERE id = 1"))
               (qss-cl-odbc:rollback c2))))
    (unwind-protect
         (progn
           ;; 1. neue Zeile, festgeschrieben auf *con*
           (u "INSERT INTO qss_odbc_ext (id, n) VALUES (1, 42)")
           (qss-cl-odbc:commit *con*)
           (multiple-value-bind (v secs)
               (wait-until 10 #'read-n)
             (is (eql 42 v) "Zeile auf der zweiten Verbindung nach 10 s nicht sichtbar")
             (when v (format t "~&      sichtbar nach ~,1F s (neue Zeile)~%" secs)))
           ;; 2. Aenderung einer bestehenden Zeile
           (u "UPDATE qss_odbc_ext SET n = 43 WHERE id = 1")
           (qss-cl-odbc:commit *con*)
           (multiple-value-bind (v secs)
               (wait-until 10 (lambda () (let ((n (read-n))) (and (eql n 43) n))))
             (is (eql 43 v) "Aenderung auf der zweiten Verbindung nach 10 s nicht sichtbar")
             (when v (format t "~&      sichtbar nach ~,1F s (Aenderung)~%" secs)))
           ;; 3. nicht festgeschriebene Aenderung bleibt unsichtbar
           (u "UPDATE qss_odbc_ext SET n = 99 WHERE id = 1")
           (sleep 1)
           (is (eql 43 (ignore-errors (read-n)))
               "nicht festgeschriebene Aenderung war auf der zweiten Verbindung sichtbar")
           (qss-cl-odbc:rollback *con*))
      (ignore-errors (qss-cl-odbc:close-connection c2))
      (ignore-errors (ddl "DROP TABLE qss_odbc_ext"))))))

(test reader-with-autocommit
  "*CONNECT-AUTOCOMMIT* = T: ein reiner Leser haelt keine Transaktion offen."
  (ignore-errors (ddl "DROP TABLE qss_odbc_ext"))
  (ddl (ext-table-ddl))
  (u "INSERT INTO qss_odbc_ext (id, n) VALUES (1, 1)")
  (qss-cl-odbc:commit *con*)
  (let ((c2 (let ((qss-cl-odbc:*connect-autocommit* t))
              (connect-test-dsn))))
    (unwind-protect
         (progn
           (is (eql 1 (caar (qss-cl-odbc:exec-query c2 "SELECT n FROM qss_odbc_ext WHERE id = 1"))))
           ;; ohne COMMIT/ROLLBACK auf c2 muss der Schreiber durchkommen
           (finishes (u "UPDATE qss_odbc_ext SET n = 2 WHERE id = 1"))
           (qss-cl-odbc:commit *con*)
           (is (eql 2 (wait-until 10 (lambda ()
                                       (let ((n (caar (qss-cl-odbc:exec-query c2 "SELECT n FROM qss_odbc_ext WHERE id = 1"))))
                                         (and (eql n 2) n)))))))
      (ignore-errors (qss-cl-odbc:close-connection c2))
      (ignore-errors (ddl "DROP TABLE qss_odbc_ext")))))

;;; ==================================================================
;;; QSS 2026-10-14: Luecken nach Coverage-Messung (sb-cover)
;;; Funktionen, die vorher kein Test ausfuehrte: Ausgabeparameter, :clob,
;;; Dezimal/BIGINT ueber SQLGetData, mehrere Ergebnismengen, Fremdschluessel,
;;; Trace, CONNECT, CONNECT-SQLITE, PRINT-OBJECT, EXEC-PREPARED-COMMAND.

(def-suite :gap-cases :in :integration :description "Luecken nach Coverage-Messung")
(in-suite :gap-cases)

;;; --- :clob (ANSI, SQLPutData in Teilen)

(def-db-test clob-ansi-roundtrip (:without :ansi-non-ascii-unsupported)
  "Gefunden 14.10.2026: Laenge des :clob in Zeichen statt Bytes -- in UTF-8 wurde
\"Groesse EUR\" nach 7 Bytes abgeschnitten."
  (let* ((umlaut +cp1252-text+)
         ;; laenger als *LOB-FETCH-BUFFER-SIZE* (Sendeteile), Umlaute auf den Teilgrenzen
         (n qss-cl-odbc::*lob-fetch-buffer-size*)
         (long (let ((str (make-string (+ (* 2 n) 3) :initial-element #\a)))
                 (dolist (i (list 0 (1- n) n (1+ n) (* 2 n) (+ (* 2 n) 2)) str)
                   (setf (char str i) #\ü)))))
    (u "INSERT INTO qss_odbc_t (id, long_text) VALUES (?, ?)" 1 (list umlaut :clob))
    (u "INSERT INTO qss_odbc_t (id, long_text) VALUES (?, ?)" 2 (list long :clob))
    (u "INSERT INTO qss_odbc_t (id, long_text) VALUES (?, ?)" 3 (list "" :clob))
    (u "INSERT INTO qss_odbc_t (id, long_text) VALUES (?, ?)" 4 (list nil :clob))
    (is (equal umlaut (q1 "SELECT long_text FROM qss_odbc_t WHERE id = ?" 1)))
    (let ((back (q1 "SELECT long_text FROM qss_odbc_t WHERE id = ?" 2)))
      (is (eql (length long) (length back)))
      (is (equal long back)))
    (is (member (q1 "SELECT long_text FROM qss_odbc_t WHERE id = ?" 3) '("" nil) :test #'equal))
    (is (null (q1 "SELECT long_text FROM qss_odbc_t WHERE id = ?" 4)))))

;;; --- Lesen ueber SQLGetData (USE-BIND-COLUMN NIL)

(def-db-test getdata-exact-decimal (:exact-numeric)
  "Gefunden 14.10.2026: ueber SQLGetData kam NUMERIC/Waehrung als Double,
trotz *DECIMAL-AS-RATIONAL* = T."
  ;; Spalte mit 4 Nachkommastellen (CURRENCY bzw. NUMERIC(19,4))
  (loop for v in '(5474/5 -1001/10000 0 922337203685477 1/10000) for id from 10
        do (u "INSERT INTO qss_odbc_t (id, amount) VALUES (?, ?)" id v))
  (u "INSERT INTO qss_odbc_t (id, amount) VALUES (?, ?)" 20 nil)
  (let ((bound (q "SELECT amount FROM qss_odbc_t ORDER BY id")))
    (with-getdata (t)
      (let ((gd (q "SELECT amount FROM qss_odbc_t ORDER BY id")))
        (is (equal bound gd))
        (is (equal '(5474/5 -1001/10000 0 922337203685477 1/10000 nil) (mapcar #'car gd)))))))

(def-type-test getdata-bigint (:bigint)
  "BIGINT ueber SQLGetData (GD-BIGINT-COLUMN), auch jenseits von 2^53."
  (let ((vals (list 0 -1 (expt 2 53) (1+ (expt 2 53)) (1- (expt 2 63)) (- (expt 2 63)))))
    (loop for v in vals for id from 1
          do (u "INSERT INTO qss_odbc_types (id, big) VALUES (?, ?)" id v))
    (u "INSERT INTO qss_odbc_types (id, big) VALUES (?, ?)" 99 nil)
    (let ((bound (mapcar #'car (q "SELECT big FROM qss_odbc_types ORDER BY id"))))
      (is (equal (append vals '(nil)) bound))
      (with-getdata (t)
        (is (equal bound (mapcar #'car (q "SELECT big FROM qss_odbc_types ORDER BY id"))))))))

;;; --- Ausgabeparameter (PostgreSQL: Funktionen mit OUT/INOUT ueber {call})

(defmacro with-pg-function ((name definition) &body body)
  `(progn
     (ddl ,definition)
     (unwind-protect (progn ,@body)
       (ignore-errors (ddl ,(format nil "DROP FUNCTION IF EXISTS ~A" name))))))

(def-db-test output-parameters-every-type (:out-parameters)
  "GET-PARAMETER-VALUE fuer jeden Parametertyp."
  (with-pg-function ("qss_out_all"
                     "CREATE OR REPLACE FUNCTION qss_out_all(a integer,
         OUT o_int integer, OUT o_str varchar, OUT o_ustr varchar, OUT o_dbl float8,
         OUT o_dec numeric, OUT o_dt timestamp, OUT o_bin bytea)
       AS $$ SELECT a * 2, 'abc' || a::text, 'Ω€ü' || a::text, a / 4.0::float8,
                    a + 0.0125, timestamp '1776-07-04 12:30:15', decode('00ff7f', 'hex') $$
       LANGUAGE sql")
    (multiple-value-bind (o-int o-str o-ustr o-dbl o-dec o-dt o-bin)
        (qss-cl-odbc:exec-command *con* "{call qss_out_all(?, ?, ?, ?, ?, ?, ?, ?)}"
                                 5
                                 (list nil :integer :out)
                                 (list nil :string :out 50)
                                 (list nil :unicode-string :out 50)
                                 (list nil :double :out)
                                 (list nil :decimal :out 19 4)
                                 (list nil :date :out)
                                 (list nil :binary :out 10))
      (is (eql 10 o-int))
      (is (equal "abc5" o-str))
      (is (equal "Ω€ü5" o-ustr))
      (is (eql 1.25d0 o-dbl))
      (is (eql 401/80 o-dec))            ; 5 + 0.0125
      (is (eql (qss-cl-odbc:encode-date 1776 7 4 12 30 15) o-dt))
      (is (equalp #(0 255 127) o-bin)))))

(def-db-test output-parameters-inout-and-null (:out-parameters)
  "INOUT: Eingabewert geht hin, Ergebnis kommt zurueck; NULL als Ausgabe."
  (with-pg-function ("qss_inout"
                     "CREATE OR REPLACE FUNCTION qss_inout(INOUT a integer, INOUT t varchar, OUT n integer)
       AS $$ SELECT a * 3, t || '-ü', NULL::integer $$ LANGUAGE sql")
    (is (equal '(21 "ab-ü" nil)
               (multiple-value-list
                (qss-cl-odbc:exec-command *con* "{call qss_inout(?, ?, ?)}"
                                         (list 7 :integer :inout)
                                         (list "ab" :unicode-string :inout 50)
                                         (list nil :integer :out)))))))

(def-db-test exec-prepared-command-out (:out-parameters)
  "Vorbereitete Anweisung mit Ausgabeparametern, mehrfach ausgefuehrt."
  (with-pg-function ("qss_twice"
                     "CREATE OR REPLACE FUNCTION qss_twice(a integer, OUT b integer) AS $$ SELECT a * 2 $$ LANGUAGE sql")
    (qss-cl-odbc:with-prepared-statement (st *con* "{call qss_twice(?, ?)}" '(:integer :in) '(:integer :out))
      (is (equal '(2) (qss-cl-odbc:exec-prepared-command st 1)))
      (is (equal '(84) (qss-cl-odbc:exec-prepared-command st 42)))
      (is (equal '(nil) (qss-cl-odbc:exec-prepared-command st nil))))))

;;; --- mehrere Ergebnismengen

(def-db-test multiple-result-sets (:multiple-result-sets)
  "EXEC-SQL / EXEC-QUERY mit zwei SELECT in einem Aufruf (SQLMoreResults)."
  (u "INSERT INTO qss_odbc_t (id, txt) VALUES (1, 'a')")
  (u "INSERT INTO qss_odbc_t (id, txt) VALUES (2, 'b')")
  (multiple-value-bind (rows result-sets out)
      (qss-cl-odbc:exec-sql *con* "SELECT id FROM qss_odbc_t ORDER BY id; SELECT txt, id FROM qss_odbc_t WHERE id = 2")
    (declare (ignore rows))
    (is (null out))
    (is (eql 2 (length result-sets)))
    (is (equal '(((1) (2)) ("id")) (mapcar (lambda (x) (if (stringp (car x)) (mapcar #'string-downcase x) x))
                                           (first result-sets))))
    (is (equal '((("b" 2)) ("txt" "id")) (mapcar (lambda (x) (if (stringp (car x)) (mapcar #'string-downcase x) x))
                                                 (second result-sets)))))
  (is (equal '(((1)) ("one") ((2)) ("two"))
             (mapcar (lambda (x) (if (stringp (car x)) (mapcar #'string-downcase x) x))
                     (multiple-value-list (qss-cl-odbc:exec-query *con* "SELECT 1 AS one; SELECT 2 AS two"))))))

;;; --- Fremdschluessel (SQLForeignKeys)

(test foreign-keys
  "GET-FOREIGN-KEYS: Treiber mit SQLForeignKeys liefern die Beziehung; die
anderen einen sauberen Fehler, danach ist die Verbindung weiter nutzbar."
  (flet ((cleanup ()
           (ignore-errors (qss-cl-odbc:rollback *con*))
           (ignore-errors (ddl "DROP TABLE qss_odbc_fk"))
           (ignore-errors (ddl "DROP TABLE qss_odbc_pk"))))
    (cleanup)
    (ddl "CREATE TABLE qss_odbc_pk (id INTEGER PRIMARY KEY)")
    (ddl (if (db-feature-p :access)
             "CREATE TABLE qss_odbc_fk (id INTEGER PRIMARY KEY, p INTEGER CONSTRAINT fk_qss_odbc REFERENCES qss_odbc_pk (id))"
             "CREATE TABLE qss_odbc_fk (id INTEGER PRIMARY KEY, p INTEGER REFERENCES qss_odbc_pk (id))"))
    (unwind-protect
         (multiple-value-bind (rows names err)
             (handler-case (qss-cl-odbc:get-foreign-keys *con* nil nil "qss_odbc_pk" nil nil "qss_odbc_fk")
               (error (e) (values nil nil e)))
           (cond (err
                  (is (not (db-feature-p :foreign-keys)) "Fehler trotz :foreign-keys: ~A" err)
                  (format t "~&      SQLForeignKeys: ~A~%" err))
                 (t
                  (is (eql 1 (length rows)))
                  (let ((row (mapcar (lambda (x) (if (stringp x) (string-downcase x) x)) (first rows))))
                    ;; Spalten nach ODBC: PKTABLE_NAME 3, PKCOLUMN_NAME 4, FKTABLE_NAME 7, FKCOLUMN_NAME 8
                    (is (equal "qss_odbc_pk" (nth 2 row)))
                    (is (equal "id" (nth 3 row)))
                    (is (equal "qss_odbc_fk" (nth 6 row)))
                    (is (equal "p" (nth 7 row))))
                  (is (<= 8 (length names)))))
           ;; Verbindung weiter nutzbar
           (is (eql 0 (caar (qss-cl-odbc:exec-query *con* "SELECT COUNT(*) FROM qss_odbc_pk")))))
      (cleanup))))

;;; --- Trace des Treibermanagers

(test trace-connection
  "TRACE-CONNECTION schreibt eine Protokolldatei, UNTRACE-CONNECTION beendet sie."
  (let ((file (namestring (merge-pathnames (format nil "qss-odbc-trace-~D.log" (get-universal-time))
                                           (uiop:temporary-directory)))))
    (unwind-protect
         (progn
           (qss-cl-odbc:trace-connection *con* file)
           (qss-cl-odbc:exec-query *con* "SELECT 1 AS x")
           (qss-cl-odbc:untrace-connection *con*)
           (is (probe-file file) "keine Trace-Datei ~A" file)
           (let ((size (with-open-file (s file :if-does-not-exist nil) (and s (file-length s)))))
             (is (and size (plusp size))))
           ;; nach UNTRACE waechst die Datei nicht mehr
           (let ((before (with-open-file (s file) (file-length s))))
             (qss-cl-odbc:exec-query *con* "SELECT 1 AS x")
             (is (eql before (with-open-file (s file) (file-length s))))))
      (ignore-errors (qss-cl-odbc:untrace-connection *con*))
      (ignore-errors (qss-cl-odbc:rollback *con*))
      (ignore-errors (delete-file file)))))

;;; --- Verbinden

(test error-carries-diagnostics
  "SQL-ERROR traegt SQLSTATE, Meldung und alle Diagnosesaetze."
  (let ((e (handler-case (progn (q "SELECT * FROM qss_odbc_no_such_table_x") nil)
             (qss-cl-odbc::sql-error (c) c))))
    (is (typep e 'qss-cl-odbc::sql-error))
    (when e
      (is (stringp (qss-cl-odbc::sql-condition-state e)))
      (is (= 5 (length (qss-cl-odbc::sql-condition-state e))))
      (is (consp (qss-cl-odbc::sql-condition-diagnostics e)))
      (is (equal (qss-cl-odbc::sql-condition-state e)
                 (first (first (qss-cl-odbc::sql-condition-diagnostics e))))))
    (ignore-errors (qss-cl-odbc:rollback *con*))
    ;; Verbindung weiter nutzbar
    (is (eql 1 (q1 "SELECT 1")))))

(def-db-test long-error-message-complete (:postgresql)
  "Meldungen laenger als der erste Puffer (512 Zeichen) kommen vollstaendig an."
  (let* ((text (make-string 3000 :initial-element #\x))
         (e (handler-case
                (progn (qss-cl-odbc:exec-command
                        *con* (format nil "DO $$ BEGIN RAISE EXCEPTION '~A€'; END $$" text))
                       nil)
              (qss-cl-odbc::sql-error (c) c))))
    (is (typep e 'qss-cl-odbc::sql-error))
    (when e
      (is (search (concatenate 'string text "€") (qss-cl-odbc::sql-condition-message e))))))

(test long-column-alias
  "Spaltennamen laenger als der erste Puffer (256 Zeichen)."
  (let ((alias (format nil "a~A" (make-string 299 :initial-element #\b))))
    (multiple-value-bind (rows names)
        (handler-case (qss-cl-odbc:exec-query *con* (format nil "SELECT 1 AS ~A" alias))
          (qss-cl-odbc::sql-error (e) (values nil (list (princ-to-string e)))))
      (if rows
          ;; manche Server kuerzen still (PostgreSQL 63, MySQL 256 Zeichen):
          ;; vollstaendig gelesen ist, was der Server liefert, mindestens 63
          (let ((name (first names)))
            (is (and (>= (length name) 63) (<= (length name) (length alias))
                     (string= name alias :end2 (length name)))
                "Spaltenname ~D Zeichen: ~S" (length name) name))
          ;; Datenbank mit kuerzerer Namensgrenze (PostgreSQL: 63): sauberer Fehler
          (is (stringp (first names)))))))

(def-db-test umlaut-on-old-buffer-boundary (:postgresql)
  "psqlODBC teilt lange Meldungen in UTF-8-Bytes; ein Umlaut auf der Grenze
liess SQLGetDiagRecW nur \"Error: SqlState=...\" liefern (alter Puffer 512)."
  (dolist (n '(500 504 505 506 1010 1015))
    (let* ((text (concatenate 'string (make-string n :initial-element #\x) "äöü€-ende"))
           (e (handler-case
                  (progn (qss-cl-odbc:exec-command
                          *con* (format nil "DO $$ BEGIN RAISE EXCEPTION '~A'; END $$" text))
                         nil)
                (qss-cl-odbc:sql-error (c) c))))
      (is (and e (search text (qss-cl-odbc:sql-condition-message e)))
          "Meldung mit ~D x verloren: ~A" n (and e (qss-cl-odbc:sql-condition-message e))))))

(test get-info-value-types
  "SQLGetInfo: Text, 16-Bit- und 32-Bit-Werte."
  (let ((info (lambda (k) (qss-cl-odbc::get-odbc-info *con* k))))
    (is (stringp (funcall info qss-cl-odbc::$SQL_DRIVER_NAME)))
    (is (stringp (funcall info qss-cl-odbc::$SQL_DM_VER)))
    (let ((n (funcall info qss-cl-odbc::$SQL_MAX_COLUMN_NAME_LEN)))
      (is (typep n '(unsigned-byte 16))))
    (let ((n (funcall info qss-cl-odbc::$SQL_GETDATA_EXTENSIONS)))
      (is (typep n '(unsigned-byte 32))))
    (is (member (funcall info qss-cl-odbc::$SQL_TXN_CAPABLE)
                (list qss-cl-odbc::$SQL_TC_NONE qss-cl-odbc::$SQL_TC_DML qss-cl-odbc::$SQL_TC_ALL
                      qss-cl-odbc::$SQL_TC_DDL_COMMIT qss-cl-odbc::$SQL_TC_DDL_IGNORE)))))

(test print-connection
  (let ((text (prin1-to-string *con*)))
    (is (search "ODBC-CONNECTION" text))
    (is (search (qss-cl-odbc::dbms-name *con*) text))))

(def-db-test connect-dsn-user-password (:empty-login-ok)
  "CONNECT mit DSN, Benutzer und Kennwort (hier leer)."
  (let ((c (qss-cl-odbc:connect *dsn* "" "")))
    (unwind-protect
         (is (equal (qss-cl-odbc::dbms-name *con*) (qss-cl-odbc::dbms-name c)))
      (qss-cl-odbc:close-connection c))))

(def-db-test connect-sqlite-file (:sqlite)
  "CONNECT-SQLITE: Datei ueber den Standard-DSN, ANSI-Kodierung UTF-8."
  (let ((file (namestring (merge-pathnames (format nil "qss-odbc-~D.db" (get-universal-time))
                                           (uiop:temporary-directory)))))
    (unwind-protect
         (let ((qss-cl-odbc:*default-sqlite-dsn* *dsn*))
           (let ((c (qss-cl-odbc:connect-sqlite file)))
             (unwind-protect
                  (progn
                    (is (eq :utf-8 (qss-cl-odbc:connection-ansi-encoding c)))
                    (qss-cl-odbc:exec-command c "CREATE TABLE t (s TEXT)")
                    (qss-cl-odbc:exec-update c "INSERT INTO t VALUES (?)" (list +cp1252-text+ :string :in 100))
                    (qss-cl-odbc:commit c)
                    (is (equal +cp1252-text+ (caar (qss-cl-odbc:exec-query c "SELECT s FROM t")))))
               (qss-cl-odbc:close-connection c))
             (is (probe-file file) "Datei ~A nicht angelegt" file)))
      (ignore-errors (delete-file file)))))


(def-db-test integers-beyond-32-bit-from-expressions (:sqlite)
  "SQLite-ODBC meldet ganzzahlige Ausdruecke als SQL_INTEGER und kuerzte beim
Lesen auf 32 Bit (gefunden mit Chinook: sum(Bytes) = 117386255350 kam als
1422138358). INTEGER-AS-BIGINT liest sie als 64 Bit."
  (is (qss-cl-odbc:integer-as-bigint *con*))
  (is (equal '((3000000000 -3000000000 117386255350 1))
             (qss-cl-odbc:exec-query *con* "SELECT 3000000000, -3000000000, CAST(117386255350 AS BIGINT), 1")))
  (with-getdata (t)                     ; auch ueber SQLGetData
    (is (equal '((3000000000)) (qss-cl-odbc:exec-query *con* "SELECT 3000000000")))))
