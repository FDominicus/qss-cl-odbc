;;;; chinook-tests.lisp -- Tests gegen die Chinook-Beispieldatenbank, FiveAM
;;;;
;;;; Chinook (github.com/lerocha/chinook-database, MIT) ist ein Musikladen mit
;;;; Kuenstlern, Alben, Titeln, Kunden und Rechnungen. Die Datenbank liegt fuer
;;;; SQLite bei (src/test/chinook/), fuer andere Datenbanken gibt es Skripte im
;;;; Chinook-Repository. Diese Suite liest nur.
;;;;
;;;; Teil 1 ist die Testsuite des Chinook-Projekts (C#, ChinookSqliteFixture.cs):
;;;; Zeilenzahl und letzter Datensatz jeder Tabelle, elf Kunden mit Unicode,
;;;; jede Rechnung hat Positionen, Rechnungssumme = Summe der Positionen. Die
;;;; erwarteten Werte stehen in chinook/expected.sexp. Anders als in C# wird
;;;; typisiert verglichen, und die Summen werden bei Datenbanken mit exakten
;;;; Dezimalzahlen ohne ROUND verglichen.
;;;;
;;;; Teil 2 sind eigene Faelle: Parameter mit Unicode, zeilenweises Lesen,
;;;; Summen ueber 32 Bit, Join mit Gruppierung, Fremdschluessel, Datumsbereich.
;;;;
;;;; Aufruf:
;;;;   (qss-cl-odbc-test:run-chinook)                  ; mitgelieferte SQLite-Datei
;;;;   (qss-cl-odbc-test:run-chinook :dsn "CHINOOK")   ; eigene Chinook-Datenbank
;;;; Ohne :DSN gilt die Umgebungsvariable QSS_CHINOOK_DSN, sonst die SQLite-Datei
;;;; ueber den Treiber aus QSS_SQLITE_ODBC_DRIVER (Vorgabe "SQLite3", unixODBC;
;;;; unter Windows heisst Werners Treiber "SQLite3 ODBC Driver"). Ist der Treiber
;;;; nicht da, wird die Suite uebersprungen.

(in-package :qss-cl-odbc-test)

(def-suite :chinook :in :qss-cl-odbc :description "Chinook-Beispieldatenbank, nur lesend")
(in-suite :chinook)

(defvar *chinook-snake-case* nil
  "T, wenn das Schema snake_case benutzt (PostgreSQL-Skript: invoice_line.unit_price).")

(defun chinook-file (name)
  (asdf:system-relative-pathname :qss-cl-odbc-test (concatenate 'string "src/test/chinook/" name)))

(defun chinook-expected ()
  (with-open-file (in (chinook-file "expected.sexp") :external-format :utf-8)
    (let ((*read-eval* nil) (*package* (find-package :qss-cl-odbc-test)))
      (loop for form = (read in nil :eof) until (eq form :eof) collect form))))

;;; ------------------------------------------------------------------
;;; Namen: geschrieben wird CamelCase wie in SQLite, MySQL und SQL Server

(defun snake-case (name)
  "InvoiceLineId -> invoice_line_id"
  (with-output-to-string (out)
    (loop for c across name
          for i from 0
          do (when (and (upper-case-p c) (> i 0)) (write-char #\_ out))
             (write-char (char-downcase c) out))))

(defun cn (name)
  "Tabellen- oder Spaltenname im Schema der Verbindung."
  (if *chinook-snake-case* (snake-case name) name))

(defun chinook-sql (sql)
  "SQL in CamelCase-Namen fuer das Schema der Verbindung. Umgesetzt werden
Woerter aus Gross- und Kleinbuchstaben (InvoiceId); Schluesselwoerter in
Grossbuchstaben (SELECT) und Funktionen in Kleinbuchstaben (sum) bleiben.
Keine Literale mit solchen Woertern: Werte als Parameter uebergeben."
  (if (not *chinook-snake-case*)
      sql
      (with-output-to-string (out)
        (let ((i 0) (n (length sql)))
          (loop while (< i n)
                do (let ((c (char sql i)))
                     (if (alpha-char-p c)
                         (let* ((end (or (position-if-not #'alphanumericp sql :start i) n))
                                (word (subseq sql i end)))
                           (write-string (if (and (upper-case-p (char word 0))
                                                  (some #'lower-case-p word))
                                             (snake-case word)
                                             word)
                                         out)
                           (setf i end))
                         (progn (write-char c out) (incf i)))))))))

(defun cq (sql &rest params)
  (apply #'qss-cl-odbc:exec-query *con* (chinook-sql sql) params))

(defun detect-snake-case (con)
  (and (ignore-errors (qss-cl-odbc:exec-query con "SELECT count(*) FROM invoice_line") t)
       (progn (ignore-errors (qss-cl-odbc:rollback con)) t)))

;;; ------------------------------------------------------------------
;;; Vergleich typisierter Werte

(defun row-value (row columns name)
  (let ((pos (position (cn name) columns :test #'string-equal)))
    (unless pos (error "Spalte ~A nicht im Ergebnis ~S" name columns))
    (nth pos row)))

(defun same-value-p (expected actual)
  "EXPECTED aus expected.sexp, ACTUAL aus der Datenbank."
  (cond ((null expected) (null actual))
        ((and (consp expected) (eq (first expected) :date))
         (and (integerp actual)
              (equal (multiple-value-list (qss-cl-odbc:decode-date actual))
                     (append (rest expected) '(0 0 0)))))
        ;; Dezimalzahl: exakt als Bruch, oder (SQLite: NUMERIC als REAL
        ;; gespeichert) als das naechstgelegene Double
        ((typep expected 'ratio)
         (if (db-feature-p :exact-numeric)
             (eql expected actual)
             (and (floatp actual) (= (coerce expected 'double-float) actual))))
        ((integerp expected) (eql expected actual))
        ((stringp expected) (equal expected actual))
        (t (equal expected actual))))

(defun check-columns (what row columns pairs)
  (loop for (name value) in pairs
        for actual = (row-value row columns name)
        do (is (same-value-p value actual)
               "~A, Spalte ~A: erwartet ~S, gelesen ~S" what name value actual)))

;;; ------------------------------------------------------------------
;;; Teil 1: die Faelle der Chinook-Testsuite

(test chinook-table-counts
  "TableShouldBePopulated: alle Zeilen lesen und zaehlen."
  (dolist (e (chinook-expected))
    (when (eq (first e) :count)
      (destructuring-bind (table n) (rest e)
        (is (= n (length (cq (format nil "SELECT * FROM ~A" table))))
            "~A: ~D Zeilen erwartet" table n)))))

(test chinook-last-records
  "LastRecordHasProperInfo: letzte Zeile in Schluesselreihenfolge."
  (dolist (e (chinook-expected))
    (when (eq (first e) :last)
      (destructuring-bind (table order &rest pairs) (rest e)
        (multiple-value-bind (rows columns)
            (cq (format nil "SELECT * FROM ~A ORDER BY ~{~A~^, ~}" table order))
          (check-columns table (car (last rows)) columns pairs))))))

(test chinook-unicode-customers
  "CustomerIdNNHasProperUnicodeCharacters: Kunden 1 bis 11 (Luís Gonçalves,
Leonie Köhler, Bjørn Hansen, František Wichterlová ...)."
  (dolist (e (chinook-expected))
    (when (eq (first e) :row)
      (destructuring-bind (table (key value) &rest pairs) (rest e)
        (multiple-value-bind (rows columns)
            (cq (format nil "SELECT * FROM ~A WHERE ~A = ?" table key) value)
          (is (= 1 (length rows)) "~A ~A = ~A: genau eine Zeile" table key value)
          (when rows
            (check-columns (format nil "~A ~A" table value) (first rows) columns pairs)))))))

(test chinook-invoices-have-lines
  "AllInvoicesMustHaveInvoiceLines"
  (is (equal '((0)) (cq "SELECT count(InvoiceId) FROM Invoice
                         WHERE InvoiceId NOT IN (SELECT InvoiceId FROM InvoiceLine)"))))

(test chinook-invoice-totals
  "InvoiceTotalMustMatchSumOfInvoiceLines, in Lisp gerechnet. Mit exakten
Dezimalzahlen ohne Rundung (C#: ROUND(..., 2)); bei SQLite (Double) auf Cent."
  (let ((sums (make-hash-table)))
    (qss-cl-odbc:do-query ((invoice price quantity) *con*
                           (chinook-sql "SELECT InvoiceId, UnitPrice, Quantity FROM InvoiceLine"))
      (incf (gethash invoice sums 0) (* price quantity)))
    (let ((invoices (cq "SELECT InvoiceId, Total FROM Invoice")))
      (is (= 412 (length invoices)))
      (is (= 412 (hash-table-count sums)))
      (loop for (invoice total) in invoices
            for sum = (gethash invoice sums)
            do (if (db-feature-p :exact-numeric)
                   (is (and (rationalp total) (eql total sum))
                       "Rechnung ~D: Summe ~S, Positionen ~S" invoice total sum)
                   (is (= (round (* 100 total)) (round (* 100 sum)))
                       "Rechnung ~D: Summe ~S, Positionen ~S (auf Cent)" invoice total sum))))))

;;; ------------------------------------------------------------------
;;; Teil 2: eigene Faelle

(test chinook-revenue
  "Umsatz aller Rechnungen: SQL-Summe = Lisp-Summe; exakt 2328,60."
  (let ((sql-sum (caar (cq "SELECT sum(Total) FROM Invoice")))
        (lisp-sum (reduce #'+ (mapcar #'first (cq "SELECT Total FROM Invoice")))))
    (if (db-feature-p :exact-numeric)
        (progn (is (eql 11643/5 sql-sum))
               (is (eql 11643/5 lisp-sum)))
        (progn (is (= 232860 (round (* 100 sql-sum))))
               (is (= 232860 (round (* 100 lisp-sum))))))))

(test chinook-unicode-parameters
  "Text mit Nicht-ASCII-Zeichen als Parameter: Suche und LIKE."
  (is (equal '((2)) (cq "SELECT CustomerId FROM Customer WHERE LastName = ?" "Köhler")))
  (is (equal '((5)) (cq "SELECT CustomerId FROM Customer WHERE LastName = ?" "Wichterlová")))
  (is (equal '((1)) (cq "SELECT CustomerId FROM Customer WHERE City = ?" "São José dos Campos")))
  (is (equal '(("Chico Science & Nação Zumbi") ("Nação Zumbi"))
             (cq "SELECT Name FROM Artist WHERE Name LIKE ? ORDER BY ArtistId" "%Nação%"))))

(test chinook-streaming
  "DO-QUERY ueber alle Titel; Summen ueber 32 Bit (Bytes: 117 386 255 350)."
  (let ((n 0) (ms 0) (bytes 0))
    (is (= 3503 (qss-cl-odbc:do-query ((m b) *con* (chinook-sql "SELECT Milliseconds, Bytes FROM Track"))
                  (incf n) (incf ms m) (incf bytes b))))
    (is (= 3503 n))
    (is (= 1378778040 ms))
    (is (= 117386255350 bytes))
    (is (= bytes (caar (cq "SELECT sum(Bytes) FROM Track"))))))

(test chinook-join-group
  "Join ueber drei Tabellen mit Gruppierung: Kuenstler mit den meisten Titeln."
  (is (equal '(("Iron Maiden" 213) ("U2" 135) ("Led Zeppelin" 114))
             (subseq (cq "SELECT a.Name, count(*) FROM Track t
                            JOIN Album al ON t.AlbumId = al.AlbumId
                            JOIN Artist a ON a.ArtistId = al.ArtistId
                          GROUP BY a.ArtistId, a.Name ORDER BY count(*) DESC, a.Name")
                     0 3))))

(test chinook-date-range
  "Rechnungsdaten: 2021-01-01 bis 2025-12-22, als Universal Time ohne Zonenverschiebung.
Sortiert statt min/max: SQLite gibt Aggregate ueber DATETIME als Text zurueck."
  (let ((dates (mapcar #'first (cq "SELECT InvoiceDate FROM Invoice ORDER BY InvoiceDate"))))
    (is (every #'integerp dates))
    (is (eql (qss-cl-odbc:encode-date 2021 1 1 0 0 0) (first dates)))
    (is (eql (qss-cl-odbc:encode-date 2025 12 22 0 0 0) (car (last dates))))))

(test chinook-foreign-keys
  "Katalog: Fremdschluessel Track.AlbumId -> Album.AlbumId."
  (let ((schema (when *chinook-snake-case* "public")))
    (multiple-value-bind (rows columns)
        (qss-cl-odbc:get-foreign-keys *con* nil schema (cn "Album") nil schema (cn "Track"))
      (is (= 1 (length rows)) "genau ein Fremdschluessel, gelesen ~S" rows)
      (when rows
        (let ((row (first rows)))
          (flet ((col (name) (nth (position name columns :test #'string-equal) row)))
            (is (string-equal (cn "AlbumId") (col "PKCOLUMN_NAME")))
            (is (string-equal (cn "AlbumId") (col "FKCOLUMN_NAME")))
            (is (string-equal (cn "Track") (col "FKTABLE_NAME")))))))))

;;; ------------------------------------------------------------------
;;; Aufruf

(defun sqlite-odbc-driver ()
  (let ((d (uiop:getenv "QSS_SQLITE_ODBC_DRIVER")))
    (if (and d (plusp (length d))) d "SQLite3")))

(defun connect-chinook (dsn)
  "Verbindung zu Chinook: DSN, sonst eine Kopie der mitgelieferten SQLite-Datei.
Zweiter Wert: die Kopie (zum Loeschen) oder NIL."
  (if dsn
      (values (qss-cl-odbc:connect-generic :dsn dsn) nil)
      (let ((copy (merge-pathnames (format nil "qss-chinook-~D-~D.sqlite"
                                           (get-universal-time) (random 1000000))
                                   (uiop:temporary-directory))))
        (uiop:copy-file (chinook-file "Chinook_Sqlite.sqlite") copy)
        (handler-case
            (values (qss-cl-odbc:connect-generic :driver (sqlite-odbc-driver)
                                                 :database (uiop:native-namestring copy))
                    copy)
          (error (e)
            (ignore-errors (delete-file copy))
            (error e))))))

(defun run-chinook (&key (dsn (let ((d (uiop:getenv "QSS_CHINOOK_DSN")))
                                (and d (plusp (length d)) d))))
  (multiple-value-bind (con copy)
      (handler-case (connect-chinook dsn)
        (error (e)
          (format t "~&Chinook uebersprungen: keine Verbindung (~A).~%~
                       Ohne DSN braucht es den SQLite-ODBC-Treiber ~S (QSS_SQLITE_ODBC_DRIVER).~%"
                  e (sqlite-odbc-driver))
          (return-from run-chinook t)))
    (let ((*con* con))
      (unwind-protect
           (let ((*features-of-db* (detect-features con))
                 (*chinook-snake-case* (detect-snake-case con)))
             (format t "~&Chinook gegen ~A (~A), Namen ~A, Merkmale ~S~%"
                     (or dsn "die mitgelieferte SQLite-Datei")
                     (slot-value con 'qss-cl-odbc::dbms-name)
                     (if *chinook-snake-case* "snake_case" "CamelCase") *features-of-db*)
             (run! :chinook))
        (ignore-errors (qss-cl-odbc:rollback con))
        (ignore-errors (qss-cl-odbc:close-connection con))
        (when copy (ignore-errors (delete-file copy)))))))
