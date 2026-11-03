;;;; chinook-tour.lisp -- qss-cl-odbc on the Chinook sample database, form by form
;;;;
;;;; Chinook is a music store: artists, albums, tracks, customers, invoices
;;;; (github.com/lerocha/chinook-database, MIT license). A copy for SQLite comes
;;;; with qss-cl-odbc (src/test/chinook/), so this tour needs no server and no
;;;; data source, only unixODBC and the SQLite ODBC driver (Debian/Ubuntu:
;;;; apt install unixodbc libsqliteodbc; Windows: Christian Werner's sqliteodbc).
;;;;
;;;; Evaluate the forms one after the other (in Emacs with SLY or SLIME:
;;;; C-x C-e after each form). The comments show what came back on Linux with
;;;; SBCL 2.2.9, unixODBC 2.3.12 and sqliteodbc 0.99991.
;;;;
;;;; SQLite has no exact decimal type: prices come back as doubles here
;;;; (0.99d0). With PostgreSQL, MySQL/MariaDB or SQL Server they are rationals
;;;; (99/100); see tour.lisp.

(asdf:load-system :qss-cl-odbc)
(use-package :qss-cl-odbc)

;;; ------------------------------------------------------------------
;;; A private copy of the database, so that the original stays as it is

(defvar *file*
  (let ((copy (merge-pathnames "qss-chinook-tour.sqlite" (uiop:temporary-directory))))
    (uiop:copy-file (asdf:system-relative-pathname
                     :qss-cl-odbc "src/test/chinook/Chinook_Sqlite.sqlite")
                    copy)
    (uiop:native-namestring copy)))

;; The driver as unixODBC knows it (odbcinst -q -d). On Windows it is usually
;; "SQLite3 ODBC Driver".
(defvar *db* (connect-generic :driver "SQLite3" :database *file*))

;;; ------------------------------------------------------------------
;;; What is in there: the catalog

(mapcar #'third (get-tables *db* nil nil "%" "TABLE"))   ; TABLE_NAME
;; => ("Album" "Artist" "Customer" "Employee" "Genre" "Invoice" "InvoiceLine"
;;     "MediaType" "Playlist" "PlaylistTrack" "Track")

(mapcar (lambda (row) (list (third row) (fourth row) (seventh row) (eighth row)))
        (get-foreign-keys *db* nil nil nil nil nil "InvoiceLine"))
;; => (("Track" "TrackId" "InvoiceLine" "TrackId")
;;     ("Invoice" "InvoiceId" "InvoiceLine" "InvoiceId"))

;;; ------------------------------------------------------------------
;;; Queries with parameters. Text is Unicode throughout, also as parameter.

(exec-query *db* "SELECT FirstName, LastName, City FROM Customer
                  WHERE Country = ? ORDER BY LastName" "Germany")
;; => (("Leonie" "Köhler" "Stuttgart") ("Hannah" "Schneider" "Berlin")
;;     ("Niklas" "Schröder" "Berlin") ("Fynn" "Zimmermann" "Frankfurt"))
;;    ("FirstName" "LastName" "City")

(exec-query *db* "SELECT CustomerId, FirstName FROM Customer WHERE LastName = ?" "Wichterlová")
;; => ((5 "František"))
;;    ("CustomerId" "FirstName")

(exec-query *db* "SELECT Name FROM Track WHERE Name LIKE ? ORDER BY Name LIMIT 4" "%ção%")
;; => (("Canção Do Novo Mundo") ("Canção Noturna") ("Conciliação") ("Condição"))
;;    ("Name")

;;; ------------------------------------------------------------------
;;; A join over three tables: the artists with the most tracks

(exec-query *db* "SELECT a.Name, count(*) AS tracks
                    FROM Track t
                    JOIN Album al ON al.AlbumId = t.AlbumId
                    JOIN Artist a ON a.ArtistId = al.ArtistId
                  GROUP BY a.ArtistId, a.Name ORDER BY tracks DESC LIMIT 5")
;; => (("Iron Maiden" 213) ("U2" 135) ("Led Zeppelin" 114) ("Metallica" 112)
;;     ("Deep Purple" 92))
;;    ("Name" "tracks")

;;; ------------------------------------------------------------------
;;; Integers beyond 32 bit: all tracks together are 117 GB

(exec-query *db* "SELECT count(*), sum(Milliseconds), sum(Bytes) FROM Track")
;; => ((3503 1378778040 117386255350))

;;; ------------------------------------------------------------------
;;; Row by row: revenue per country, computed in Lisp. The cents are counted
;;; as integers because SQLite gives the totals as doubles; with an exact
;;; database you would simply add the totals.

(let ((cents (make-hash-table :test #'equal)))
  (do-query ((country total) *db* "SELECT BillingCountry, Total FROM Invoice")
    (incf (gethash country cents 0) (round (* 100 total))))
  (subseq (sort (loop for country being the hash-keys of cents using (hash-value c)
                      collect (list country (/ c 100)))
                #'> :key #'second)
          0 3))
;; => (("USA" 26153/50) ("Canada" 7599/25) ("France" 1951/10))   ; 523.06, 303.96, 195.10

;;; ------------------------------------------------------------------
;;; Dates are universal times; DECODE-DATE gives the wall clock time as stored

(exec-query *db* "SELECT InvoiceId, InvoiceDate, Total FROM Invoice
                  WHERE CustomerId = ? ORDER BY InvoiceDate DESC LIMIT 2" 2)
;; => ((293 3929817600 0.99d0) (241 3909686400 5.94d0))
;;    ("InvoiceId" "InvoiceDate" "Total")

(multiple-value-bind (year month day) (decode-date 3929817600)
  (format nil "~2,'0D.~2,'0D.~D" day month year))
;; => "13.07.2024"

;;; ------------------------------------------------------------------
;;; Changing data: a playlist with all tracks by Jobim, in one transaction.
;;; Autocommit is off, so nothing is visible to others before COMMIT.

(exec-update *db* "INSERT INTO Playlist (PlaylistId, Name) VALUES (?, ?)"
             19 "Favoriten – Bossa Nova")
;; => 1

(with-prepared-statement (st *db* "INSERT INTO PlaylistTrack (PlaylistId, TrackId) VALUES (?, ?)"
                             '(:integer :in) '(:integer :in))
  (loop for (track) in (exec-query *db* "SELECT TrackId FROM Track WHERE Composer LIKE ?" "%Jobim%")
        sum (exec-prepared-update st 19 track)))
;; => 4

(exec-query *db* "SELECT p.Name, count(*) FROM Playlist p
                    JOIN PlaylistTrack pt ON pt.PlaylistId = p.PlaylistId
                  WHERE p.PlaylistId = ? GROUP BY p.Name" 19)
;; => (("Favoriten – Bossa Nova" 4))

(rollback *db*)                                 ; or (commit *db*)
(exec-query *db* "SELECT count(*) FROM Playlist")
;; => ((18))

;;; ------------------------------------------------------------------

(close-connection *db*)
(delete-file *file*)
