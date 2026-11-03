;;;; tour.lisp -- a short tour of qss-cl-odbc, form by form
;;;;
;;;; Evaluate the forms one after the other in the REPL (in Emacs with SLY or
;;;; SLIME: C-x C-e after each form). The comments show what came back from
;;;; PostgreSQL 16 with psqlODBC (Unicode) on Linux, SBCL 2.2.9. Other
;;;; databases differ in details: SQLite has no exact decimal type, Access
;;;; writes [brackets] instead of "quotes".
;;;;
;;;; Needs a data source for an empty database; change "pgdemo" below. The
;;;; tour creates the table ARTICLE and drops it at the end.

(asdf:load-system :qss-cl-odbc)
(use-package :qss-cl-odbc)

;;; ------------------------------------------------------------------
;;; Connecting. Autocommit is off: every change needs COMMIT.

(defvar *db* (connect-generic :dsn "pgdemo"))
;; with user and password: (connect-generic :dsn "pgdemo" :uid "demo" :pwd "secret")
;; without a data source:  (connect-generic :driver "{PostgreSQL Unicode}"
;;                                          :server "localhost" :database "demo")

(exec-command *db* "CREATE TABLE article (id INTEGER PRIMARY KEY, name VARCHAR(80),
                                          price NUMERIC(10,2), added TIMESTAMP)")

;;; ------------------------------------------------------------------
;;; Parameters: plain Lisp values. Their SQL type follows from the value:
;;; integers, strings (always Unicode), rationals (exact decimals), and dates
;;; as a list with :date.

(exec-update *db* "INSERT INTO article VALUES (?, ?, ?, ?)"
             1 "Kaffee, 500 g" 899/100 (list (encode-date 2026 11 3 9 30 0) :date))
;; => 1                                         ; rows affected

(exec-update *db* "INSERT INTO article (id, name, price) VALUES (?, ?, ?), (?, ?, ?)"
             2 "Crème brûlée" 45/10
             3 "Tee 🍵 grün" 1/10)
;; => 2

(commit *db*)

;;; ------------------------------------------------------------------
;;; Queries return the rows and, as second value, the column names.

(exec-query *db* "SELECT id, name, price FROM article WHERE price < ? ORDER BY id" 10)
;; => ((1 "Kaffee, 500 g" 899/100) (2 "Crème brûlée" 9/2) (3 "Tee 🍵 grün" 1/10))
;;    ("id" "name" "price")

;;; ------------------------------------------------------------------
;;; Dates are universal times, without time zone shifts: what the database
;;; stores is what DECODE-DATE gives back.

(decode-date (caar (exec-query *db* "SELECT added FROM article WHERE id = 1")))
;; => 2026 11 3 9 30 0

;; Also before 1900 (Access stores times of day on 1899-12-30):
(exec-update *db* "UPDATE article SET added = ? WHERE id = 2"
             (list (encode-date 1899 12 30 8 15 0) :date))
(multiple-value-list
 (decode-date (caar (exec-query *db* "SELECT added FROM article WHERE id = 2"))))
;; => (1899 12 30 8 15 0)

;;; ------------------------------------------------------------------
;;; Row by row: DO-QUERY binds the columns of each row, without collecting
;;; the result. For large tables.

(do-query ((name price) *db* "SELECT name, price FROM article ORDER BY price DESC")
  (format t "~A: ~,2F~%" name price))
;; Kaffee, 500 g: 8.99
;; Crème brûlée: 4.50
;; Tee 🍵 grün: 0.10
;; => 3                                         ; rows
;;    ("name" "price")

;;; ------------------------------------------------------------------
;;; Prepared statements: parse once, execute many times.

(with-prepared-statement (st *db* "INSERT INTO article (id, name, price) VALUES (?, ?, ?)"
                             '(:integer :in) '(:unicode-string :in 80) '(:decimal :in 10 2))
  (loop for i from 10 below 1010
        sum (exec-prepared-update st i (format nil "Artikel ~D" i) (/ i 100))))
;; => 1000

;; Exact decimals: the sum as NUMERIC and as DOUBLE PRECISION.
(exec-query *db* "SELECT count(*), sum(price), sum(CAST(price AS DOUBLE PRECISION)) FROM article")
;; => ((1003 510859/100 5108.589999999999d0))
;;    ("count" "sum" "sum")

(rollback *db*)
(exec-query *db* "SELECT count(*) FROM article")
;; => ((3))                                     ; the 1000 rows are gone again

;;; ------------------------------------------------------------------
;;; Errors are conditions of type SQL-ERROR with SQLSTATE and the complete
;;; message of the driver.

(handler-case (exec-update *db* "INSERT INTO article (id, name) VALUES (?, ?)" 1 "twice")
  (sql-error (e)
    (list (sql-condition-state e) (sql-condition-message e))))
;; => ("23505" "ERROR: duplicate key value violates unique constraint \"article_pkey\"
;;    DETAIL: Key (id)=(1) already exists.;
;;    Error while executing the query")

(rollback *db*)                                 ; PostgreSQL: the transaction is aborted

;;; ------------------------------------------------------------------
;;; Catalog functions: results like EXEC-QUERY.

(get-primary-keys *db* nil "public" "article")
;; => (("demo" "public" "article" "id" 1 "article_pkey"))
;;    ("TABLE_CAT" "TABLE_SCHEM" "TABLE_NAME" "COLUMN_NAME" "KEY_SEQ" "PK_NAME")

(mapcar (lambda (row) (list (fourth row) (sixth row)))   ; COLUMN_NAME, TYPE_NAME
        (get-columns *db* nil "public" "article" "%"))
;; => (("id" "int4") ("name" "varchar") ("price" "numeric") ("added" "timestamp"))

;;; ------------------------------------------------------------------

(exec-command *db* "DROP TABLE article")
(commit *db*)
(close-connection *db*)
