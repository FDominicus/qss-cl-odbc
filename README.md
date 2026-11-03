# qss-cl-odbc

ODBC for Common Lisp: ODBC 3.5, Unicode, 64 bit; Windows, Linux, macOS.

```lisp
(ql:quickload :qss-cl-odbc)
(defvar *con* (qss-cl-odbc:connect-generic :dsn "MYDSN"))
(qss-cl-odbc:exec-query *con* "SELECT id, name FROM person WHERE id > ?" 10)
```

qss-cl-odbc is a thin layer over the ODBC API. It runs SQL with parameters and
returns the results as Lisp lists. It does not generate SQL and has no object
layer.

## Try it without a server

The [Chinook](https://github.com/lerocha/chinook-database) sample database (a
music store with artists, albums, tracks, customers and invoices; MIT license)
comes along as an SQLite file. With unixODBC and the SQLite ODBC driver
(Debian/Ubuntu: `apt install unixodbc libsqliteodbc`) you need no server and no
data source:

```lisp
(defvar *db* (qss-cl-odbc:connect-generic
              :driver "SQLite3"
              :database (uiop:native-namestring
                         (asdf:system-relative-pathname
                          :qss-cl-odbc "src/test/chinook/Chinook_Sqlite.sqlite"))))

(qss-cl-odbc:exec-query *db* "SELECT a.Name, count(*) AS tracks
                                FROM Track t
                                JOIN Album al ON al.AlbumId = t.AlbumId
                                JOIN Artist a ON a.ArtistId = al.ArtistId
                              GROUP BY a.ArtistId, a.Name ORDER BY tracks DESC LIMIT 3")
;; => (("Iron Maiden" 213) ("U2" 135) ("Led Zeppelin" 114))
;;    ("Name" "tracks")
```

[`doc/examples/chinook-tour.lisp`](doc/examples/chinook-tour.lisp) goes on from
there, form by form, on a copy of the file: the catalog, Unicode parameters,
row-by-row reading, dates, and a transaction with a prepared statement.

## Examples

These examples need a database with exact decimals; they were run against
PostgreSQL 16 with psqlODBC. The full tour,
which you can evaluate form by form, is in
[`doc/examples/tour.lisp`](doc/examples/tour.lisp).

```lisp
(use-package :qss-cl-odbc)
(defvar *db* (connect-generic :dsn "pgdemo"))   ; autocommit is off

(exec-command *db* "CREATE TABLE article (id INTEGER PRIMARY KEY, name VARCHAR(80),
                                          price NUMERIC(10,2), added TIMESTAMP)")
```

**Parameters are plain Lisp values.** Strings are always passed as Unicode,
rationals as exact decimals, and dates as a list with `:date`:

```lisp
(exec-update *db* "INSERT INTO article VALUES (?, ?, ?, ?)"
             1 "Kaffee, 500 g" 899/100 (list (encode-date 2026 11 3 9 30 0) :date))
;; => 1
(exec-update *db* "INSERT INTO article (id, name, price) VALUES (?, ?, ?), (?, ?, ?)"
             2 "Crème brûlée" 45/10
             3 "Tee 🍵 grün" 1/10)
;; => 2
(commit *db*)
```

**Queries return the rows, and the column names as a second value:**

```lisp
(exec-query *db* "SELECT id, name, price FROM article WHERE price < ? ORDER BY id" 10)
;; => ((1 "Kaffee, 500 g" 899/100) (2 "Crème brûlée" 9/2) (3 "Tee 🍵 grün" 1/10))
;;    ("id" "name" "price")
```

**Dates come back exactly as stored,** without time zone shifts, and dates
before 1900 work too:

```lisp
(decode-date (caar (exec-query *db* "SELECT added FROM article WHERE id = 1")))
;; => 2026 11 3 9 30 0
(exec-update *db* "UPDATE article SET added = ? WHERE id = 2"
             (list (encode-date 1899 12 30 8 15 0) :date))   ; an Access time of day
```

**Large results can be read row by row:**

```lisp
(do-query ((name price) *db* "SELECT name, price FROM article ORDER BY price DESC")
  (format t "~A: ~,2F~%" name price))
;; Kaffee, 500 g: 8.99
;; Crème brûlée: 4.50
;; Tee 🍵 grün: 0.10
```

**Prepared statements are parsed once and executed many times:**

```lisp
(with-prepared-statement (st *db* "INSERT INTO article (id, name, price) VALUES (?, ?, ?)"
                             '(:integer :in) '(:unicode-string :in 80) '(:decimal :in 10 2))
  (loop for i from 10 below 1010
        sum (exec-prepared-update st i (format nil "Artikel ~D" i) (/ i 100))))
;; => 1000
```

**Decimals are exact.** Here is the sum of the now 1003 prices, once as NUMERIC
and once as DOUBLE PRECISION:

```lisp
(exec-query *db* "SELECT sum(price), sum(CAST(price AS DOUBLE PRECISION)) FROM article")
;; => ((510859/100 5108.589999999999d0))
(rollback *db*)                                 ; the 1000 rows are gone again
```

**Errors are conditions** that carry the SQLSTATE and the driver's complete
message:

```lisp
(handler-case (exec-update *db* "INSERT INTO article (id, name) VALUES (?, ?)" 1 "twice")
  (sql-error (e) (list (sql-condition-state e) (sql-condition-message e))))
;; => ("23505" "ERROR: duplicate key value violates unique constraint \"article_pkey\"
;;    DETAIL: Key (id)=(1) already exists.;
;;    Error while executing the query")
```

**Catalog functions** return their results the same way:

```lisp
(get-primary-keys *db* nil "public" "article")
;; => (("demo" "public" "article" "id" 1 "article_pkey"))
;;    ("TABLE_CAT" "TABLE_SCHEM" "TABLE_NAME" "COLUMN_NAME" "KEY_SEQ" "PK_NAME")
```

## Derived from plain-odbc

qss-cl-odbc started as Roland Averkamp's
[plain-odbc](https://common-lisp.net/project/plain-odbc/) (2005) and keeps its
interface. Most of the code below that interface has been rewritten:

- **ODBC 3.5 instead of 2.10**: handle API, diagnostics with `SQLGetDiagRecW`
  (all records, ODBC 3 SQLSTATEs), all text through the Unicode (…W) functions.
- **64 bit**: `SQLLEN`/`SQLULEN` are pointer-sized. With 32-bit lengths,
  indicators and data-at-execution values were corrupted on 64-bit Windows.
- **Generated lowest layer**: constants, types, structures and function
  declarations are generated from Microsoft's ODBC headers (`odbc-headers/`)
  and cross-checked against a C compiler. Paul Meurer's earlier layer is gone.
- **Exact decimals**: NUMERIC/DECIMAL/CURRENCY come as rationals
  (`1094.80` → `5474/5`), not as doubles.
- **Dates before 1900** work (Access stores times of day on 1899-12-30), and
  there are no time zone shifts.
- **Text encoding per connection**: Unicode by default; the ANSI encoding for
  `:string` parameters follows the driver (Access: Windows-1252, SQLite:
  UTF-8).
- **Streaming**: `do-query` and `map-query` go through large results row by
  row.
- **Test suite**: more than 500 FiveAM checks, with cases taken from the test
  suites of racket/db, pyodbc, psqlodbc and odbc-api. Driver peculiarities are
  recorded as features, and tests that do not apply are skipped with a reason.

### Coming from plain-odbc

Load `:qss-cl-odbc` instead of `:plain-odbc` and write `qss-cl-odbc:` where you
wrote `plain-odbc:`. There is deliberately no `PLAIN-ODBC` nickname, so both
can be loaded into the same Lisp. Two things behave differently: decimals come
as rationals (set `qss-cl-odbc:*decimal-as-rational*` to NIL for doubles), and
errors carry ODBC 3 SQLSTATEs (`HY000`, `42S02`) instead of ODBC 2 ones
(`S1000`).

## Tested with

| Platform                     | Drivers                                                         |
|------------------------------|-----------------------------------------------------------------|
| Windows 64 bit, SBCL 2.6.9   | Microsoft Access (ACE), SQLite ODBC, PostgreSQL (psqlODBC)      |
| Linux 64 bit, SBCL 2.2.9     | SQLite ODBC, PostgreSQL (psqlODBC), MariaDB (Connector/ODBC 3.1) |

Not tested yet: SQL Server, macOS. The individual runs are listed in
`qss-patches.txt`, and the peculiarities of each driver are described in
`doc/notes.html`.

## Requirements

- [CFFI](https://cffi.common-lisp.dev/) and babel (both in Quicklisp)
- an ODBC driver manager. On Windows it is built in (`odbc32.dll`). On Linux
  and BSD use unixODBC (`libodbc.so.2`; the development package provides
  `libodbc.so`). On macOS use unixODBC or iODBC.

Loading needs neither a C compiler nor the ODBC headers.

## Installation

qss-cl-odbc is not in Quicklisp yet. Clone it into
`~/quicklisp/local-projects/` (or anywhere ASDF looks), then:

```lisp
(ql:register-local-projects)
(ql:quickload :qss-cl-odbc)
```

## Systems

| System                 | Purpose                                                                                                   |
|------------------------|-----------------------------------------------------------------------------------------------------------|
| `qss-cl-odbc`          | the library                                                                                               |
| `qss-cl-odbc-test`     | tests: `(qss-cl-odbc-test:run-all :dsn "MYDSN")`, or `(asdf:test-system :qss-cl-odbc)` with `QSS_ODBC_DSN` set |
| `qss-cl-odbc-generate` | regenerates `src/odbc/odbc-api-generated.lisp` from `odbc-headers/` and cross-checks it with a C compiler; not needed to use the library |

The integration tests create and drop tables named `qss_odbc_…`, so run them
against an empty database. Without a DSN only the unit tests run.

`run-all` also runs the suite `:chinook`, which only reads: the cases of the
Chinook project's own test suite plus some more, by default on the SQLite copy
in `src/test/chinook/`. For another database load the Chinook script for it
and call `(qss-cl-odbc-test:run-chinook :dsn "CHINOOK")` (or set
`QSS_CHINOOK_DSN`).

## Documentation

- `doc/documentation.html`: the manual
- `doc/notes.html`: driver notes
- `doc/examples/tour.lisp`: the tour from the examples above, form by form
- `doc/examples/chinook-tour.lisp`: a tour on the Chinook database, no server needed
- `doc/examples/`: also the same program in C, in Lisp line by line on the
  raw API, and with qss-cl-odbc
- `qss-patches.txt`: changes against plain-odbc and test runs (in German)

## License

BSD. The parts by Roland Averkamp and by QSS / Friedrich Dominicus each carry
their own copyright notice. The Microsoft ODBC headers in `odbc-headers/` are
under the MIT license, and so is the Chinook database in `src/test/chinook/`.
See `license.txt`.
