;;; -*- Mode: lisp -*-

(in-package :qss-cl-odbc)

;; QSS 2026-10-13: Kodierung der ANSI-Schnittstelle (SQL_C_CHAR, Spaltennamen,
;; Meldungen). Vorher fest ISO-8859-1; der Access-Treiber liefert unter
;; Windows die ANSI-Codepage, in Deutschland Windows-1252 (mit EUR = #x80).
;; QSS 2026-10-14: Vorgabe nach Betriebssystem -- unter Windows die ANSI-
;; Codepage (Deutschland: Windows-1252), sonst UTF-8 (Locale der Treiber unter
;; Linux/macOS). Vorher fest :cp1252, unter Linux falsch fuer :string-Parameter.
(defvar *ansi-encoding* #+windows :cp1252 #-windows :utf-8
  "Vorgabe fuer neue Verbindungen. Jede Verbindung merkt sich ihre Kodierung
beim Verbinden (CONNECTION-ANSI-ENCODING, :ansi-encoding bei CONNECT-GENERIC);
fuer Abfragen gilt die der Verbindung, nicht der aktuelle Wert hier.")


(defmacro cffi-0.10-or-0.9 (form-0.10 form-0.9)
  (if (BOUNDP 'cffi::*foreign-string-mappings*)
      form-0.10
    form-0.9))

(cffi-0.10-or-0.9
 (defun get-string (ptr length) 
   (cffi:foreign-string-to-lisp ptr :count length :encoding *ansi-encoding*)
   )
 (defun get-string (ptr length)
   (cffi:foreign-string-to-lisp ptr length nil)
   )
 )
 
(cffi-0.10-or-0.9
 (defun get-string-nts (ptr)
   (cffi:foreign-string-to-lisp ptr :max-chars MOST-POSITIVE-FIXNUM :encoding *ansi-encoding*))
 (defun get-string-nts (ptr)
   (cffi:foreign-string-to-lisp ptr MOST-POSITIVE-FIXNUM t)))

(cffi-0.10-or-0.9
 (defun put-string (ptr vector)
   (cffi:lisp-string-to-foreign vector ptr
                              ;; QSS: Puffergroesse in Bytes, nicht in Zeichen
                              (1+ (babel:string-size-in-octets vector :encoding *ansi-encoding*))
                              :encoding *ansi-encoding*))
 (defun put-string (ptr vector)
   (cffi:lisp-string-to-foreign vector ptr (1+ (length vector)))))

(defun %null-ptr () 
  (cffi:null-pointer))


(cffi-0.10-or-0.9
 (defmacro with-foreign-string-alloc ((ptr text) &body body)
   `(cffi:with-foreign-string (,ptr ,text :encoding *ansi-encoding*)
                              ,@body))
 
 (defmacro with-foreign-string-alloc ((ptr text) &body body)
   `(cffi:with-foreign-string (,ptr ,text)
                              ,@body)))
                            

(defun get-byte-vector (ptr length)
  (let ((res (make-array length :element-type '(unsigned-byte 8))))
      (dotimes (i length)
        (setf (aref res i) (cffi:mem-aref ptr :uint8 i)))
    res))


(defun put-byte-vector (ptr vector)
  (dotimes (i (length vector))
      (setf (cffi:mem-aref ptr :uint8 i) (aref vector i))))

  
;; QSS 2026-10-13: UTF-16LE ueber babel statt eigener Umrechnung -- vorher nur
;; die BMP, Surrogatpaare (Zeichen jenseits U+FFFF) gingen kaputt.
(defun wchar-bytes-to-string (byte-vector)
  (babel:octets-to-string byte-vector :encoding :utf-16le))

(defun string-to-wchar-bytes (string)
  (babel:string-to-octets string :encoding :utf-16le))

(defun wide-octets (string)
  "Laenge von STRING in UTF-16LE, in Bytes."
  (babel:string-size-in-octets string :encoding :utf-16le))

(defun %put-unicode-string (ptr string)
  "Schreibt STRING als UTF-16LE mit abschliessender 0 (2 Bytes)."
  (let ((octets (string-to-wchar-bytes string)))
    (put-byte-vector ptr octets)
    (setf (cffi:mem-aref ptr :uint8 (length octets)) 0
          (cffi:mem-aref ptr :uint8 (1+ (length octets))) 0)
    (length octets)))

(defun %get-unicode-string (ptr len)
  "LEN Bytes UTF-16LE ab PTR als String."
  (wchar-bytes-to-string (get-byte-vector ptr len)))

(defun get-wide-string-nts (ptr)
  "Null-terminierter UTF-16LE-String ab PTR."
  (let ((n 0))
    (loop until (and (zerop (cffi:mem-aref ptr :uint8 n))
                     (zerop (cffi:mem-aref ptr :uint8 (1+ n))))
          do (incf n 2))
    (%get-unicode-string ptr n)))

(defun alloc-wide-string (string)
  "Neuer Fremdspeicher mit STRING als UTF-16LE, null-terminiert."
  (let ((ptr (cffi:foreign-alloc :uint8 :count (+ 2 (wide-octets string)))))
    (%put-unicode-string ptr string)
    ptr))

(defmacro with-wide-string ((ptr string) &body body)
  "PTR auf STRING als UTF-16LE; bei STRING = NIL der Nullzeiger."
  (let ((s (gensym)))
    `(let* ((,s ,string)
            (,ptr (if ,s (alloc-wide-string ,s) (cffi:null-pointer))))
       (unwind-protect (progn ,@body)
         (unless (cffi:null-pointer-p ,ptr) (cffi:foreign-free ,ptr))))))

(defmacro with-wide-strings (specs &body body)
  (if specs
      `(with-wide-string ,(car specs) (with-wide-strings ,(cdr specs) ,@body))
      `(progn ,@body)))

(defun alloc-wide-buffer (chars)
  "Puffer fuer CHARS UTF-16-Einheiten plus abschliessende 0, mit 0 vorbelegt."
  (let* ((n (* 2 (1+ chars)))
         (ptr (cffi:foreign-alloc :uint8 :count n)))
    (dotimes (i n ptr) (setf (cffi:mem-aref ptr :uint8 i) 0))))


(defun alloc-chars (size)
  (cffi:foreign-alloc :char :count (1+ size)))
   

;(defun make-unicode-string (length)
;  #+clisp
;  (make-string length)
;  #+allegro
;  (make-string length )
;  #+lispworks
;  (make-string length :initial-element (code-char 300)))