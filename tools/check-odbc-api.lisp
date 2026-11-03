;;; -*- Mode: Lisp -*-
;;; Cross-check of src/odbc/odbc-api-generated.lisp against a C compiler and
;;; the ODBC headers of this platform (unixODBC or iODBC under Linux/macOS,
;;; mingw-w64 under MSYS2, Windows SDK with MSVC). Independent of the
;;; Microsoft headers the generator reads.
;;;
;;;   (asdf:load-system :qss-cl-odbc)
;;;   (asdf:load-system :qss-cl-odbc-generate)
;;;   (qss-cl-odbc-generate:check)                    ; cc from PATH
;;;   (qss-cl-odbc-generate:check :cc "gcc" :cflags '("-I/usr/local/include"))
;;;
;;; Checks: value of every constant, size and signedness of every foreign
;;; type, size and field offsets of every structure, and every function
;;; prototype (redeclared with the header type names; a mismatch is a
;;; "conflicting types" error of the compiler). Without a compiler the check
;;; is skipped (returns :NO-COMPILER).
;;;
;;; Copyright (c) 2026 QSS / Friedrich Dominicus, BSD license (see license.txt)

(in-package :qss-cl-odbc-generate)

(export '(check))

(defparameter +c-prelude+
  "#if defined(_WIN32)
#include <windows.h>
#endif
#include <stdio.h>
#include <stddef.h>
#include <sql.h>
#include <sqlext.h>
#include <sqlucode.h>
")

(defun c-string-printer ()
  "static void ps(const char *s) { putchar('\"'); for (; *s; s++) { if (*s == '\"' || *s == '\\\\') putchar('\\\\'); putchar(*s); } putchar('\"'); }
")

(defun write-values-program (stream constants spec view)
  (write-string +c-prelude+ stream)
  (write-string (c-string-printer) stream)
  (format stream "int main(void) {~%")
  ;; types: size and signedness (an all-ones value is negative when signed)
  (loop for (c-name lisp-name) in (spec-types spec)
        do (format stream "  printf(\"(:type \\\"~A\\\" %d %d)\\n\", (int)sizeof(~A), (int)(((~A)-1) < 0));~%"
                   (string-downcase (symbol-name lisp-name)) c-name c-name))
  ;; structures: size and field offsets
  (loop for (c-name lisp-name) in (spec-structs spec)
        for fields = (gethash c-name (api-structs view))
        do (format stream "  printf(\"(:struct \\\"~A\\\" %d\", (int)sizeof(~A));~%"
                   (string-downcase (symbol-name lisp-name)) c-name)
           (loop for (fname) in fields
                 do (format stream "  printf(\" (~A %d)\", (int)offsetof(~A, ~A));~%"
                            (string-downcase fname) c-name fname))
           (format stream "  printf(\")\\n\");~%"))
  ;; constants
  (loop for (name . value) in constants
        do (format stream "#ifdef ~A~%" name)
           (if (stringp value)
               (format stream "  printf(\"(:const \\\"~A\\\" \"); ps(~A); printf(\")\\n\");~%" name name)
               (format stream "  printf(\"(:const \\\"~A\\\" %lld)\\n\", (long long)(~A));~%" name name))
           (format stream "#else~%  printf(\"(:const \\\"~A\\\" :missing)\\n\");~%#endif~%" name))
  (format stream "  return 0;~%}~%"))

(defun run (command)
  "Runs COMMAND (list); returns (values exit-code output)."
  (multiple-value-bind (out err code)
      (uiop:run-program command :output :string :error-output :string :ignore-error-status t)
    (values code (concatenate 'string out err))))

(defun find-compiler (cc)
  (or cc
      (find-if (lambda (c) (ignore-errors (zerop (run (list c "--version")))))
               '("cc" "gcc" "clang"))))

(defun library-symbol (name)
  (or (find-symbol (string-upcase name) :qss-cl-odbc)
      (error "qss-cl-odbc has no symbol ~A (load qss-cl-odbc first)" name)))

(defun lisp-signed-p (ctype)
  "Signedness of a CFFI integer type: write all-ones bytes, read back.
Floating point types count as signed (as (double)-1 < 0 in C)."
  (let ((size (cffi:foreign-type-size ctype)))
    (cffi:with-foreign-object (p :uint8 size)
      (cffi:with-foreign-object (q :double)
        ;; a float type reads back 1.0 as a float, an integer type does not
        (setf (cffi:mem-ref q :double) 0d0)
        (when (handler-case (floatp (cffi:mem-ref q ctype)) (error () nil))
          (return-from lisp-signed-p t)))
      (dotimes (i size) (setf (cffi:mem-aref p :uint8 i) 255))
      (minusp (cffi:mem-ref p ctype)))))

(defun check (&key cc cflags (spec (read-spec)) (header-dir (system-file "odbc-headers/"))
                (directory (uiop:ensure-directory-pathname
                            (merge-pathnames "qss-cl-odbc-check/" (uiop:temporary-directory))))
                (verbose t))
  "Compiles and runs the cross-check. Returns a plist with :mismatches (list of
strings), :missing (constants the platform headers lack), :checked (count)."
  (unless (find-package :qss-cl-odbc)
    (error "load qss-cl-odbc first"))
  (let ((cc (find-compiler cc)))
    (unless cc
      (when verbose (format t "~&qss-cl-odbc check: no C compiler found, skipped.~%"))
      (return-from check :no-compiler))
    (ensure-directories-exist directory)
    (let* ((64-bit (= 8 (cffi:foreign-type-size :pointer)))
           (view (read-headers spec :header-dir header-dir :pointer-width-64 64-bit))
           (constants (spec-constants view spec))
           (c-file (merge-pathnames "values.c" directory))
           (exe (merge-pathnames (if (uiop:os-windows-p) "values.exe" "values") directory))
           (mismatches '()) (missing '()) (checked 0))
      (flet ((note-mismatch (fmt &rest args)
               (push (apply #'format nil fmt args) mismatches)))
        ;; 1. values
        (with-open-file (s c-file :direction :output :if-exists :supersede)
          (write-values-program s constants spec view))
        (multiple-value-bind (code output)
            (run (append (list cc) cflags (list "-o" (namestring exe) (namestring c-file))))
          (unless (zerop code)
            (error "compiling ~A failed:~%~A" c-file output)))
        (multiple-value-bind (code output) (run (list (namestring exe)))
          (unless (zerop code) (error "running ~A failed:~%~A" exe output))
          (with-input-from-string (in output)
            (let ((*read-eval* nil) (*package* (find-package :keyword)))
              (loop for form = (read in nil)
                    while form
                    do (incf checked)
                       (ecase (first form)
                         (:type
                          (destructuring-bind (name size signed) (rest form)
                            (let ((ctype (library-symbol name)))
                              (unless (= size (cffi:foreign-type-size ctype))
                                (note-mismatch "type ~A: C size ~D, Lisp ~D" name size (cffi:foreign-type-size ctype)))
                              (unless (eq (= signed 1) (lisp-signed-p ctype))
                                (note-mismatch "type ~A: C ~:[unsigned~;signed~], Lisp not" name (= signed 1))))))
                         (:struct
                          (destructuring-bind (name size &rest offsets) (rest form)
                            (let ((stype (list :struct (library-symbol name))))
                              (unless (= size (cffi:foreign-type-size stype))
                                (note-mismatch "struct ~A: C size ~D, Lisp ~D" name size (cffi:foreign-type-size stype)))
                              (loop for (field offset) in offsets
                                    for lisp-offset = (cffi:foreign-slot-offset
                                                       stype (library-symbol (lispify (string field))))
                                    unless (= offset lisp-offset)
                                      do (note-mismatch "struct ~A field ~A: C offset ~D, Lisp ~D"
                                                   name field offset lisp-offset)))))
                         (:const
                          (destructuring-bind (name value) (rest form)
                            (let* ((sym (library-symbol (concatenate 'string "$" name)))
                                   (lisp (symbol-value sym)))
                              (cond ((eq value :missing) (push name missing))
                                    ((not (equal value lisp))
                                     (note-mismatch "constant ~A: C ~S, Lisp ~S" name value lisp)))))))))))
        ;; 2. prototypes, one translation unit each
        (dolist (fn (spec-functions spec))
          (let ((proto (gethash fn (api-prototypes view)))
                (file (merge-pathnames (format nil "proto-~A.c" fn) directory)))
            (with-open-file (s file :direction :output :if-exists :supersede)
              (write-string +c-prelude+ s)
              (format s "~A~%" (prototype-c-text proto)))
            (incf checked)
            (multiple-value-bind (code output)
                (run (append (list cc) cflags (list "-fsyntax-only" (namestring file))))
              (unless (zerop code)
                (note-mismatch "prototype ~A: ~A" fn
                          (string-trim '(#\Newline #\Space) output))))))
        (setf mismatches (nreverse mismatches) missing (nreverse missing))
        (when verbose
          (format t "~&qss-cl-odbc check with ~A: ~D items, ~D mismatches, ~D constants not in the platform headers.~%"
                  cc checked (length mismatches) (length missing))
          (dolist (m mismatches) (format t "  MISMATCH ~A~%" m)))
        (list :mismatches mismatches :missing missing :checked checked :compiler cc)))))
