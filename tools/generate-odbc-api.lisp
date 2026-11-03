;;; -*- Mode: Lisp -*-
;;; Generator for src/odbc/odbc-api-generated.lisp
;;;
;;; Reads Microsoft's ODBC headers (odbc-headers/, MIT license) and writes the
;;; constants, foreign types, structures and function declarations qss-cl-odbc
;;; uses, as listed in tools/odbc-api-spec.lisp. Needs no C compiler: it has a
;;; small C preprocessor (#if/#ifdef/#define, C integer expressions) and reads
;;; typedefs, structures and prototypes itself. tools/check-odbc-api.lisp
;;; checks the result against a real compiler and another set of headers.
;;;
;;;   (asdf:load-system :qss-cl-odbc-generate)
;;;   (qss-cl-odbc-generate:generate)            ; writes the file
;;;   (qss-cl-odbc-generate:generated-text)      ; only returns the text
;;;
;;; Copyright (c) 2026 QSS / Friedrich Dominicus, BSD license (see license.txt)

(defpackage :qss-cl-odbc-generate
  (:use :common-lisp)
  (:export #:generate #:generated-text #:read-spec #:read-headers
           #:system-file #:lisp-function-name
           #:api-constants #:api-prototypes #:api-structs #:spec-functions
           #:spec-types #:spec-structs #:prototype-c-text #:spec-constants))

(in-package :qss-cl-odbc-generate)

(defun system-file (relative)
  (asdf:system-relative-pathname :qss-cl-odbc-generate relative))

;;; ------------------------------------------------------------------
;;; Spec

(defun read-spec (&optional (file (system-file "tools/odbc-api-spec.lisp")))
  (with-open-file (s file :external-format :utf-8)
    (let ((*read-eval* nil)
          (*package* (find-package :qss-cl-odbc-generate)))
      (read s))))

(defun spec-functions (spec) (getf spec :functions))
(defun spec-types (spec) (getf spec :types))
(defun spec-structs (spec) (getf spec :structs))

;;; ------------------------------------------------------------------
;;; Source text: lines, continuations, comments

(defun read-file-text (file)
  (with-open-file (s file :external-format :latin-1)
    (let ((text (make-string (file-length s))))
      (subseq text 0 (read-sequence text s)))))

(defun strip-comments (text)
  "Removes /* */ and // comments; keeps string literals and line breaks."
  (with-output-to-string (out)
    (let ((i 0) (n (length text)))
      (loop while (< i n)
            do (let ((c (char text i)))
                 (cond
                   ((and (char= c #\/) (< (1+ i) n) (char= (char text (1+ i)) #\*))
                    (let ((end (or (search "*/" text :start2 (+ i 2)) (- n 2))))
                      ;; keep line breaks so line structure stays
                      (loop for j from i below (+ end 2)
                            when (char= (char text j) #\Newline) do (write-char #\Newline out))
                      (write-char #\Space out)
                      (setf i (+ end 2))))
                   ((and (char= c #\/) (< (1+ i) n) (char= (char text (1+ i)) #\/))
                    (setf i (or (position #\Newline text :start i) n)))
                   ((char= c #\")
                    (let ((j (1+ i)))
                      (loop while (and (< j n) (char/= (char text j) #\"))
                            do (when (char= (char text j) #\\) (incf j))
                               (incf j))
                      (write-string text out :start i :end (min n (1+ j)))
                      (setf i (1+ j))))
                   (t (write-char c out) (incf i))))))))

(defun logical-lines (text)
  "Lines with backslash continuations joined; CR removed."
  (let ((lines '()) (current (make-string-output-stream)))
    (with-input-from-string (s (remove #\Return text))
      (loop for line = (read-line s nil)
            while line
            do (let ((trimmed (string-right-trim '(#\Space #\Tab) line)))
                 (if (and (plusp (length trimmed))
                          (char= (char trimmed (1- (length trimmed))) #\\))
                     (progn (write-string (subseq trimmed 0 (1- (length trimmed))) current)
                            (write-char #\Space current))
                     (progn (write-string line current)
                            (push (get-output-stream-string current) lines))))))
    (nreverse lines)))

;;; ------------------------------------------------------------------
;;; Tokenizer (C tokens; numbers keep their text)

(defun ident-start-p (c) (or (alpha-char-p c) (char= c #\_)))
(defun ident-char-p (c) (or (alphanumericp c) (char= c #\_)))

(defparameter +punctuators+
  '("<<" ">>" "<=" ">=" "==" "!=" "&&" "||" "##"
    "(" ")" "[" "]" "{" "}" "," ";" "!" "~" "*" "/" "%" "+" "-" "<" ">"
    "&" "^" "|" "?" ":" "=" "." "#"))

(defun tokenize (text)
  "List of tokens: (:ident . s) (:number . s) (:string . s) (:punct . s)."
  (let ((tokens '()) (i 0) (n (length text)))
    (loop while (< i n)
          do (let ((c (char text i)))
               (cond
                 ((member c '(#\Space #\Tab #\Newline #\Page #\Return)) (incf i))
                 ((ident-start-p c)
                  (let ((j (or (position-if-not #'ident-char-p text :start i) n)))
                    (push (cons :ident (subseq text i j)) tokens) (setf i j)))
                 ((digit-char-p c)
                  (let ((j (or (position-if-not (lambda (ch) (or (alphanumericp ch) (char= ch #\.)))
                                                text :start i)
                               n)))
                    (push (cons :number (subseq text i j)) tokens) (setf i j)))
                 ((char= c #\")
                  (let ((j (1+ i)) (out (make-string-output-stream)))
                    (loop while (and (< j n) (char/= (char text j) #\"))
                          do (if (char= (char text j) #\\)
                                 (progn (incf j) (when (< j n) (write-char (char text j) out)))
                                 (write-char (char text j) out))
                             (incf j))
                    (push (cons :string (get-output-stream-string out)) tokens)
                    (setf i (1+ j))))
                 ((char= c #\')
                  (let ((j (position #\' text :start (1+ i))))
                    (push (cons :number (format nil "~D" (char-code (char text (1+ i))))) tokens)
                    (setf i (1+ j))))
                 (t
                  (let ((p (find-if (lambda (p) (and (<= (+ i (length p)) n)
                                                     (string= p text :start2 i :end2 (+ i (length p)))))
                                    +punctuators+)))
                    (unless p (error "tokenize: unexpected character ~S in ~S" c text))
                    (push (cons :punct p) tokens)
                    (incf i (length p)))))))
    (nreverse tokens)))

(defun tok-p (tok kind &optional text)
  (and tok (eq (car tok) kind) (or (null text) (string= (cdr tok) text))))

(defun parse-c-integer (text)
  "C integer literal (decimal, hex, octal; suffixes u/l) -> integer."
  (let* ((s (string-right-trim "uUlL" text))
         (value (cond ((and (> (length s) 2) (char-equal (char s 1) #\x))
                       (parse-integer s :start 2 :radix 16))
                      ((and (> (length s) 1) (char= (char s 0) #\0))
                       (parse-integer s :start 1 :radix 8))
                      (t (parse-integer s)))))
    value))

;;; ------------------------------------------------------------------
;;; Macros and the preprocessor

(defstruct macro name params body)    ; params NIL = object-like; body = tokens

(defvar *macros*)                     ; name -> macro
(defvar *assume-true-functions* '("WINAPI_FAMILY_PARTITION")
  "Function-like macros from Windows headers that the ODBC headers test in #if;
treated as true (desktop API family).")

(define-condition unevaluable (error) ((reason :initarg :reason :reader reason))
  (:report (lambda (c s) (format s "not evaluable: ~A" (reason c)))))

(defun eval-tokens (tokens &key in-if (depth 0))
  "Evaluates a C constant expression. IN-IF: #if rules (defined, unknown
identifiers are 0). Returns an integer or a string."
  (when (> depth 50) (error 'unevaluable :reason "macro recursion"))
  (let ((toks tokens))
    (labels ((peek () (first toks))
             (next () (pop toks))
             (expect (text)
               (unless (tok-p (next) :punct text)
                 (error 'unevaluable :reason (format nil "expected ~A" text))))
             (ident-value (name)
               (let ((m (gethash name *macros*)))
                 (cond
                   ((and in-if (string= name "defined"))
                    (let ((paren (tok-p (peek) :punct "(")))
                      (when paren (next))
                      (let ((id (next)))
                        (when paren (expect ")"))
                        (if (gethash (cdr id) *macros*) 1 0))))
                   ((and in-if (member name *assume-true-functions* :test #'string=)
                         (tok-p (peek) :punct "("))
                    (let ((level 0))
                      (loop for tk = (next)
                            do (cond ((tok-p tk :punct "(") (incf level))
                                     ((tok-p tk :punct ")") (decf level)))
                            until (zerop level)))
                    1)
                   ((and m (null (macro-params m)))
                    (if (null (macro-body m))
                        (if in-if 0 (error 'unevaluable :reason (format nil "~A is empty" name)))
                        (eval-tokens (macro-body m) :in-if in-if :depth (1+ depth))))
                   (in-if 0)
                   (t (error 'unevaluable :reason (format nil "unknown identifier ~A" name))))))
             (primary ()
               (let ((tk (next)))
                 (cond
                   ((null tk) (error 'unevaluable :reason "unexpected end"))
                   ((eq (car tk) :number) (parse-c-integer (cdr tk)))
                   ((eq (car tk) :string)
                    ;; adjacent string literals concatenate
                    (let ((s (cdr tk)))
                      (loop while (and (peek) (eq (car (peek)) :string))
                            do (setf s (concatenate 'string s (cdr (next)))))
                      s))
                   ((eq (car tk) :ident) (ident-value (cdr tk)))
                   ((tok-p tk :punct "(") (prog1 (expr 0) (expect ")")))
                   ((tok-p tk :punct "-") (- (unary)))
                   ((tok-p tk :punct "+") (unary))
                   ((tok-p tk :punct "!") (if (zerop (unary)) 1 0))
                   ((tok-p tk :punct "~") (lognot (unary)))
                   (t (error 'unevaluable :reason (format nil "unexpected ~A" (cdr tk)))))))
             (unary () (primary))
             (binary-precedence (tk)
               (and tk (eq (car tk) :punct)
                    (cdr (assoc (cdr tk)
                                '(("*" . 10) ("/" . 10) ("%" . 10) ("+" . 9) ("-" . 9)
                                  ("<<" . 8) (">>" . 8) ("<" . 7) ("<=" . 7) (">" . 7) (">=" . 7)
                                  ("==" . 6) ("!=" . 6) ("&" . 5) ("^" . 4) ("|" . 3)
                                  ("&&" . 2) ("||" . 1) ("?" . 0))
                                :test #'string=))))
             (apply-op (op a b)
               (flet ((bool (x) (if x 1 0)))
                 (cond ((string= op "*") (* a b))
                       ((string= op "/") (truncate a b))
                       ((string= op "%") (rem a b))
                       ((string= op "+") (+ a b))
                       ((string= op "-") (- a b))
                       ((string= op "<<") (ash a b))
                       ((string= op ">>") (ash a (- b)))
                       ((string= op "<") (bool (< a b)))
                       ((string= op "<=") (bool (<= a b)))
                       ((string= op ">") (bool (> a b)))
                       ((string= op ">=") (bool (>= a b)))
                       ((string= op "==") (bool (= a b)))
                       ((string= op "!=") (bool (/= a b)))
                       ((string= op "&") (logand a b))
                       ((string= op "^") (logxor a b))
                       ((string= op "|") (logior a b))
                       ((string= op "&&") (bool (and (/= a 0) (/= b 0))))
                       ((string= op "||") (bool (or (/= a 0) (/= b 0)))))))
             (expr (min-prec)
               (let ((left (unary)))
                 (loop for tk = (peek)
                       for prec = (binary-precedence tk)
                       while (and prec (>= prec min-prec))
                       do (next)
                          (if (string= (cdr tk) "?")
                              (let ((then (expr 0)))
                                (expect ":")
                                (let ((else (expr 0)))
                                  (setf left (if (/= left 0) then else))))
                              (let ((right (expr (1+ prec))))
                                (unless (and (integerp left) (integerp right))
                                  (error 'unevaluable :reason "operator on a string"))
                                (setf left (apply-op (cdr tk) left right)))))
                 left)))
      (prog1 (expr 0)
        (when toks (error 'unevaluable :reason (format nil "trailing ~A" (cdr (first toks)))))))))

(defun parse-define (rest)
  "REST: text after #define. Returns a MACRO."
  (let* ((rest (string-left-trim '(#\Space #\Tab) rest))
         (end (or (position-if-not #'ident-char-p rest) (length rest)))
         (name (subseq rest 0 end))
         (after (subseq rest end)))
    (if (and (plusp (length after)) (char= (char after 0) #\())
        (let ((close (position #\) after)))
          (make-macro :name name
                      :params (mapcar (lambda (p) (string-trim " " p))
                                      (split-string (subseq after 1 close) #\,))
                      :body (tokenize (subseq after (1+ close)))))
        (make-macro :name name :params nil :body (tokenize after)))))

(defun split-string (s ch)
  (loop with start = 0
        for pos = (position ch s :start start)
        collect (subseq s start pos)
        while pos do (setf start (1+ pos))))

(defun directive (line)
  "For a preprocessor line: (values keyword rest), else NIL."
  (let ((trimmed (string-left-trim '(#\Space #\Tab) line)))
    (when (and (plusp (length trimmed)) (char= (char trimmed 0) #\#))
      (let* ((body (string-left-trim '(#\Space #\Tab) (subseq trimmed 1)))
             (end (or (position-if-not #'alpha-char-p body) (length body))))
        (values (intern (string-upcase (subseq body 0 end)) :keyword)
                (subseq body end))))))

(defstruct api
  (macro-order '())          ; macro names in definition order (active branches)
  (code "")                  ; active non-directive text
  macros)

(defun preprocess (files defines)
  "Runs FILES (in order) through the preprocessor with DEFINES (alist
name . value text). Returns an API."
  (let ((*macros* (make-hash-table :test #'equal))
        (order '())
        (code (make-string-output-stream)))
    (loop for (name . value) in defines
          do (setf (gethash name *macros*) (make-macro :name name :body (tokenize value))))
    (dolist (file files)
      ;; stack entries: (active-now parent-active any-branch-taken)
      (let ((stack '()))
        (flet ((active () (or (null stack) (first (first stack)))))
          (dolist (line (logical-lines (strip-comments (read-file-text file))))
            (multiple-value-bind (kw rest) (directive line)
              (case kw
                ((:if :ifdef :ifndef)
                 (let* ((parent (active))
                        (cond (and parent
                                   (ecase kw
                                     (:if (/= 0 (eval-tokens (tokenize rest) :in-if t)))
                                     (:ifdef (and (gethash (string-trim '(#\Space #\Tab) rest) *macros*) t))
                                     (:ifndef (not (gethash (string-trim '(#\Space #\Tab) rest) *macros*)))))))
                   (push (list cond parent cond) stack)))
                (:elif
                 (destructuring-bind (now parent taken) (pop stack)
                   (declare (ignore now))
                   (let ((cond (and parent (not taken)
                                    (/= 0 (eval-tokens (tokenize rest) :in-if t)))))
                     (push (list cond parent (or taken cond)) stack))))
                (:else
                 (destructuring-bind (now parent taken) (pop stack)
                   (declare (ignore now))
                   (push (list (and parent (not taken)) parent t) stack)))
                (:endif (pop stack))
                (:define
                 (when (active)
                   (let ((m (parse-define rest)))
                     (unless (gethash (macro-name m) *macros*)
                       (push (macro-name m) order))
                     (setf (gethash (macro-name m) *macros*) m))))
                (:undef
                 (when (active) (remhash (string-trim '(#\Space #\Tab) rest) *macros*)))
                ((:include :pragma :error :line nil)
                 (when (and (null kw) (active))
                   (write-line line code)))
                (t (when (active) (warn "unknown directive ~A" kw)))))))
        (when stack (error "~A: unterminated #if" file))))
    (make-api :macro-order (nreverse order)
              :code (get-output-stream-string code)
              :macros *macros*)))

;;; ------------------------------------------------------------------
;;; Declarations: typedefs, structures, prototypes

(defun sal-annotation-p (tok)
  "Microsoft source annotations: _In_, _Out_writes_opt_(...), _Success_(...)."
  (and (eq (car tok) :ident)
       (let ((s (cdr tok)))
         (and (> (length s) 2) (char= (char s 0) #\_) (upper-case-p (char s 1))
              (char= (char s (1- (length s))) #\_)))))

(defun strip-annotations (tokens)
  (let ((out '()))
    (loop while tokens
          do (let ((tk (pop tokens)))
               (if (sal-annotation-p tk)
                   (when (tok-p (first tokens) :punct "(")
                     (let ((level 0))
                       (loop for x = (pop tokens)
                             do (cond ((tok-p x :punct "(") (incf level))
                                      ((tok-p x :punct ")") (decf level)))
                             until (zerop level))))
                   (push tk out))))
    (nreverse out)))

(defun statements (tokens)
  "Splits TOKENS at top-level semicolons (braces nest)."
  (let ((result '()) (current '()) (depth 0))
    (dolist (tk tokens)
      (cond ((tok-p tk :punct "{") (incf depth) (push tk current))
            ((tok-p tk :punct "}") (decf depth) (push tk current))
            ((and (zerop depth) (tok-p tk :punct ";"))
             (when current (push (nreverse current) result))
             (setf current '()))
            (t (push tk current))))
    (nreverse result)))

(defparameter +calling-conventions+ '("SQL_API" "__stdcall" "EXPORT" "WINAPI" "CALLBACK" "extern"))

(defun split-top-level (tokens separator)
  (let ((parts '()) (current '()) (depth 0))
    (dolist (tk tokens)
      (cond ((or (tok-p tk :punct "(") (tok-p tk :punct "[")) (incf depth) (push tk current))
            ((or (tok-p tk :punct ")") (tok-p tk :punct "]")) (decf depth) (push tk current))
            ((and (zerop depth) (tok-p tk :punct separator))
             (push (nreverse current) parts) (setf current '()))
            (t (push tk current))))
    (push (nreverse current) parts)
    (nreverse parts)))

(defun declarator (tokens)
  "TOKENS of 'type name' (name may be missing). Returns (values type-tokens name)."
  (let ((last (car (last tokens))))
    (if (and (eq (car last) :ident)
             (> (length tokens) 1)
             (not (member (cdr last) '("int" "long" "short" "char" "double" "float" "void")
                          :test #'string=)))
        (values (butlast tokens) (cdr last))
        (values tokens nil))))

(defstruct prototype name return-type params)   ; params: list of (name . type-tokens)

(defun read-declarations (api)
  "Typedefs (name -> type tokens), structs (name -> fields), prototypes."
  (let ((typedefs (make-hash-table :test #'equal))
        (structs (make-hash-table :test #'equal))
        (prototypes (make-hash-table :test #'equal)))
    (dolist (st (statements (strip-annotations (tokenize (api-code api)))))
      (cond
        ;; typedef struct tag { fields } NAME[, *PNAME]
        ((and (tok-p (first st) :ident "typedef")
              (or (tok-p (second st) :ident "struct") (tok-p (second st) :ident "enum"))
              (find-if (lambda (tk) (tok-p tk :punct "{")) st))
         (let* ((open (position-if (lambda (tk) (tok-p tk :punct "{")) st))
                (close (position-if (lambda (tk) (tok-p tk :punct "}")) st :from-end t))
                (names (remove-if-not (lambda (tk) (eq (car tk) :ident)) (subseq st (1+ close))))
                (struct-p (tok-p (second st) :ident "struct")))
           (when (and struct-p names)
             (let ((fields (loop for field in (statements (subseq st (1+ open) close))
                                 collect (multiple-value-bind (type name) (declarator field)
                                           (cons name type)))))
               (setf (gethash (cdr (first names)) structs) fields)))))
        ;; typedef TYPE NAME
        ((tok-p (first st) :ident "typedef")
         (unless (find-if (lambda (tk) (tok-p tk :punct "(")) st)
           (multiple-value-bind (type name) (declarator (rest st))
             (when name (setf (gethash name typedefs) type)))))
        ;; RET SQL_API Name ( params )
        ((let ((open (position-if (lambda (tk) (tok-p tk :punct "(")) st)))
           (and open (> open 1)
                (eq (car (nth (1- open) st)) :ident)
                (tok-p (car (last st)) :punct ")")
                (let* ((name (cdr (nth (1- open) st)))
                       (ret (remove-if (lambda (tk) (and (eq (car tk) :ident)
                                                         (member (cdr tk) +calling-conventions+
                                                                 :test #'string=)))
                                       (subseq st 0 (1- open))))
                       (inner (subseq st (1+ open) (1- (length st))))
                       (params (if (or (null inner)
                                       (and (= 1 (length inner)) (tok-p (first inner) :ident "void")))
                                   '()
                                   (loop for p in (split-top-level inner ",")
                                         for i from 1
                                         collect (multiple-value-bind (type pname) (declarator p)
                                                   (cons (or pname (format nil "arg~D" i)) type))))))
                  (setf (gethash name prototypes)
                        (make-prototype :name name :return-type ret :params params))
                  t))))))
    (values typedefs structs prototypes)))

;;; ------------------------------------------------------------------
;;; Types

(defparameter +windows-types+
  ;; Types the ODBC headers take from windows.h (precondition of sql.h).
  '(("INT64" "__int64") ("UINT64" "unsigned" "__int64") ("HWND" "void" "*")
    ("HANDLE" "void" "*") ("WCHAR" "wchar_t") ("DWORD" "unsigned" "long")
    ("WORD" "unsigned" "short") ("BYTE" "unsigned" "char") ("BOOL" "int")))

(defparameter +base-types+
  ;; Windows ABI (LLP64): long is 32 bits. The pointer-sized ODBC types are
  ;; handled separately (headers read with and without _WIN64).
  '((("char") . :char) (("signed" "char") . :char) (("unsigned" "char") . :uchar)
    (("short") . :int16) (("signed" "short") . :int16) (("unsigned" "short") . :uint16)
    (("int") . :int32) (("signed") . :int32) (("signed" "int") . :int32)
    (("unsigned") . :uint32) (("unsigned" "int") . :uint32)
    (("long") . :int32) (("signed" "long") . :int32) (("unsigned" "long") . :uint32)
    (("__int64") . :int64) (("unsigned" "__int64") . :uint64)
    (("double") . :double) (("float") . :float) (("void") . :void)
    (("wchar_t") . :uint16)))

(defun normalize-base (words)
  "Drops 'int' after short/long and 'const'."
  (let ((w (remove "const" words :test #'string=)))
    (if (and (member "int" w :test #'string=)
             (or (member "long" w :test #'string=) (member "short" w :test #'string=)))
        (remove "int" w :test #'string=)
        w)))

(defun resolve-type (tokens typedefs macros &optional (depth 0))
  "C type tokens -> CFFI keyword (:pointer for any pointer)."
  (when (> depth 30) (error "type recursion: ~S" tokens))
  (when (find-if (lambda (tk) (tok-p tk :punct "*")) tokens)
    (return-from resolve-type :pointer))
  ;; expand object-like macros (ODBCINT64 -> __int64, SQLLEN -> SQLINTEGER)
  (let ((words (loop for tk in tokens
                     for m = (and (eq (car tk) :ident) (gethash (cdr tk) macros))
                     append (if (and m (null (macro-params m)) (macro-body m))
                                (mapcar #'cdr (macro-body m))
                                (list (cdr tk))))))
    (when (member "*" words :test #'string=)
      (return-from resolve-type :pointer))
    (let ((base (cdr (assoc (normalize-base words) +base-types+ :test #'equal))))
      (cond
        (base base)
        ((and (= 1 (length words)) (gethash (first words) typedefs))
         (resolve-type (gethash (first words) typedefs) typedefs macros (1+ depth)))
        ((and (= 1 (length words)) (assoc (first words) +windows-types+ :test #'string=))
         (resolve-type (mapcar (lambda (w) (if (string= w "*") (cons :punct w) (cons :ident w)))
                               (rest (assoc (first words) +windows-types+ :test #'string=)))
                       typedefs macros (1+ depth)))
        ((and (= 2 (length words)) (string= (first words) "unsigned"))
         ;; unsigned TYPEDEF (unsigned ODBCINT64 after expansion is covered above)
         (let ((inner (resolve-type (list (cons :ident (second words))) typedefs macros (1+ depth))))
           (case inner (:int16 :uint16) (:int32 :uint32) (:int64 :uint64) (:char :uchar) (t inner))))
        (t (error "unknown type ~{~A~^ ~}" words))))))

;;; ------------------------------------------------------------------
;;; Reading the headers (two variants: with and without _WIN64)

(defstruct header-view api typedefs structs prototypes)

(defun read-headers (spec &key (header-dir (system-file "odbc-headers/")) pointer-width-64)
  (let* ((files (mapcar (lambda (f) (merge-pathnames f header-dir)) (getf spec :headers)))
         (defines (append (getf spec :defines)
                          (when pointer-width-64 (list (cons (getf spec :pointer-width-define) "1")))))
         (api (preprocess files defines)))
    (multiple-value-bind (typedefs structs prototypes) (read-declarations api)
      (make-header-view :api api :typedefs typedefs :structs structs :prototypes prototypes))))

(defun view-type (view tokens)
  (resolve-type tokens (header-view-typedefs view) (api-macros (header-view-api view))))

(defun api-constants (view prefix)
  "Alist (name . value) of integer or string macros starting with PREFIX, in order."
  (let ((*macros* (api-macros (header-view-api view))))
    (loop for name in (api-macro-order (header-view-api view))
          for m = (gethash name *macros*)
          when (and m (null (macro-params m))
                    (>= (length name) (length prefix))
                    (string= prefix name :end2 (length prefix))
                    (macro-body m))
            append (handler-case (let ((v (eval-tokens (macro-body m))))
                                   (list (cons name v)))
                     (unevaluable () nil)))))

(defun spec-excluded-constants (spec)
  (loop for (names) in (getf spec :exclude-constants) append names))

(defun spec-constants (view spec)
  "API-CONSTANTS of VIEW with the prefix of SPEC, without the excluded ones."
  (let ((excluded (spec-excluded-constants spec)))
    (remove-if (lambda (entry) (member (car entry) excluded :test #'string=))
               (api-constants view (getf spec :constant-prefix)))))

(defun api-prototypes (view) (header-view-prototypes view))
(defun api-structs (view) (header-view-structs view))

;;; ------------------------------------------------------------------
;;; Lisp names

(defun lispify (c-name)
  "SQLGetDiagRecW -> sql-get-diag-rec-w, StrLen_or_IndPtr -> str-len-or-ind-ptr,
fParamType -> f-param-type, cbColDef -> cb-col-def."
  (let ((out (make-string-output-stream))
        (s c-name)
        (n (length c-name)))
    (loop for i from 0 below n
          for c = (char s i)
          do (cond
               ((char= c #\_) (write-char #\- out))
               ((and (upper-case-p c) (> i 0)
                     (let ((prev (char s (1- i))))
                       (or (lower-case-p prev) (digit-char-p prev)
                           ;; end of an acronym: SQLFetch -> sql-fetch
                           (and (upper-case-p prev) (< (1+ i) n) (lower-case-p (char s (1+ i)))))))
                (unless (char= (char s (1- i)) #\_) (write-char #\- out))
                (write-char (char-downcase c) out))
               (t (write-char (char-downcase c) out))))
    (get-output-stream-string out)))

(defun lisp-function-name (c-name)
  (concatenate 'string "%%" (lispify c-name)))

;;; ------------------------------------------------------------------
;;; Output

(defun header-type-name (tokens)
  "Single type name of a parameter (SQLLEN) or NIL for pointers/compound types."
  (and (= 1 (length tokens)) (eq (car (first tokens)) :ident) (cdr (first tokens))))

(defun prototype-c-text (proto)
  "The prototype in C with header type names (for the compiler check)."
  (flet ((type-text (tokens) (format nil "~{~A~^ ~}" (mapcar #'cdr tokens))))
    (format nil "SQLRETURN SQL_API ~A(~:[void~;~:*~{~A~^, ~}~]);"
            (prototype-name proto)
            (loop for (name . type) in (prototype-params proto)
                  collect (format nil "~A ~A" (type-text type) name)))))

(defun foreign-type-form (view tokens spec)
  "Lisp foreign type for a parameter/field as text: spec name, :pointer or base keyword."
  (let* ((name (header-type-name tokens))
         (lisp (and name (second (assoc name (spec-types spec) :test #'string=)))))
    (if lisp
        (string-downcase (symbol-name lisp))
        (format nil "~(~S~)" (view-type view tokens)))))

(defun generated-text (&key (spec (read-spec)) (header-dir (system-file "odbc-headers/")))
  "The text of src/odbc/odbc-api-generated.lisp."
  (let* ((v64 (read-headers spec :header-dir header-dir :pointer-width-64 t))
         (v32 (read-headers spec :header-dir header-dir :pointer-width-64 nil))
         (c64 (spec-constants v64 spec))
         (c32 (spec-constants v32 spec))
         ;; SQL_API is __stdcall in the headers: matters only on 32-bit Windows
         (stdcall (let ((m (gethash "SQL_API" (api-macros (header-view-api v64)))))
                    (and m (equal (mapcar #'cdr (macro-body m)) '("__stdcall")))))
         (*print-case* :downcase))
    ;; same names with and without _WIN64; values may differ (SQL_C_BOOKMARK)
    (unless (equal (mapcar #'car c64) (mapcar #'car c32))
      (error "constants differ with/without ~A: ~S" (getf spec :pointer-width-define)
             (set-exclusive-or (mapcar #'car c64) (mapcar #'car c32) :test #'string=)))
    (with-output-to-string (out)
      (format out ";;; -*- Mode: Lisp -*-
;;; GENERATED by tools/generate-odbc-api.lisp from odbc-headers/*.h
;;; (Microsoft ODBC headers, MIT license) and tools/odbc-api-spec.lisp.
;;; Do not edit; change the spec and regenerate:
;;;   (asdf:load-system :qss-cl-odbc-generate) (qss-cl-odbc-generate:generate)
;;; The test GENERATED-FILE-IS-CURRENT checks that this file matches.

(in-package :qss-cl-odbc)

;;; Types~%")
      ;; foreign types
      (loop for (c-name lisp-name) in (spec-types spec)
            for t64 = (view-type v64 (list (cons :ident c-name)))
            for t32 = (view-type v32 (list (cons :ident c-name)))
            do (if (eq t64 t32)
                   (format out "(cffi:defctype ~(~A~) ~S)                ; ~A~%" lisp-name t64 c-name)
                   (format out "(cffi:defctype ~(~A~) #.(if (= 8 (cffi:foreign-type-size :pointer)) ~S ~S)) ; ~A, pointer-sized~%"
                           lisp-name t64 t32 c-name)))
      ;; structures
      (format out "~%;;; Structures~%")
      (loop for (c-name lisp-name) in (spec-structs spec)
            for fields = (or (gethash c-name (api-structs v64)) (error "no struct ~A" c-name))
            do (format out "(cffi:defcstruct ~(~A~)                         ; ~A~%" lisp-name c-name)
               (loop for (fname . ftype) in fields
                     for rest on fields
                     do (format out "  (~(~A~) ~A)~:[)~;~]~%"
                                (lispify fname) (foreign-type-form v64 ftype spec) (rest rest))))
      ;; functions
      (format out "~%;;; Functions (all return SQLRETURN)~%")
      (dolist (c-name (spec-functions spec))
        (let ((p64 (or (gethash c-name (api-prototypes v64)) (error "no prototype ~A" c-name)))
              (p32 (gethash c-name (api-prototypes v32))))
          (unless (and p32 (equal (prototype-params p64) (prototype-params p32)))
            (error "prototype ~A differs with/without _WIN64" c-name))
          (unless (equal (header-type-name (prototype-return-type p64)) "SQLRETURN")
            (error "~A does not return SQLRETURN" c-name))
          (format out "(cffi:defcfun (~S ~A~@[ :convention #+(and windows x86) :stdcall #-(and windows x86) :cdecl~]) sql-return"
                  c-name (lisp-function-name c-name) stdcall)
          (loop for (pname . ptype) in (prototype-params p64)
                do (format out "~%  (~A ~A)" (lispify pname) (foreign-type-form v64 ptype spec)))
          (format out ")~%")))
      ;; constants
      (format out "~%;;; Constants (~D)~%" (length c64))
      (loop for (names reason) in (getf spec :exclude-constants)
            do (format out ";;; Left out: ~{~A~^, ~}~%;;;   ~A~%" names reason))
      (loop for (name . value) in c64
            for value32 = (cdr (assoc name c32 :test #'string=))
            do (cond ((stringp value)
                      (format out "(defparameter $~A ~S)~%" name value))
                     ((eql value value32)
                      (format out "(defconstant $~A ~D)~%" name value))
                     (t
                      (format out "(defconstant $~A #.(if (= 8 (cffi:foreign-type-size :pointer)) ~D ~D)) ; pointer-sized~%"
                              name value value32)))))))

(defun generate (&key (output (system-file "src/odbc/odbc-api-generated.lisp")))
  "Writes the generated file; returns its pathname."
  (let ((text (generated-text)))
    (with-open-file (s output :direction :output :if-exists :supersede
                              :external-format :utf-8)
      (write-string text s))
    output))
