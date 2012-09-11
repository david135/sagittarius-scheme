;; -*- scheme -*-
#!compatible

(import (rnrs)
	(rnrs mutable-pairs)
	(sagittarius)
	(sagittarius vm)
	(srfi :64 testing))

(define-syntax define-lambda
  (syntax-rules ()
    ((_ name formals body ...)
     (define name (lambda formals body ...)))))
(define-lambda f (t rest) `(t ,t))

(test-begin "sagittarius specific")
(test-equal "bytevector->integer"
	    #x12345678
	    (bytevector->integer #vu8(#x12 #x34 #x56 #x78)))
;; make result bignum
(test-equal "bytevector->integer"
	    #x1234567890
	    (bytevector->integer #vu8(#x12 #x34 #x56 #x78 #x90)))
(test-equal "bytevector->integer"
	    #x1234567890abcdef1234567890abcdef
	    (bytevector->integer #vu8(#x12 #x34 #x56 #x78 #x90 #xab #xcd #xef
				      #x12 #x34 #x56 #x78 #x90 #xab #xcd #xef)))


;; with start and end
(test-equal "bytevector->integer (start end)"
	    #x0203
	    (bytevector->integer #vu8(1 2 3 4) 1 3))
(test-equal "bytevector->integer (bignum start end)"
	    #x02030405060708090a
	    (bytevector->integer #vu8(1 2 3 4 5 6 7 8 9 #xa #xb) 1 10))
(test-equal "integer->bytevector"
	    #vu8(#x12 #x34 #x56 #x78)
	    (integer->bytevector #x12345678))
(test-equal "integer->bytevector (with optional)"
	    #vu8(#x00 #x12 #x34 #x56 #x78)
	    (integer->bytevector #x12345678 5))
(test-equal "integer->bytevector (with optional)"
	    #vu8(#x34 #x56 #x78)
	    (integer->bytevector #x12345678 3))


(test-assert "load test"
	     (begin
	       (load "r6rs-hash.scm")
	       (not (vm-r6rs-mode?))))

;;(test-assert "literal list" (eq? '(a b c) '(a b c)))
;;(test-assert "literal list" (eq? '(a b . c) '(a b . c)))
;;(test-assert "literal vector" (eq? #(a b c) #(a b c)))

(test-assert "literal bytevector" (eq? #vu8(1 2 3) #vu8(1 2 3)))

(test-equal "`(t ,t)" (f 'a 'b) '(t a))

(let ((l1 '(a b c))
      (l2 '(a b . c))
      (l3 '((a b) . c))
      (v  '#(a b c)))
  (test-error "literal list set!"
	      (lambda (e) (assertion-violation? e))
	      (set-car! l1 'e))
  (test-error "literal list set!" 
	      (lambda (e) (assertion-violation? e))
	      (set-car! l2 'e))
  (test-error "literal list set!" 
	      (lambda (e) (assertion-violation? e))
	      (set-car! (car l3) 'e))
  (test-error "literal vector set!" 
	      (lambda (e) (assertion-violation? e))
	      (vector-set! v 0 'e)))

(test-error "literal bytevector u8 set!"
	    (lambda (e) (assertion-violation? e))
	    (bytevector-u8-set! #vu8(1 2 3) 0 4))
(test-error "literal bytevector s8 set!"
	    (lambda (e) (assertion-violation? e))
	    (bytevector-s8-set! #vu8(1 2 3) 0 4))
(test-error "literal bytevector native u16 set!"
	    (lambda (e) (assertion-violation? e))
	    (bytevector-u16-native-set! #vu8(1 2 3 4) 1 5))
(test-error "literal bytevector u16 set!"
	    (lambda (e) (assertion-violation? e))
	    (bytevector-u16-set! #vu8(1 2 3 4) 1 5))
(test-error "literal bytevector native s16 set!"
	    (lambda (e) (assertion-violation? e))
	    (bytevector-s16-native-set! #vu8(1 2 3 4) 1 5))
(test-error "literal bytevector s16 set!"
	    (lambda (e) (assertion-violation? e))
	    (bytevector-s16-set! #vu8(1 2 3 4) 1 5))

;; Issue 12
(test-equal "\\x0; test" #vu8(0) (string->utf8 "\x0;"))

;; bytevector output-port
(let-values (((out getter) (open-bytevector-output-port)))
  (put-bytevector out #vu8(1 2))
  (test-equal "port-position(binary port)" 2 (port-position out))
  (let ((save (port-position out)))
    (set-port-position! out 0)
    (put-bytevector out #vu8(1 2 3 4 5))
    (set-port-position! out save)
    (put-bytevector out #vu8(1 2 3 4 5)))
  (test-equal "getter" #vu8(1 2 1 2 3 4 5) (getter)))

(let-values (((out getter) (open-string-output-port)))
  (put-string out "12")
  (test-equal "port-position(binary port)" 2 (port-position out))
  (let ((save (port-position out)))
    (set-port-position! out 0)
    (put-string out "12345")
    (set-port-position! out save)
    (put-string out "12345"))
  (test-equal "getter" "1212345" (getter)))

;; issue 15
(test-equal "heavy call" 1 ((let ((c (lambda (n) n))) c) 1))


(library (inner)
    (export (rename (fuga buzz) (car first)))
    (import (rnrs))
  (define fuga 'fuga))

(library (test)
    (export)
    (import (rnrs) (sagittarius) (inner))
  (define test '*test*)
  (define test1 'oops)
  
  (export test1 buzz first)
  (export test (rename cdr test3) (rename car test2))

  (define variable 1)
  (export variable)
  )
(import (test))
(test-equal "multi export syntax" car first)
(test-equal "multi export syntax" car test2)
(test-equal "multi export syntax" buzz 'fuga)
(test-equal "multi export syntax" test1 'oops)
(test-equal "multi export syntax" test '*test*)
;; this must be syntax error
;;(test-error "immutable varialbe" (set! variable 2))

;; issue 16
(define (make-test-binary-port out)
  (define (write! bv start count)
    (put-bytevector out bv start (+ start count))
    count)
  (make-custom-binary-output-port "test port" write! #f #f #f))

(test-equal "custom binary output port"
	    (string->utf8 "test")
	    (call-with-bytevector-output-port
	     (lambda (out)
	       (let* ((bin (make-test-binary-port out))
		      (tin (transcoded-port bin (native-transcoder))))
		 (display "test" tin)))))

;; custom codec test
;; This must be run on UTF-8 file λ
(import (encoding sjis) (encoding euc-jp))
;; sjis
(let ((tr (make-transcoder (sjis-codec) 'lf))
      (file  (string-append (current-directory)
			    "/test/data/sjis.txt")))
  ;; read
  (test-equal "read from sjis file" "あいうえお"
	      (call-with-input-file file
		get-line
		:transcoder tr))
  ;; write
  ;; bytevector contains \n as well
  (let ((bv (call-with-input-file file get-bytevector-all :transcoder #f)))
    (let-values (((out getter) (open-bytevector-output-port tr)))
      (put-string out "あいうえお\n")
      (test-equal "write sjis" bv (getter))))
  )

;; euc-jp
(let ((tr (make-transcoder (euc-jp-codec) 'lf))
      (file  (string-append (current-directory)
			    "/test/data/euc-jp.txt")))
  ;; read
  (test-equal "read from euc-jp file" "あいうえお"
	      (call-with-input-file file
		get-line
		:transcoder tr))
  ;; write
  ;; bytevector contains \n as well
  (let ((bv (call-with-input-file file get-bytevector-all :transcoder #f)))
    (let-values (((out getter) (open-bytevector-output-port tr)))
      (put-string out "あいうえお\n")
      (test-equal "write euc-jp" bv (getter))))
  )

;; some illegal character reading
(test-error "ascii 0 read" (read (open-string-input-port "\x0;")))
(test-error "ascii 1 read" (read (open-string-input-port "\x1;")))
(test-error "ascii 2 read" (read (open-string-input-port "\x2;")))
(test-error "ascii 3 read" (read (open-string-input-port "\x3;")))

;; Textual port buffer problem
(define tr (make-transcoder (utf-8-codec)))
(call-with-port
 (open-bytevector-input-port (string->utf8 "xyzzy") tr)
 (lambda (in)
   (test-equal "first get char" #\x (get-char in))
   (test-equal "first lookahead char" #\y (lookahead-char in))))
(call-with-port
 (open-bytevector-input-port (string->utf8 "abcdef") tr)
 (lambda (in)
   (test-equal "second get char" #\a (get-char in))
   ;; actually, this is the only problem
   (test-equal "sedond lookahead char" #\b (lookahead-char in))))

;; issue 19
(test-assert "bytevector set! (minus values)"
	     (bytevector-s32-native-set! (make-bytevector 4) 0 -1))
(test-assert "bytevector set! (minus values)"
	     (bytevector-s32-set! (make-bytevector 4) 0 -1 'little))
(test-assert "bytevector set! (minus values)"
	     (bytevector-s64-native-set! (make-bytevector 8) 0 -1))
(test-assert "bytevector set! (minus values)"
	     (bytevector-s64-set! (make-bytevector 8) 0 -1 'little))

;;-----------------------------------------------------------------------
;; Al Petrofsky's finding
;; http://groups.google.com/groups?dq=&hl=ja&selm=87g00y4b6l.fsf%40radish.petrofsky.org

(test-equal "Al's call/cc test" 1 (call/cc (lambda (c) (0 (c 1)))))

;; syntax-case stuff
(define-syntax aif
  (lambda (x)
    (syntax-case x ()
      ((aif expr then else)
       (with-syntax ((it (datum->syntax #'aif 'it)))
	 #'(let ((it expr))
	     (if it then else)))))))
(test-equal "aif"
	    1
	    (aif (assq 'a '((a . 1) (b . -2)))
		 (cdr it)
		 it))
(test-equal "aif (with local it)"
	    1
	    (let ((it #f))
	      (aif (assq 'a '((a . 1) (b . -2)))
		   (cdr it)
		   it)))

;; pattern variable resolution
(define-syntax patvar-inner
  (syntax-rules ()
    ((_ r e) 
     (if (= r 1) e #f))))
(define-syntax patvar
  (syntax-rules ()
    ((_ expr)
     (let ((r 1))
       (patvar-inner r expr)))))

(test-equal "pattern variable resolution" 2 
	    (let ((r 2)) (patvar r)))


;; optional argument for utf8->string, string->utf8 (R7RS)
(let ((bv #vu8(#x31 #x32 #x33 #x34 #x35 #x36)))
  (test-equal "utf8->string (with start)" "23456" (utf8->string bv 1))
  (test-equal "utf8->string (with start and end)" "234" (utf8->string bv 1 4)))

(let ((s "123456"))
  (test-equal "string->utf8 (with start)"
	      #vu8(#x32 #x33 #x34 #x35 #x36) (string->utf8 s 1))
  (test-equal "string->utf8 (with start and end)"
	      #vu8(#x32 #x33 #x34) (string->utf8 s 1 4)))

;;; equal? for record
(let ()
  (define-record-type (pare kons pare?)
    (fields (mutable x kar set-kar!)
	    (immutable y kdr)))
  (define a (kons (vector 1 2 3) '(a b c)))
  (define b (kons (vector 1 2 3) '(a b c)))
  (test-assert "equal? (record)" (equal? a b)))

;; vector-append
(let ((a '#(1 2 3))
      (b '#(4 5 6)))
  (test-equal "vector-append (0)" '#() (vector-append))
  (test-equal "vector-append (1)" '#(1 2 3) (vector-append a))
  ;; newly created check
  (test-assert "vector-append (eq?)" (not (eq? a (vector-append a))))
  (test-equal "vector-append (2)" '#(1 2 3 4 5 6) (vector-append a b)))

;; vector-concatenate
(test-equal "vector-append (0)" '#() (vector-concatenate '()))
(test-equal "vector-append (1)" '#(1 2 3) (vector-concatenate '(#(1 2 3))))
;; newly created check
(let ((a '(#(1 2 3))))
  (test-assert "vector-append (eq?)" (not (eq? a (vector-concatenate a)))))
(test-equal "vector-append (2)" '#(1 2 3 4 5 6)
	    (vector-concatenate '(#(1 2 3) #(4 5 6))))

;; bytevector-append
(let ((a #vu8(1 2 3))
      (b #vu8(4 5 6)))
  (test-equal "bytevector-append (0)" #vu8() (bytevector-append))
  (test-equal "bytevector-append (1)" #vu8(1 2 3) (bytevector-append a))
  ;; newly created check
  (test-assert "bytevector-append (eq?)" (not (eq? a (bytevector-append a))))
  (test-equal "bytevector-append (2)" #vu8(1 2 3 4 5 6)
	      (bytevector-append a b)))

;; bytevector-concatenate
(test-equal "bytevector-append (0)" #vu8() (bytevector-concatenate '()))
(test-equal "bytevector-append (1)" #vu8(1 2 3)
	    (bytevector-concatenate '(#vu8(1 2 3))))
;; newly created check
(let ((a '(#vu8(1 2 3))))
  (test-assert "bytevector-append (eq?)" 
	       (not (eq? a (bytevector-concatenate a)))))
(test-equal "bytevector-append (2)" #vu8(1 2 3 4 5 6)
	    (bytevector-concatenate '(#vu8(1 2 3) #vu8(4 5 6))))

;; eqv?
;; R6RS 11.5 6th item
(test-assert "eqv? 0.0 -0.0" (not (eqv? 0.0 -0.0)))

;; SRFI 61 cond
(test-equal "SRFI-61 cond"
	    3
	    (cond ((+ 1 2) number? => (lambda (x) x))))

;; Issue 22
(test-equal "read-delimited-list from custom texutal port"
	    '(1)
	    (let ()
	      (define (make-custom-input-port in)
		(define (read! s start count)
		  (get-string-n! in s start count))
		(define (close) (close-input-port in))
		(make-custom-textual-input-port "test" read! #f #f close))
	      (read-delimited-list 
	       #\) (make-custom-input-port (open-string-input-port "1)")))))

;; list->string has optional arguments start and end
(test-equal "list->string with start"
	    "bcdef" (list->string '(#\a #\b #\c #\d #\e #\f) 1))
(test-equal "list->string with start and end"
	    "bc" (list->string '(#\a #\b #\c #\d #\e #\f) 1 3))
(test-equal "list->string no length" "" (list->string '(#\a) 0 0))
;; list->string only checks given range is char or not
(test-equal "list->string with start (check)"
	    "bcdef" (list->string '(hoge #\b #\c #\d #\e #\f) 1))
(test-equal "list->string with start and end (check)"
	    "bc" (list->string '(hoge #\b #\c hoge hoge hoge) 1 3))
;; error case
(test-error "list->string range error" (lambda (e) e)
	    (list->string '(#\a) -1))
(test-error "list->string range error" (lambda (e) e)
	    (list->string '(#\a) 2 1))
(test-error "list->string range error" (lambda (e) e)
	    (list->string '(#\a) 0 2))

;; mod-inverse


;; mod-expt

(test-end)
