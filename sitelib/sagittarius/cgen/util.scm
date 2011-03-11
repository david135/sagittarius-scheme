;; -*- scheme -*-
(library (sagittarius cgen util)
    (export for-each1-with-index string-split alpha alpha-num
	    c-identifier register-number-compare
	    register-and/or
	    gen-temporary)
    (import (rnrs (6))
	    (only (srfi :13) string-tokenize)
	    (only (srfi :14) string->char-set)
	    (sagittarius format))

  (define for-each1-with-index
    (lambda (proc lst)
      (let loop ((i 0)
		 (lst lst))
	(cond ((null? lst) '())
	      (else
	       (proc i (car lst))
	       (loop (+ i 1) (cdr lst)))))))

  ;; number compare
  (define-syntax register-number-compare
    (syntax-rules ()
      ((_ name op)
       (define (name body dispatch k)
	 (or (= (length body) 3)
	     (error 'break ("3 argument required but got ~s" (length body)) body))
	 (dispatch (cadr body) dispatch k)
	 (format #t " ~s " 'op)
	 (dispatch (caddr body) dispatch k)))))

  (define-syntax register-and/or
    (syntax-rules ()
      ((_ name op)
       (define (name body dispatch k)
	 (display "(")
	 (let loop ((first #t)
		    (args (cdr body)))
	   (unless (null? args)
	     (unless first
	       (format #t " ~a " op))
	     (dispatch (car args) dispatch k)
	     (loop #f (cdr args))))
	 (display ")")))))


  (define alpha        "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
  (define alpha-num    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890")
  (define c-identifier "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890*_")

  (define (string-split s . seed)
    (if (null? seed)
	(string-tokenize s (string->char-set alpha-num))
	(string-tokenize s (string->char-set (car seed)))))
  
  (define gen-temporary
    (let ((count 0))
      (lambda ()
	(set! count (+ count 1))
	(string->symbol (format "~s~a" 'cgen_ count))))))
