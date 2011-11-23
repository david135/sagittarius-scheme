;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; apropos.scm: REPL support
;;;  
;;;   Copyright (c) 2000-2011  Takashi Kato  <ktakashi@ymail.com>
;;;   
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;   
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;  
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;  
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;  

(library (apropos)
    (export apropos)
    (import (rnrs)
	    (core base) ;; for hashtable->alist
	    (srfi :13 strings)
	    (sagittarius)
	    (sagittarius regex)
	    (sagittarius control)
	    (sagittarius vm))
  
  (define-with-key (apropos arg :key (port (current-output-port)))
    (let ((arg (cond ((symbol? arg) (symbol->string arg))
		     ((string? arg) (regex arg))
		     (else (assertion-violation 'apropos (format "symbol or string required, but got ~a" arg))))))
      (define (match? e)
	(cond ((string? arg)
	       (and (string-contains (symbol->string e) arg) e))
	      ((regex-pattern? arg)
	       (and (looking-at arg (symbol->string e)) e))
	      (else #f)))
      (define core (string->symbol (format "~a" '(core))))
      (define (replace-null-lib name)
	(if (eq? name 'null)
	    core
	    name))
      (let* ((alist (hashtable->alist (library-table (vm-current-library))))
	     (ret (filter values
			  (map (lambda (name/gloc)
				 (if (match? (car name/gloc))
				     name/gloc
				     #f))
			       alist))))				      
	(for-each (lambda (pairs)
		    (format port ";; ~30,,,,a ~a~%"
			    (car pairs)
			    (library-name (gloc-library (cdr pairs)))))
		  ret))
      ;; search parents
      (let* ((parents (library-parents (vm-current-library)))
	     (h (make-equal-hashtable)))
	(for-each (lambda (parent)
		    (let ((lib (car parent))
			  (alist (cdr parent)))
		      (let ((ret (let loop ((alist alist)
					    (r '()))
				   (cond ((null? alist) r)
					 ((match? (caar alist))
					  (loop (cdr alist)
						(lset-union eq? r (list (caar alist)))))
					 (else
					  (loop (cdr alist) r))))))
			
			(for-each (lambda (p)
				    (let ((key (cons p (replace-null-lib (library-name lib)))))
				      (unless (hashtable-ref h key #f)
					(hashtable-set! h key #t)))
				    #;(format port ";; ~30,,,,a ~a~%"
					    p
				    (replace-null-lib (library-name lib))))
				  ret))))
		  parents)
	(for-each (lambda (p)
		    (format port ";; ~30,,,,a ~a~%" (car p) (cdr p)))
		  (list-sort (lambda (key1 key2)
			       (let ((lib1 (symbol->string (cdr key1)))
				     (var1 (symbol->string (car key1)))
				     (lib2 (symbol->string (cdr key2)))
				     (var2 (symbol->string (car key2))))
				 (if (string=? lib1 lib2)
				     (string< var1 var2)
				     (string< lib1 lib2)
				 )))
			     (hashtable-keys-list h)))
	)
      ))
)