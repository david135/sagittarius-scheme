;; -*- scheme -*-
(library (sagittarius cgen vminsn)
    (export gen-insn)
    (import (rnrs (6))
	    (rnrs eval (6))
	    (match)
	    (sagittarius format)
	    (sagittarius cgen util)
	    (core)
	    (prefix (sagittarius cgen base) base:))

  (define *name-values* '())
  (define *labeled-insns* '())
  (define *insn-defines* '())
  (define *return* '())
  (define *insn-bodies* '())
  (define *combined-insns* '())

  (define (parse-keyword name clauses)
    (let loop ((clauses clauses))
      (cond ((null? clauses) '())
	    ((and (pair? (car clauses))
		  (eq? ':value (caar clauses)))
	     (set! *name-values* (acons name (cadar clauses) *name-values*))
	     (loop (cdr clauses)))
	    ((or (keyword? (car clauses))
		 (symbol? (car clauses)))
	     (case (car clauses)
	       ((:label)
		(set! *labeled-insns* (cons name *labeled-insns*))
		(loop (cdr clauses)))
	       ((:return)
		(set! *return* (cons name *return*))
		(loop (cdr clauses)))
	       ((:combined)
		(set! *combined-insns* (acons name (cadr clauses) *combined-insns*))
		'())
	       (else (loop (cdr clauses)))))
	    (else clauses))))

  (define (def-insn body dispatch k)
    (match body
      (('define-inst name (insn-argc argc src?) . rest)
       (set! *insn-defines* (acons name (list insn-argc argc (if src? 'TRUE 'FALSE)) *insn-defines*))
       (let ((body (parse-keyword name rest)))
	 (if (> (length *return*) 1)
	     (base:warn (format "more then one return keyword(~s)" *return*)))
	 (set! *insn-bodies* (acons name body *insn-bodies*))
	 ((renderer) (format "CASE(~a) {~%" name))
	 (renderer-indent-incl!)
	 (cond ((memq name *return*)
		((renderer) "return AC(vm);"))
	       ((assq name *combined-insns*)
		=> (lambda (info)
		     (let ((names (cdr info)))
		       (for-each (lambda (name)
				   (let ((b (assq name *insn-bodies*)))
				     (or b
					 (error 'define-insn
						"invalid combined instruction" info))
				     (dispatch `(begin ,@(cdr b)) dispatch k)))
				 names))
		     ((renderer) "NEXT;")))
	       (else
		(dispatch `(begin ,@body) dispatch k)
		((renderer) "NEXT;")))
	 (renderer-indent-decl!)
	 ((renderer) (format "~%}~%~%"))))
      (else
       (error 'define-inst "no match form" body))))


  (define (write-header ofile)
    (display (format "/* -*- C -*- */~%"))
    (display (format "/* This file is autmatically generated from ~s. DO NOT EDIT!!*/~%" ofile)))
  
  (define (write-definition)
    (for-each (lambda (def)
		(let ((name (car def))
		      (params (cdr def)))
		  (display (format "DEFINSN(~a" name))
		  (for-each (lambda (param)
			      (display (format ", ~a" param)))
			    params)
		  (display (format ", ~a" (if (memq name *labeled-insns*) 'TRUE 'FALSE)))
		  (display (format ")~%"))))
	      (reverse *insn-defines*)))

  (define (flush-renderer file)
    (write-header file)
    (display "#ifdef DEFINSN")(newline)
    (write-definition)
    (display "#endif /* DEFINSN */")(newline)
    (display "#ifdef VM_LOOP")(newline)
    (display *render-content*)
    (display "#endif /* VM_LOOP */")(newline))

  (define *render-content* "")

  (define (init)
    (let ((renderer (base:generate-renderer
		     (lambda (x)
		       (set! *render-content* 
			     (string-append *render-content* x))))))
      (set-renderer! renderer)
      (base:add-dispatch 'define-inst def-insn)))

  (define (do-generate file)
    (let loop ((r (read)))
      (if (eof-object? r)
	  (flush-renderer file)
	  (begin
	    (base:dispatch-method r base:dispatch-method (lambda (k) k))
	    (loop (read))))))

  (define (gen-insn file)
    (let ((out "vminsn.c"))
      (base:init)
      (init)
      (if (file-exists? out)
	  (delete-file out))
      (with-output-to-file out
	(lambda ()
	  (with-input-from-file file
	    (lambda ()
	      (do-generate file)))))))
)