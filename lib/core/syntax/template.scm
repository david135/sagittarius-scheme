(library (core syntax template)
    (export generate-output)
    (import (core)
	    (core base)
	    (core errors)
	    (core syntax pattern)
	    (core syntax helper)
	    (core misc)
	    (sagittarius))
  ;; from Ypsilon
  (define parse-ellipsis-splicing
    (lambda (form rename compare)
      (let loop ((len 2) (tail (cdddr form)))
	(cond ((and (pair? tail) (compare (car tail) (rename '...)))
	       (loop (+ len 1) (cdr tail)))
	      (else
	       (values (list-head form len) tail len))))))

  (define generate-output
    (lambda (template sids rename compare expr case?)
      (let ((_cdr (rename 'cdr))       (_car (rename 'car))
	    (_quote (rename 'quote))   (_lambda (rename 'lambda))
	    (_null? (rename 'null?))   (_pair? (rename 'pair?))
	    (_map (rename 'map))       (_apply (rename 'apply))	  
	    (_list->vector (rename 'list->vector))
	    (_rename (rename 'rename)) (_compare (rename 'compare)))

	(define generate-ellipsis
	  (lambda (ellipsis body)
	    (let ((sids (ellipsis-sids ellipsis)))
	      (if (pair? sids)
		  (let ((name (sid-name (car sids)))
			(expression (sid-expression (car sids))))
		    (cond ((and (null? (cdr sids))
				(eq? body name))
			   expression)
			  ((and (null? (cdr sids))
				(pair? body)
				(eq? (cadr body) name)
				(null? (cddr body)))
			   `(,_map ,(car body) ,expression))
			  (else
			   `(,_map (,_lambda
				    ,(map sid-name sids)
				    ,body)
				   ,@(map sid-expression sids)))))
		  (assertion-violation "syntax template" "missing ellipsis in expansion." 
				       (list template (unwrap-syntax body)))))))
	(define expand-variable
	  (lambda (tmpl ellipses)
	    (let ((sid (let lp ((sids sids))
			 (and (pair? sids)
			      (if (compare (sid-name (car sids)) tmpl)
				  (car sids)
				  (lp (cdr sids)))))))
	      (if sid
		  (begin
		    (add-control! sid ellipses)
		    (sid-expression sid))
		  `(,_rename (,_quote ,tmpl))))))

	(define syntax-expression?
	  (lambda (expr)
	    (and (pair? expr)
		 (compare (car expr) (rename 'syntax))
		 (or (= (length expr) 2)
		     (syntax-violation 'syntax
				       "expected exactly one datum"
				       expr)))))

	(define loop
	  (lambda (tmpl ellipses in-syntax?)
	    (cond ((variable? tmpl)
		   (expand-variable tmpl ellipses))
		  ((ellipsis-quote? tmpl rename compare)
		   `(,_quote ,(if (pair? (cdr tmpl))
				  (if (pair? (cddr tmpl)) (cddr tmpl) (cadr tmpl))
				  (cdr tmpl))))
		  ;; (p ... ...)
		  ((ellipsis-splicing-pair? tmpl rename compare)
		   (receive (body tail len) (parse-ellipsis-splicing tmpl rename compare)
		     (optimized-append (apply optimized-append
					      rename compare (let ((ellipsis (make-ellipsis '())))
							       (generate-ellipsis ellipsis
										  (loop body
											(cons ellipsis
											      ellipses)
											in-syntax?))))
				       (loop tail ellipses in-syntax?))))
		  
		  ;; (p ...)
		  ((ellipsis-pair? tmpl rename compare)
		   (optimized-append rename compare
				     (let ((ellipsis (make-ellipsis '())))
				       (generate-ellipsis ellipsis
							  (loop (car tmpl)
								(cons ellipsis
								      ellipses)
								in-syntax?)))
				     (loop (cddr tmpl) ellipses in-syntax?)))
		  ((pair? tmpl)
		   (cond ((and case?
			       (syntax-expression? tmpl))
			  (loop (cadr tmpl) ellipses #t))
			 ((and case?
			       (not in-syntax?)
			       (not (syntax-expression? tmpl)))
			  ;; inside of these non-syntax template, there might be
			  ;; syntax expressions, so we need to search it.
			  ;;tmpl
			  (cons (loop (car tmpl) ellipses #f)
				(loop (cdr tmpl) ellipses #f)))
			 (else
			  (optimized-cons rename compare
					  (loop (car tmpl) ellipses in-syntax?)
					  (loop (cdr tmpl) ellipses in-syntax?)))))
		  ((vector? tmpl)
		   `(,_list->vector ,(loop (vector->list tmpl) ellipses in-syntax?)))
		  ((null? tmpl)
		   `(,_quote ()))
		  (else 
		   `(,_quote ,tmpl)))))
	(loop template '() #f))))

)
;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End: