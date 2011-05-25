;; -*- scheme -*-
;; syntax-case compiler
(define-constant PATTERN 2) ;; the same as compiler.scm

(define extend-env
 (lambda (new old)
   (acons PATTERN new old)))

(define (count-pair p)
    (let loop ((lst p) (n 0))
      (if (pair? lst) (loop (cdr lst) (+ n 1)) n)))

(define unique-id-list?
  (lambda (lst)
    (and (list? lst)
         (not (let loop ((lst lst))
                (and (pair? lst)
                     (or (not (variable? (car lst)))
                         (id-memq (car lst) (cdr lst))
                         (loop (cdr lst)))))))))

(define ellipsis?
  (lambda (expr)
    (and (variable? expr)
	 (eq? (identifier->symbol expr) '...))))

(define bar?
  (lambda (expr)
    (and (variable? expr)
	 (eq? (identifier->symbol expr) '_))))

(define ellipsis-pair?
  (lambda (form)
    (and (pair? form)
         (pair? (cdr form))
         (ellipsis? (cadr form)))))

(define ellipsis-splicing-pair?
  (lambda (form)
    (and (pair? form)
         (pair? (cdr form))
         (ellipsis? (cadr form))
         (pair? (cddr form))
         (ellipsis? (caddr form)))))

(define ellipsis-quote?
  (lambda (form)
    (and (pair? form)
         (ellipsis? (car form))
         (pair? (cdr form))
         (null? (cddr form)))))

(define check-pattern
  (lambda (pat lites)

    (define check-duplicate-variable
      (lambda (pat lites)
        (let loop ((lst pat) (pool '()))
          (cond ((pair? lst)
                 (loop (cdr lst)
                       (loop (car lst) pool)))
                ((ellipsis? lst) pool)
                ((bar? lst) pool)
                ((variable? lst)
                 (if (id-memq lst lites)
                     pool
                     (if (memq lst pool)
                         (assertion-violation "syntax pattern" "duplicate pattern variables" pat lst)
                         (cons lst pool))))
                ((vector? lst)
                 (loop (vector->list lst) pool))
                (else pool)))))

    (define check-misplaced-ellipsis
      (lambda (pat lites)
        (let loop ((lst pat))
          (cond ((ellipsis? lst)
                 (assertion-violation "syntax pattern" "improper use of ellipsis" pat))
                ((ellipsis-pair? lst)
                 (and (variable? (car lst))
                      (id-memq (car lst) lites)
                      (assertion-violation "syntax pattern" "ellipsis following literal" pat lst))
                 (let loop ((lst (cddr lst)))
                   (and (pair? lst)
                        (if (ellipsis? (car lst))
                            (assertion-violation "syntax pattern" "ambiguous use of ellipsis" pat)
                            (loop (cdr lst))))))
                ((pair? lst)
                 (or (loop (car lst)) (loop (cdr lst))))
                ((vector? lst)
                 (loop (vector->list lst)))
                (else #f)))))

    (check-misplaced-ellipsis pat lites)
    (check-duplicate-variable pat lites)))


(define collect-vars-ranks
  (lambda (pat lites depth ranks)    
    (cond ((bar? pat) ranks)
          ((variable? pat)
           (if (id-memq pat lites)
               ranks
               (acons pat depth ranks)))
          ((ellipsis-pair? pat)
           (collect-vars-ranks (cddr pat) lites depth
                               (if (variable? (car pat))
                                   (acons (car pat) (+ depth 1) ranks)
                                   (collect-vars-ranks (car pat) lites (+ depth 1) ranks))))
          ((pair? pat)
           (collect-vars-ranks (cdr pat) lites depth
                               (collect-vars-ranks (car pat) lites depth ranks)))
          ((vector? pat)
           (collect-vars-ranks (vector->list pat) lites depth ranks))
          (else ranks))))

;; this must be in compiler.scm for global lambda let dynamic-wind
;; but for now
(define compile-syntax-case
  (lambda (exp-name expr literals rules library env p1env)
    (let ((literals (unwrap-syntax literals)))
      (define parse-pattern
	(lambda (pattern)
	  (let ((ranks   (collect-vars-ranks pattern literals 0 '())))
	    (check-pattern pattern literals)
	    (values pattern ranks
		    ;; extend p1env frame like this (SYNTAX pattern ((a . 0) ...))
		    (map (lambda (a)
			   (cons (car a)
				 (cdr a)))
			 ranks)))))

      (or (and (list? literals)
	       (for-all variable? literals))
	  (assertion-violation 'syntax-case "invalid literals" expr literals))
      (or (unique-id-list? literals)
	  (assertion-violation 'syntax-case "duplicate literals" expr literals))
      (and (memq '_ literals)
	   (assertion-violation 'syntax-case "_ in literals" expr literals))
      (and (memq '... literals)
	   (assertion-violation 'syntax-case "... in literals" expr literals))

      (letrec ((newenv '())
	       (processes (map (lambda (clause)
				 (let ((len (length clause)))
				   (if (or (= len 2) (= len 3))
				       (receive (pattern ranks env) (parse-pattern (car clause))
					 (define construct
					   (lambda (clause)
					     `(lambda (,@(map car ranks)
						       use-env . .vars)
						(let ((.ranks (append ',ranks .ranks)))
						  (let ((.save .vars.)
							(.env-save use-env))
						    (dynamic-wind
							(lambda ()
							  (set! .vars. (append .vars .vars.))
							  (set! .vars .vars.)
							  (set! .use-env use-env))
							(lambda () ,clause)
							(lambda ()
							  (set! .vars. .save)
							  (set! .use-env .env-save))))))))
					 (set! newenv (append env newenv))
					 `(list ',pattern
						',ranks
						,(if (= len 2) ; fender
						     #f
						     (construct (cadr clause)))
						,(construct (if (= len 2)
								(cadr clause)
								(caddr clause)))))
				       (assertion-violation 'syntax-case
							    "a clause must be either (<pattern> <expression) or (<pattern> <fender> <expression)"
							    clause))))
			       rules)))
	(let ((newenv (extend-env newenv env)))
	  (values newenv
		  `(.match-syntax-case ',literals
				       ,newenv
				       ,expr
				       ,@processes)))))))

(define match-ellipsis?
  (lambda (expr pat lites mac-env use-env)
    (or (null? expr)
        (and (pair? expr)
             (match-pattern? (car expr) (car pat) lites mac-env use-env)
             (match-ellipsis? (cdr expr) pat lites mac-env use-env)))))

(define match-ellipsis-n?
  (lambda (expr pat n lites mac-env use-env)
    (or (= n 0)
        (and (pair? expr)
             (match-pattern? (car expr) (car pat) lites mac-env use-env)
             (match-ellipsis-n? (cdr expr) pat (- n 1) lites mac-env use-env)))))

(define match-pattern?
  (lambda (expr pat lites mac-env use-env)
    (define (compare a b)
      (identifier=? use-env a mac-env b))
    (cond ((bar? pat) #t)
          ((variable? pat)
           (cond ((id-memq pat lites)
                  (and (variable? expr)
		       ;; if we can use compare from er-macro-transformer...
                       (compare pat expr)))
                 (else #t)))
          ((ellipsis-pair? pat)
           (if (and (null? (cddr pat)) (list? expr))
               (or (variable? (car pat))
                   (match-ellipsis? expr pat lites mac-env use-env))
               (let ((n (- (count-pair expr) (count-pair (cddr pat)))))
                 (if (= n 0)
                     (match-pattern? expr (cddr pat) lites mac-env use-env)
                     (and (> n 0)
                          (match-ellipsis-n? expr pat n lites mac-env use-env)
                          (match-pattern? (list-tail expr n) (cddr pat) lites mac-env use-env))))))
          ((pair? pat)
           (and (pair? expr)
                (match-pattern? (car expr) (car pat) lites mac-env use-env)
                (match-pattern? (cdr expr) (cdr pat) lites mac-env use-env)))
          ((vector? pat)
           (and (vector? expr)
                (match-pattern? (vector->list expr) (vector->list pat) lites mac-env use-env)))
          (else (equal? pat expr)))))

(define collect-unique-ids ; exclude '...
  (lambda (expr)
    (let loop ((lst expr) (ans '()))
      (cond ((pair? lst)
             (loop (cdr lst)
                   (loop (car lst) ans)))
            ((ellipsis? lst) ans)
            ((variable? lst)
             (if (id-memq lst ans) ans (cons lst ans)))
            ((vector? lst)
             (loop (vector->list lst) ans))
            (else ans)))))

(define union-vars
  (lambda (vars evars)
    (if (null? evars)
        vars
        (union-vars (bind-var! (caar evars) (reverse (cdar evars)) vars)
                    (cdr evars)))))

(define bind-var!
  (lambda (pat expr vars)
    (cond ((bar? pat) vars)
          (else
           (let ((slot (assq pat vars)))
             (if slot
                 (begin (set-cdr! slot (cons expr (cdr slot))) vars)
                 (acons pat (list expr) vars)))))))

(define bind-null-ellipsis
  (lambda (pat lites vars)
    (let loop ((lst (collect-unique-ids (car pat))) (vars vars))
      (if (null? lst)
          vars
          (loop (cdr lst)
                (if (memq (car lst) lites)
                    vars
                    (bind-var! (car lst) '() vars)))))))

(define bind-ellipsis
  (lambda (expr pat lites vars evars)
    (if (null? expr)
        (if (null? evars)
            (bind-null-ellipsis pat lites vars)
            (union-vars vars evars))
        (bind-ellipsis (cdr expr) pat lites vars
                       (bind-pattern (car expr) (car pat) lites evars)))))

(define bind-ellipsis-n
  (lambda (expr pat lites n vars evars)
    (if (= n 0)
        (if (null? evars)
            (bind-null-ellipsis pat lites vars)
            (union-vars vars evars))
        (bind-ellipsis-n (cdr expr) pat lites (- n 1) vars
                         (bind-pattern (car expr) (car pat) lites evars)))))


(define bind-pattern
  (lambda (expr pat lites vars)
    (cond ((variable? pat)
           (if (id-memq pat lites)
               vars
               (bind-var! pat expr vars)))
          ((ellipsis-pair? pat)
           (if (and (null? (cddr pat)) (list? expr))
               (if (variable? (car pat))
                   (bind-var! (car pat) expr vars)
                   (bind-ellipsis expr pat lites vars '()))
               (let ((n (- (count-pair expr) (count-pair (cddr pat)))))
                 (bind-pattern (list-tail expr n) (cddr pat) lites
                               (if (and (= n 0)
					(variable? (car pat)))
                                   (bind-var! (car pat) '() vars)
                                   (bind-ellipsis-n expr pat lites n vars '()))))))
          ((pair? pat)
           (bind-pattern (cdr expr) (cdr pat) lites
                         (bind-pattern (car expr) (car pat) lites vars)))
          ((vector? pat)
           (bind-pattern (vector->list expr) (vector->list pat) lites vars))
          (else vars))))


;; this needs to be toplevel but how?
(define match-syntax-case
  (lambda (literals mac-env expr . process)
    ;; TODO this might be still too naive
    (define p1env?
      (lambda (env)
	(and (vector? env)
	     (library? (vector-ref env 0)))))
    ;; direct expression from define-syntax has p1env, so we need to unwrap it,
    ;; however if expression was defined by user or something, it doesn't have
    ;; it. so we need to check if it has ir or not.
    ;; expression is (expr . (use-env . mac-env))
    (receive (form use-env)
	(if (and (pair? expr)
		 (pair? (cdr expr))
		 (p1env? (cadr expr))
		 (p1env? (cddr expr)))
	    (values #;(car expr) (wrap-syntax (car expr) (cadr expr) (make-eq-hashtable) #t)
				 (cadr expr))
	    (values expr '()))
      (define match
	(lambda (form pat)
	  (and (match-pattern? form pat literals mac-env use-env)
	       (bind-pattern form pat literals '()))))
      ;; for now I don't consider renaming
      (let loop ((lst process))
	(if (null? lst)
	    (assertion-violation (and (pair? form)
				      (car form))
				 "invalid syntax" form)
	    (let ((clause (car lst)))
	      (let ((pat (car clause))
		    (patvars (cadr clause))
		    (fender (caddr clause))
		    (expr (cadddr clause)))
		(let ((vars (match form pat)))
		  (if (and vars
			   (or (not fender)
			       (apply fender (append (map cadr vars) (list use-env) vars))))
		      (apply expr (append (map cadr vars) (list use-env) vars))
		      (loop (cdr lst)))))))))))

;; syntax
;; first compile syntax
;; when a form is given, we bind the given variables.
(define compile-syntax
  (lambda (exp-name tmpl p1env)
    (let ((ids (collect-unique-ids tmpl)))
      (let ((ranks (filter values
			   (map (lambda (id)
				  (let ((rank (p1env-pvar-lookup p1env id)))
				    ;; rank must be number
				    (and (not (variable? rank))
					 (cons id rank))))
				ids))))
	;; later
	;;(check-template tmpl ranks)
	(let ((patvar (map car ranks)))
	  (if (variable? tmpl)
	      (if (null? ranks)
		  `(.expand-syntax ',patvar ',tmpl .ranks () #f ,p1env)
		  `(.expand-syntax ',patvar ',tmpl (list (cons ',tmpl 0)) .vars use-env ,p1env)))
	  (if (null? ranks)
	      `(.expand-syntax ',patvar ',tmpl .ranks () #f ,p1env)
	      `(.expand-syntax ',patvar ',tmpl (append ',ranks .ranks) .vars use-env ,p1env)))))))

(define expand-syntax
  (lambda (patvars template ranks vars use-env p1env)
    (let ((seen (make-eq-hashtable)))
      (define emit
	(lambda (datum)
	  (cond ((identifier? datum) datum)
		((and (symbol? datum)
		      (not (null? use-env)))
		 (wrap-syntax datum use-env seen))
		(else datum))))

      (define contain-indentifier?
	(lambda (lst)
	  (let loop ((lst lst))
	    (cond ((pair? lst)
		   (or (null? (car lst))
		       (loop (car lst))
		       (loop (cdr lst))))
		  ((vector? lst)
		   (let loop2 ((i (- (vector-length lst) 1)))
		     (and (>= i 0)
			  (or (loop (vector-ref lst i))
			      (loop2 (- i 1))))))
		  (else
		   (identifier? lst))))))
      (define partial-identifier
	(lambda (lst)
	  ;; TODO this is a temporary solution
	  (define renamed-ids (make-eq-hashtable))
	  (let loop ((lst lst))
	    (cond ((contain-indentifier? lst)
		   (cond ((pair? lst)
			  (let ((a (loop (car lst))) (d (loop (cdr lst))))
			    (cond ((and (eq? (car lst) a) (eq? (cdr lst) d)) lst)
				  (else (cons a d)))))
			 ((and (identifier? lst)
			       (assq lst vars))
			  => (lambda (slot)
			       (emit (cadr slot))))
			 (else lst)))
		  ((null? lst) '())
		  ((symbol? lst)
		   (cond ((hashtable-ref renamed-ids lst #f)
			  => (lambda (id) id))
			 ((hashtable-ref seen lst #f)
			  => (lambda (id) id))
			 ((assq lst vars)
			  => (lambda (slot)
			       (emit (cadr slot))))
			 (else
			  (if (null? use-env)
			      lst
			      (wrap-syntax lst use-env renamed-ids)))))
		  ((vector? lst)
		   (list->vector (loop (vector->list lst))))
		  ((pair? lst)
		   (cons (loop (car lst))
			 (loop (cdr lst))))
		  (else lst)))))

      (if (null? template)
	  '()
	  (let ((form (transcribe-template template ranks patvars vars use-env emit)))
	    (cond ((null? form) '())
		  ((identifier? form) form)
		  ((symbol? form)
		   (wrap-syntax form p1env))
		  (else
		   (partial-identifier form))))))))

(define parse-ellipsis-splicing
    (lambda (form)
      (let loop ((len 2) (tail (cdddr form)))
	(cond ((and (pair? tail)
		    (ellipsis? (car tail)))
	       (loop (+ len 1) (cdr tail)))
	      (else
	       (values (list-head form len) tail len))))))

(define rank-of
  (lambda (name ranks)
    (let ((slot (assq name ranks)))
      (if slot (cdr slot) -1))))

(define subform-of
  (lambda (name vars)
    (cdr (assq name vars))))

(define contain-rank-moved-var?
  (lambda (tmpl ranks vars)

    (define traverse-escaped
      (lambda (lst depth)
        (let loop ((lst lst) (depth depth))
          (cond ((variable? lst)
                 (< 0 (rank-of (identifier->symbol lst) ranks) depth))
                ((pair? lst)
                 (or (loop (car lst) depth)
                     (loop (cdr lst) depth)))
                ((vector? lst)
                 (loop (vector->list lst) depth))
                (else #f)))))

    (let loop ((lst tmpl) (depth 0))
      (cond ((variable? lst)
             (< 0 (rank-of (identifier->symbol lst) ranks) depth))
            ((ellipsis-quote? lst)
             (traverse-escaped (cadr lst) depth))
            ((ellipsis-splicing-pair? lst)
             (receive (body tail len) (parse-ellipsis-splicing lst)
               (or (loop body (+ depth 1))
                   (loop tail depth))))
            ((ellipsis-pair? lst)
             (or (loop (car lst) (+ depth 1))
                 (loop (cddr lst) depth)))
            ((pair? lst)
             (or (loop (car lst) depth)
                 (loop (cdr lst) depth)))
            ((vector? lst)
             (loop (vector->list lst) depth))
            (else #f)))))


(define adapt-to-rank-moved-vars
  (lambda (form ranks vars)
    
    (define rewrite-template-ranks-vars
      (lambda (tmpl ranks vars)
        (let ((moved-ranks (make-eq-hashtable)) (moved-vars (make-eq-hashtable)))

          (define make-infinite-list
            (lambda (e)
              (let ((lst (list e)))
                (begin (set-cdr! lst lst) lst))))

          (define revealed
            (lambda (name depth)
              (if (< 0 (rank-of name ranks) depth)
                  (let ((renamed (string->symbol (format "~a:~a" (gensym) name))))
                    (or (hashtable-ref moved-ranks renamed #f)
                        (let loop ((i (- depth (rank-of name ranks))) (var (subform-of name vars)))
                          (cond ((> i 0)
                                 (loop (- i 1) (list (make-infinite-list (car var)))))
                                (else
                                 (hashtable-set! moved-ranks renamed depth)
                                 (hashtable-set! moved-vars renamed var)))))
                    renamed)
                  name)))

          (define traverse-escaped
            (lambda (lst depth)
              (let loop ((lst lst) (depth depth))
                (cond ((symbol? lst)
                       (revealed lst depth))
                      ((pair? lst)
                       (cons (loop (car lst) depth)
                             (loop (cdr lst) depth)))
                      ((vector? lst)
                       (list->vector (loop (vector->list lst) depth)))
                      (else lst)))))

          (let ((rewrited
                 (let loop ((lst tmpl) (depth 0))
                   (cond ((symbol? lst)
                          (revealed lst depth))
                         ((ellipsis-quote? lst)
                          (cons (car lst)
                                (traverse-escaped (cdr lst) depth)))
                         ((ellipsis-splicing-pair? lst)
                          (receive (body tail len) (parse-ellipsis-splicing lst)
                            (append (loop body (+ depth 1)) (cons '... (loop tail depth)))))
                         ((ellipsis-pair? lst)
                          (cons (loop (car lst) (+ depth 1))
                                (cons '... (loop (cddr lst) depth))))
                         ((pair? lst)
                          (cons (loop (car lst) depth)
                                (loop (cdr lst) depth)))
                         ((vector? lst)
                          (list->vector (loop (vector->list lst) depth)))
                         (else lst)))))
            (values rewrited
                    (append ranks (hashtable->alist moved-ranks))
                    (append vars (hashtable->alist moved-vars)))))))

    (if (contain-rank-moved-var? form ranks vars)
        (rewrite-template-ranks-vars form ranks vars)
        (values form ranks vars))))

(define collect-ellipsis-vars
  (lambda (tmpl ranks depth vars)
    (let ((ids (collect-unique-ids tmpl)))
      (filter values
              (map (lambda (slot)
                     (and (memq (car slot) ids)
                          (let ((rank (cdr (assq (car slot) ranks))))
                            (cond ((< rank depth) slot)
                                  ((null? (cdr slot)) slot)
                                  (else (cons (car slot) (cadr slot)))))))
                   vars)))))

(define consume-ellipsis-vars
  (lambda (ranks depth vars)
    (let ((exhausted #f) (consumed #f))
      ;; consumed exhausted return
      ;; #t       #t    --> #f        error, different size of matched subform
      ;; #t       #f    --> remains   more variable to reveal
      ;; #f       #t    --> #t        all variable revealed
      ;; #f       #f    --> ()        no variable revealed
      (let ((remains
             (let loop ((lst vars))
               (cond ((null? lst) lst)
                     ((< (rank-of (caar lst) ranks) depth)
                      (cons (car lst) (loop (cdr lst))))
                     ((null? (cdar lst))
                      (loop (cdr lst)))
                     ((null? (cddar lst))
                      (set! exhausted #t)
                      (loop (cdr lst)))
                     (else
                      (or (circular-list? (cdar lst)) (set! consumed #t))
                      (acons (caar lst) (cddar lst) (loop (cdr lst))))))))
        (if consumed
            (and (not exhausted) remains)
            (or exhausted '()))))))

(define transcribe-template
  (lambda (template ranks patvars vars use-env emit)
    (define remove-duplicates
      (lambda (alist)
	(or (let loop ((lst alist))
	      (cond ((null? lst) alist)
		    ((assq (caar lst) (cdr lst)) #f)
		    (else (loop (cdr lst)))))
	    (let loop ((lst alist) (acc '()))
	      (cond ((null? lst) acc)
		    ((assq (caar lst) acc)
		     (loop (cdr lst) acc))
		    (else
		     (loop (cdr lst) (cons (car lst) acc))))))))
    (let ((seen (make-eq-hashtable))
	  ;; creates dummy p1env
	  (env  (if (null? use-env) `#(,(vm-current-library) () #f #f) use-env)))
      (receive (tmpl ranks vars)
	  (adapt-to-rank-moved-vars template ranks (remove-duplicates vars))

	(define expand-var
	  (lambda (tmpl vars)
	    (cond ((assq tmpl vars)
		   => (lambda (slot)
			(cond ((null? (cdr slot)) '())
			      ;; if we don't share this with syntax-rules
			      ;; we don't have check emit.
			      (emit (emit (cadr slot)))
			      (else (cadr slot)))))
		  (else
		   ;; we don't have any way to detect right pattern and template for now,
		   ;; so ranks may contains invalid pattern variable name, in that case
		   ;; we just return template variable as a result.
		   tmpl
		   #;(assertion-violation "syntax template"
		   "subforms have different size of matched input"
		   `(template: ,template) `(subforms: ,@vars))))))
	(define expand-ellipsis-var
	  (lambda (tmpl vars)
	    (cond ((assq tmpl vars)
		   => (lambda (slot)
			(cond ((null? (cdr slot)) '())
			      ;; if we don't share this with syntax-rules
			      ;; we don't have check emit.
			      (emit (map emit (cadr slot)))
			      (else (cadr slot)))))
		  (else
		   (assertion-violation "syntax template"
					"subforms have different size of matched input"
					`(template: ,template) `(subforms: ,@vars))))))

	(define expand-ellipsis-template
	  (lambda (tmpl depth vars)
	    (let loop ((expr '()) (remains (collect-ellipsis-vars tmpl ranks depth vars)))
	      (cond ((pair? remains)
		     (loop (cons (expand-template tmpl depth remains) expr)
			   (consume-ellipsis-vars ranks depth remains)))
		    ((null? remains) '())
		    ((eq? remains #t) (reverse expr))
		    (else
		     (assertion-violation "syntax template"
					  "subforms have different size of matched input"
					  `(template: ,template) `(subforms: ,@vars)))))))

	(define expand-escaped-template
	  (lambda (tmpl depth vars)
	    (cond ((variable? tmpl)
		   (if (< (rank-of tmpl ranks) 0)
		       tmpl
		       (expand-var tmpl vars)))
		  ((pair? tmpl)
		   (if (and emit (null? (car tmpl)))
		       (cons '()
			     (expand-escaped-template (cdr tmpl) depth vars))
		       (cons (expand-escaped-template (car tmpl) depth vars)
			     (expand-escaped-template (cdr tmpl) depth vars))))
		  ((vector? tmpl)
		   (list->vector (expand-escaped-template (vector->list tmpl) depth vars)))
		  (else tmpl))))

	(define expand-template
	  (lambda (tmpl depth vars)
	    (cond ((variable? tmpl)
		   (if (< (rank-of tmpl ranks) 0)
		       (wrap-syntax tmpl env seen)
		       (expand-var tmpl vars)))
		  ((ellipsis-quote? tmpl)
		   (expand-escaped-template (cadr tmpl) depth vars))
		  ((ellipsis-splicing-pair? tmpl)
		   (receive (body tail len) (parse-ellipsis-splicing tmpl)
		     (append (apply append (expand-ellipsis-template body (+ depth 1) vars))
			     (expand-template tail depth vars))))
		  ((ellipsis-pair? tmpl)
		   (cond ((variable? (car tmpl))
			  (let lp ((rank (rank-of (car tmpl) ranks))
				   (rest (cdr ranks)))
			    (cond ((= rank (+ depth 1))
				   (append (expand-ellipsis-var (car tmpl) vars)
					   (expand-template (cddr tmpl) depth vars)))
				  ((>= rank 0)
				   ;; it might be free pattern variable retry
				   (lp (rank-of (car tmpl) rest)
				       (cdr rest)))
				  (else
				   ;; error?
				   (assertion-violation "syntax template"
							"missing ellipsis"
							template)))))
			 ((pair? (car tmpl))
			  (append (expand-ellipsis-template (car tmpl) (+ depth 1) vars)
				  (expand-template (cddr tmpl) depth vars)))))
		  ((pair? tmpl)
		   (if (and emit (null? (car tmpl)))
		       (cons '()
			     (expand-template (cdr tmpl) depth vars))
		       (cons (expand-template (car tmpl) depth vars)
			     (expand-template (cdr tmpl) depth vars))))
		  ((vector? tmpl)
		   (list->vector (expand-template (vector->list tmpl) depth vars)))
		  (else tmpl))))
	(if (and (= (safe-length tmpl) 2) (ellipsis? (car tmpl)))
	    (expand-escaped-template (cadr tmpl) 0 vars)
	    (expand-template tmpl 0 vars))))))

;; procedures
(define syntax->datum
  (lambda (expr)
    (let loop ((lst expr))
      (cond ((pair? lst)
	     (let ((a (loop (car lst))) (d (loop (cdr lst))))
	       (cond ((and (eq? a (car lst)) (eq? d (cdr lst))) lst)
		     (else (cons a d)))))
	    ((vector? lst)
	     (list->vector (map loop (vector->list lst))))
	    ((identifier? lst)
	     (id-name lst))
	    (else lst)))))

(define datum->syntax
  (lambda (template-id datum)
    (or (identifier? template-id)
	(assertion-violation 'datum->syntax
			     (format "expected identifier, but got ~s" template-id)))
    ;; construct p1env
    (let ((p1env `#(,(id-library template-id)
		    ,(id-envs template-id)
		    'datum->syntax
		    #f)))
      (wrap-syntax datum p1env))))

(define generate-temporaries
  (lambda (obj)
    (or (list? obj)
        (assertion-violation 'generate-temporaries
			     (format "expected list, but got ~s" obj)))
    (map (lambda (n) (make-identifier (gensym) '() (vm-current-library))) obj)))

(define (make-variable-transformer proc)
  (make-macro 'variable-transformer
	      (lambda (m expr p1env data)
		(proc (wrap-syntax expr p1env)))
	      '()
	      ;; TODO it might be wrong.
	      `#(,(vm-current-library) () #f #f)))

(define .transcribe-syntax-rules
  (lambda (form use-env mac-env spec)

    (define transcribe-compiled-templete
      (lambda (spec vars)
        (let ((template (car spec))
              (ranks    (cadr spec)))
	  (transcribe-template template ranks '() vars use-env #f))))

    (or (pair? form)
        (assertion-violation form "misplaced syntactic keyword" form))

    (let ((lites (car spec))
	  (rules (cdr spec)))
      (let loop ((rules rules))
        (if (null? rules)
	    (assertion-violation (car form) "invalid syntax" form)
            (let* ((rule (car rules))
                   (pattern (car rule))
                   (vars (and (match-pattern? form pattern lites mac-env use-env)
                              (bind-pattern form pattern lites '()))))
              (if vars
                  (transcribe-compiled-templete (cdr rule) vars)
                  (loop (cdr rules)))))))))

(define parse-syntax-rule
  (lambda (lites clause)
    (let ((pattern (car clause)) (template (cadr clause)))
      (check-pattern pattern lites)
      (let ((ranks (collect-vars-ranks pattern lites 0 '())))
        ;;(check-template template ranks)
        (values pattern template ranks)))))

(define compile-syntax-rules
  (lambda (form lites clauses env)
    (let ((lites (unwrap-syntax lites))
	  (clauses (unwrap-syntax clauses)))
      `(lambda (x)
	 (.transcribe-syntax-rules (car x) (cadr x) (cddr x)
	  ',(cons lites
		  (map (lambda (clause)
			 (receive (pattern template ranks) (parse-syntax-rule lites clause)
			   (list pattern template ranks)))
		       clauses)))))))


;; toplevel variables
(set-toplevel-variable! '.match-syntax-case match-syntax-case)
(set-toplevel-variable! '.expand-syntax expand-syntax)
(set-toplevel-variable! '.ranks '())
(set-toplevel-variable! '.vars. '())
(set-toplevel-variable! '.make-variable-transformer make-variable-transformer)
(set-toplevel-variable! '.count-pair count-pair)
(set-toplevel-variable! '.use-env #f)
(set-toplevel-variable! '.transcribe-syntax-rules .transcribe-syntax-rules)

;;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
