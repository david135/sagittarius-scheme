(cond-expand
 (gauche
  (use srfi-13)
  )
 (sagittarius
  (import (srfi :13) (srfi :6)
	  (rename (sagittarius) (interned-symbol? symbol-interned?))))
 )
;(load "../insn.scm")
(define (library-name->string lib delim)
  (let ((s (string-map (lambda (c)
			 (cond ((char=? c #\space) delim)
			       ((or (char=? c #\()
				    (char=? c #\))) #\space)
			       ((char=? c #\-) delim)
			       (else c))) lib)))
    (string-trim-both s)))
(define (library-name->path lib)
  (library-name->string lib #\.))

(define (write-macro out)
  (format out "/* This file is automatically generated. DO NOT EDIT!*/~%")
  (format out "#define LIBSAGITTARIUS_BODY~%")
  (format out "#include \"sagittarius.h\"~%")
  (format out "#define WORD(x)       SG_WORD(x)~%")
  (format out "#define SYMBOL(x)     Sg_Intern(Sg_MakeString(UC(x), SG_LITERAL_STRING))~%")
  (format out "#define SYMBOLW(x)    WORD(SYMBOL(x))~%")
  (format out "#define UISYMBOL(x)   Sg_MakeSymbol(Sg_MakeString(UC(x), SG_LITERAL_STRING), FALSE)~%")
  (format out "#define UISYMBOLW(x)  WORD(UISYMBOL(x))~%")
  (format out "#define STRING(x)     Sg_MakeString(UC(x), SG_LITERAL_STRING)~%")
  (format out "#define STRINGW(x)    WORD(STRING(x))~%")
  (format out "#define KEYWORD(x)    Sg_MakeKeyword(STRING(x))~%")
  (format out "#define KEYWORDW(x)   WORD(KEYWORD(x))~%")
  (format out "#define IDENT(x, lib) WORD(Sg_MakeIdentifier(SYMBOL(x), SG_NIL, (lib)))~%")
  (format out "#define UNSIDENT(x, lib) WORD(Sg_MakeIdentifier(UISYMBOL(x), SG_NIL, (lib)))~%"))

(define (code-make-pair p . word?)
  (let loop ((p p)
	     (top (if (null? word?)
		      #t
		      (car word?))))
    (cond ((null? p) (format "SG_NIL"))
	  ((symbol? p)
	   (if (symbol-interned? p)
	       (format "SYMBOL(~s)" (symbol->string p))
	       (format "UISYMBOL(~s)" (symbol->string p))))
	  ((string? p)
	   (format "STRING(~s)" p))
	  ((boolean? p)
	   (format "SG_MAKE_BOOL(~s)" (if p 'TRUE 'FALSE)))
	  ((number? p)
	   (format "SG_MAKE_INT(~s)" p))
	  ((keyword? p)
	   (format "KEYWORD(\"~a\")" p))
	  ((pair? p)
	   (let ((r
		  (format "Sg_Cons(~a, ~a)"
			  (loop (car p) #f)
			  (loop (cdr p) #f))))
	     (if top
		 (string-append "WORD(" r ")")
		 r)))
	  ((char? p)
	   (format "SG_MAKE_CHAR(~a)" (char->integer p)))
	  (else
	   (error "unknown scheme object" p)))))

(define (write-object out o line)
  (format out "  /* ~5d */" line)
  (cond ((pair? o)
	 (format out "        WORD(SG_UNDEF), /* ~s */~%" o))
	((symbol? o)
	 (format out "        WORD(SG_UNDEF), /* ~s */~%" (decode-start-comment o)))
	((number? o)
	 ;; assume there is no bignum, ratnum, flonum or complex.
	 (format out "        WORD(SG_MAKE_INT(~a)),~%" o))
	((string? o)
	 (format out "        WORD(SG_UNDEF), /* ~s */~%" o))
	((boolean? o)
	 (format out "        WORD(SG_MAKE_BOOL(~s)),~%" (if o 'TRUE 'FALSE)))
	((null? o)
	 (format out "        WORD(SG_NIL),~%"))
	((char? o)
	 (format out "        WORD(SG_MAKE_CHAR(~a)),~%" (char->integer o)))
	((undefined? o)
	 (format out "        WORD(SG_UNDEF)  /* undef */,~%"))
	((identifier? o)
	 (format out "        WORD(SG_UNDEF)  /* identifier#~s */,~%" (decode-start-comment (id-name o))))
	((keyword? o)
	 ;; I think i will add keyword for future
	 (format out "        WORD(SG_UNDEF)  /* <keyrowd ~s> */,~%" o))
	((code-builder? o)
	 (format out "        WORD(SG_UNDEF)  /* <code-builder> */,~%" #;(code-builder-name o)))
	((library? o)
	 (format out "        WORD(SG_UNDEF)  /* <library ~s> */,~%" (library-name o)))
	(else
	 (close-output-port out)
	 (error "Unknown scheme object:" o))))

(define (write-insn out insn i info line)
  (format out "  /* ~5d */" line)
  (if (negative? insn)
      (format out "        -0x~8'0X          /* ~6a (~s) */,~%" (abs insn) i (car info))
      (format out "        0x~8'0X           /* ~6a (~s) */,~%" (abs insn) i (car info))))

(define (decode-start-comment sym)
  (if (symbol? sym)
      (let* ((s (string-copy (symbol->string sym)))
	     (len (string-length s)))
	(let loop ((i 0))
	  (unless (= i len)
	    (cond ((and (char=? (string-ref s i) #\/)
			(not (>= (+ i 1) len))
			(char=? (string-ref s (+ i 1)) #\*))
		   (string-set! s (+ i 1) #\@))
		  ((and (char=? (string-ref s i) #\*)
			(not (>= (+ i 1) len))
			(char=? (string-ref s (+ i 1)) #\/))
		   (string-set! s (+ i 1) #\@)))
	    (loop (+ i 1))))
	(string->symbol s))
      sym))


(define (object? o)
  (or (identifier? o)
      (pair? o)
      (symbol? o)
      (string? o)
      (keyword? o)
      (library? o)
      (code-builder? o)))

;; calculate length of total cb without src info
(define (pre-resolve-code-builder cb alist objects)
  (define (mark-objects cb)
    (let* ((code (array-data (code-builder-code cb)))
	   (len  (array-length (code-builder-code cb))))
      (let loop ((i 0))
	(if (= i len)
	    #t
	    (let ((v (vector-ref code i)))
	      (if (object? v)
		  (hashtable-set! objects v 'mark))
	      (if (code-builder? v)
		  (mark-objects v))
	      (loop (+ i 1)))))))

  (mark-objects cb)
  (let ((code (array-data (code-builder-code cb)))
	(len  (array-length (code-builder-code cb))))
    (let loop ((i 0)
	       (ret 0))
      (if (= i len)
	  ret
	  (let* ((insn (vector-ref code i))
		 (info (lookup-insn-name (get-insn insn))))
	    (let ((name (car info))
		  (argc (caddr info)))
	      (when (eq? name 'CLOSURE)
		(let* ((cb2 (vector-ref code (+ i 1)))
		       (start (+ i 1 ret)))
		  (unless (assq cb2 (vector-ref alist 0))
					;((cb index))
		    (vector-set! alist 0 (append (vector-ref alist 0) (list (cons cb2 (cons start len))))))
		  (let ((len (pre-resolve-code-builder cb2 alist objects)))
		    (cond ((assq cb2 (vector-ref alist 0))
			   => (lambda (cb-info)
				(let ((s/l (cdr cb-info)))
				  (set-cdr! s/l len)))))
		    (set! ret (+ ret len)))))
	      (loop (+ i 1 argc)
		    (+ ret 1 argc))))))))

(define for-each1-with-index
  (lambda (proc lst)
    (let loop ((i 0)
	       (lst lst))
      (cond ((null? lst) '())
	    (else
	     (proc i (car lst))
	     (loop (+ i 1) (cdr lst)))))))

(define (resolve-code-builder out root-cb cb-list objects)
  (define (write-cb cb line)
    (let ((code (array-data (code-builder-code cb)))
	  (clen (array-length (code-builder-code cb))))
      #;(format out "/* ~s */~%" (if (identifier? (code-builder-name cb))
				   (id-name (code-builder-name cb))
				   (code-builder-name cb)))
      (let lp ((i 0))
	(if (= i clen)
	    i
	    (let* ((insn (vector-ref code i))
		   (info (lookup-insn-name (get-insn insn)))
		   (name (car info)))
	      (write-insn out insn i info (+ i line))
	      (let arg ((j 0))
		(unless (= j (caddr info))
		  (let ((obj (vector-ref code (+ i 1 j))))
		    ;; memorize its index
		    (when (object? obj)
		      (let ((c (hashtable-ref objects obj #f)))
			(if (eq? c 'mark)
			    (hashtable-set! objects obj (list (+ i 1 j line))) ; fist one
			    (hashtable-set! objects obj (append c (list (+ i 1 j line))))))) ; duplicated (such as symbol)
		    (write-object out obj (+ i 1 j line))
		    (arg (+ j 1)))))
	      (lp (+ i 1 (caddr info))))))))
  (let ((offset (write-cb root-cb 0)))
    (for-each1-with-index (lambda (index lst)
			    (let ((cb    (car lst)))
			      (let ((off (write-cb cb offset)))
				(set-cdr! lst index)
				(set! offset (+ offset off)))))
			  cb-list)))
	  
(define (write-code-builders out root-cb cb-list)
  (let ((offset (array-length (code-builder-code root-cb))))
    (define (write-cb cb)
      (let ((code (array-data (code-builder-code cb)))
	    (clen (array-length (code-builder-code cb))))
	(format out "    SG_STATIC_CODE_BUILDER(&sg__wc.w[~a], NULL, ~s, ~s, ~s, ~s, ~s), /* ~s */~%"
		offset
		(code-builder-argc cb)
		(if (code-builder-optional? cb) 'TRUE 'FALSE)
		(code-builder-freec cb)
		(code-builder-maxstack cb)
		(array-length (code-builder-code cb))
		(decode-start-comment
		 (if (identifier? (code-builder-name cb))
		     (id-name (code-builder-name cb))
		     (code-builder-name cb))))
	(set! offset (+ offset (array-length (code-builder-code cb))))))
    (for-each (lambda (lst)
		(let ((cb (car lst)))
		  (write-cb cb)))
	      cb-list)))

(define (resolve-import out lib-list)
  ;; assume only once is enough
  (for-each (lambda (lib)
	      (for-each 
	       (lambda (import)
		 (unless (library-transient (car import))
		   (format out "  Sg_ImportLibrary(lib, SG_OBJ(SYMBOL(~s)));~%"
			   (format "~s" (library-name (car import))))))
	       (library-imported lib))
	      (format out "  Sg_LibraryExportedSet(lib, ~a);~%"
		      (code-make-pair (library-exported lib) #f)))
	    lib-list))

(define (resolve-collected-id out ids proc)
  (for-each (lambda (alist)
	      (display " " out)
	      (let loop ((pos (cdr alist)))
		(unless (null? pos)
		  (format out " sg__wc.w[~a] =" (car pos))
		  (loop (cdr pos))))
	      (let ((id (car alist)))
		(proc id)
		))
	    ids))
	      

(define (code->c cb libname out)
  (write-macro out)
  (let* ((cb-info (vector '()))		; child cb's
	 (objects (make-eq-hashtable))
	 (len (pre-resolve-code-builder cb cb-info objects))
	 (toplevel-name (format "toplevel_~s" (string->symbol (library-name->string libname #\_))))
	 (libname-p (read (open-input-string libname))) ;; convert to s-exp
	 #;(imported (map car (library-imported (find-library libname-p #f)))))
    (format out "static struct sg__wcRec {~%")
    (format out "  SgCodeBuilder cb[~a];~%" (+ 1 (length (vector-ref cb-info 0))))
    (format out "  SgWord        w[~a];~%" len)
    (format out "} sg__wc = {~%")
    (format out "  { /* code builder */~%")
    (write-code-builders out cb (vector-ref cb-info 0))
    (format out "  },~%")
    (format out "  { /* compiled code */~%")
    (resolve-code-builder out cb (vector-ref cb-info 0) objects)
    (format out "  }~%")
    (format out "};~%")

    (format out "~%")
    (format out "static SgCodeBuilder ~a = SG_STATIC_CODE_BUILDER(&sg__wc.w[0], SG_FALSE, 0, FALSE, 0, ~a, ~a);~%"
	    toplevel-name
	    (code-builder-maxstack cb)
	    (array-length (code-builder-code cb)))

    (let* ((name (string->symbol (string-append "Sg__Init_" (library-name->string libname #\_))))
	   (keys (hashtable-keys-list objects))
	   (cb-alist (vector-ref cb-info 0))
	   (lib-info '())
	   ;; alist of ids ((id pos ...) ...)
	   (collected-ids '())
	   (collected-syms '()))
      (format out "void ~s()~%" name)
      (format out "{~%")
      (format out "  SgLibrary *lib = Sg_FindLibrary(SYMBOL(~s), TRUE);~%" libname)
      (format out "  SgLibrary *save = Sg_VM()->currentLibrary;~%")
      ;;(format out "  SgObject  uninterned = SG_NIL;~%")
      ;;(format out "  SgWord    tmp = WORD(SG_FALSE);~%")
      (let loop ((keys keys))
	(unless (null? keys)
	  (let* ((object (car keys))
		 (pos-list    (hashtable-ref objects object #f))) ; should not be #f
	    (for-each (lambda (pos)
			(cond ((identifier? object)
			       (when (and (not (memq (id-library object) lib-info))
					  (or (library? (id-library object))
					      #;(and (print (id-name object)) #f))
					  (equal? (library-name (id-library object)) libname-p))
				 (set! lib-info (cons (id-library object) lib-info)))
			       (cond ((assq (id-name object) collected-ids)
				      => (lambda (slot)
					   (set-cdr! slot (cons pos (cdr slot)))))
				     (else
				      (set! collected-ids (acons (id-name object) (list pos) collected-ids)))))
			      ((symbol? object)
			       (cond ((assq object collected-syms)
				      => (lambda (slot)
					   (set-cdr! slot (cons pos (cdr slot)))))
				     (else
				      (set! collected-syms (acons object (list pos) collected-syms)))))
			      (else
			       ;; the rest we can ignore for now.
			       (format out "  sg__wc.w[~a] = " pos)
			       (cond 
				#;((symbol? object)
				 (format out "SYMBOLW(~s);~%" (symbol->string object)))
				((string? object)
				 (format out "STRINGW(~s);~%" object))
				((keyword? object)
				 (format out "KEYWORDW(\"~a\");~%" object))
				((pair? object)
				 (format out "~a;~%" (code-make-pair object)))
				((library? object)
				 (format out "SYMBOLW(\"~s\");~%" (library-name object)))
				((code-builder? object)
				 (let ((info (assq object cb-alist)))
				   (let ((name (code-builder-name object)))
				     (format out "WORD(&sg__wc.cb[~a]);~%" (cdr info))
				     (format out "  sg__wc.cb[~a].name = ~a;~%" (cdr info)
					     (cond ((identifier?  name)
						    (format "SYMBOL(~s)" (symbol->string (id-name name))))
						   ((symbol? name)
						    (format "SYMBOL(~s)" (symbol->string name)))
						   ((boolean? name)
						    (format "SG_MAKE_BOOL(~s)" (if name 'TRUE 'FALSE)))
						   (else
						    (format "SYMBOL(\"~a\")" name)))))))))))
		      pos-list)
	    (loop (cdr keys)))))
      (resolve-collected-id out collected-ids 
			    (lambda (id) 
			      (if (symbol-interned? id)
				  (format out " IDENT(\"~a\", lib);~%" id)
				  (format out " UNSIDENT(\"~a\", lib);~%" id))))
      (resolve-collected-id out collected-syms
			    (lambda (id) 
			      (if (symbol-interned? id)
				  (format out " SYMBOLW(\"~a\");~%" id)
				  (format out " UISYMBOL(\"~a\");~%" id))))
      (resolve-import out lib-info)
      (format out "  Sg_VM()->currentLibrary = lib;~%")
      (format out "  Sg_VMExecute(SG_OBJ(&~a));~%" toplevel-name)
      (format out "  Sg_VM()->currentLibrary = save;~%")
      (format out "}~%"))))
;;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End
