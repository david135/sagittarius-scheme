;; -*- Scheme -*-

(cond-expand
 (sagittarius
  (import (rnrs)))
 (else #t))

(define c-file   "../src/builtin-symbols.c")
(define c-header "../src/sagittarius/builtin-symbols.h")

(define (header out)
  (format out "/* This file is automatically generated by builtin-symbols.scm. DO NOT EDIT! */~%")
  (format out "#ifndef BUILTIN_SYMBOL_H__~%")
  (format out "#define BUILTIN_SYMBOL_H__~%")
  (format out "SG_EXTERN SgSymbol Sg_BuiltinSymbols[];~%")
  (let loop ((symbols symbols)
	     (i 0))
    (unless (null? symbols)
      (let ((p (car symbols)))
	(let ((def (cadr p)))
	  (format out "#define ~s SG_OBJ(&Sg_BuiltinSymbols[~a])~%" def i)))
      (loop (cdr symbols) (+ i 1))))
  (format out "#endif /* BUILTIN_SYMBOL_H__ */~%"))

(define (body out)
  (format out "/* This file is automatically generated by builtin-symbols.scm. DO NOT EDIT! */~%")
  (format out "#define LIBSAGITTARIUS_BODY~%")
  (format out "#include <sagittarius.h>~%")
  (format out "SgSymbol Sg_BuiltinSymbols[] = {~%")
  (format out "#define ENTRY() {{SG_CLASS2TAG(SG_CLASS_SYMBOL)}, NULL, SG_SYMBOL_INTERNED}~%")
  (let loop ((symbols symbols))
    (unless (null? symbols)
      (format out "  ENTRY(),~%")
      (loop (cdr symbols))))
  (format out "#undef ENTRY~%")
  (format out "};~%")
  (format out "static void init_builtin_symbols()~%")
  (format out "{~%")
  (format out "#define STRING(s)    Sg_MakeString(UC(s), SG_LITERAL_STRING)~%")
  (format out "#define INTERN(s, i) \\~%")
  (format out "  Sg_BuiltinSymbols[i].name = STRING(s); \\~%")
  (format out "  Sg_HashTableSet(obtable, Sg_BuiltinSymbols[i].name, SG_OBJ(&Sg_BuiltinSymbols[i]), 0)~%")
  (let loop ((symbols symbols)
	     (i 0))
    (unless (null? symbols)
      (format out "  INTERN(\"~a\", ~a);~%" (caar symbols) i)
      (loop (cdr symbols) (+ i 1))))
  (format out "#undef INTERN~%")
  (format out "}~%"))
  
(define (generate file-out header-out)
  (header header-out)
  (body   file-out))

(define (main args)
  (cond-expand
   (sagittarius
    (print "generating builtin symbold")
    (if (file-exists? c-header)
	(delete-file c-header))
    (if (file-exists? c-file)
	(delete-file c-file)))
   (else #t))
  (let ((c-file-out (open-output-file c-file))
	(c-header-out (open-output-file c-header)))
    (generate c-file-out c-header-out)
    (close-output-port c-file-out)
    (close-output-port c-header-out)))

(define symbols
  '((quote             	  SG_SYMBOL_QUOTE)
    (quasiquote        	  SG_SYMBOL_QUASIQUOTE)
    (unquote           	  SG_SYMBOL_UNQUOTE)
    (unquote-splicing  	  SG_SYMBOL_UNQUOTE_SPLICING)
    (syntax               SG_SYMBOL_SYNTAX)
    (quasisyntax          SG_SYMBOL_QUASISYNTAX)
    (unsyntax             SG_SYMBOL_UNSYNTAX)
    (unsyntax-splicing    SG_SYMBOL_UNSYNTAX_SPLICING)
    (define    	       	  SG_SYMBOL_DEFINE)
    (define-constant   	  SG_SYMBOL_DEFINE_CONSTANT)
    (let       	       	  SG_SYMBOL_LET)
    (let*      	       	  SG_SYMBOL_LET_STAR)
    (letrec    	       	  SG_SYMBOL_LETREC)
    (letrec*   	       	  SG_SYMBOL_LETREC_STAR)
    (set!    	       	  SG_SYMBOL_SET)
    (begin     	       	  SG_SYMBOL_BEGIN)
    (|.|       	       	  SG_SYMBOL_DOT)
    (|(|                  SG_SYMBOL_LPAREN)
    (|)|                  SG_SYMBOL_RPAREN)
    (|[|                  SG_SYMBOL_LBRACK)
    (|]|                  SG_SYMBOL_RBRACK)
    (if                	  SG_SYMBOL_IF)
    (and               	  SG_SYMBOL_AND)
    (or                	  SG_SYMBOL_OR)
    (cond              	  SG_SYMBOL_COND)
    (=>                	  SG_SYMBOL_YIELDS)
    (do                	  SG_SYMBOL_DO)
    (while             	  SG_SYMBOL_WHILE)
    (unless            	  SG_SYMBOL_UNLESS)
    (when              	  SG_SYMBOL_WHEN)
    (receive           	  SG_SYMBOL_RECEIVE)
    (library           	  SG_SYMBOL_LIBRARY)
    (import            	  SG_SYMBOL_IMPORT)
    (export            	  SG_SYMBOL_EXPORT)
    (for               	  SG_SYMBOL_FOR)
    (only              	  SG_SYMBOL_ONLY)
    (rename            	  SG_SYMBOL_RENAME)
    (prefix            	  SG_SYMBOL_PREFIX)
    (expand            	  SG_SYMBOL_EXPAND)
    (run               	  SG_SYMBOL_RUN)
    (define-syntax     	  SG_SYMBOL_DEFINE_SYNTAX)
    (let-syntax        	  SG_SYMBOL_LET_SYNTAX)
    (letrec-syntax     	  SG_SYMBOL_LETREC_SYNTAX)
    (syntax-rules      	  SG_SYMBOL_SYNTAX_RULES)
    (er-macro-transformer SG_SYMBOL_ER_MACRO_TRANSFORMER)
    (...                  SG_SYMBOL_ELLIPSIS)
    (%macroexpand-1       SG_SYMBOL_MACROEXPAND_1)
    (%macroexpand         SG_SYMBOL_MACROEXPAND)
    (file-options         SG_SYMBOL_FILE_OPTIONS) ; not so smart...
    (record-type-descriptor SG_SYMBOL_RTD)
    (record-constructor-descriptor SG_SYMBOL_RCD)
    (replace              SG_SYMBOL_REPLACE)
    (ignore               SG_SYMBOL_IGNORE)
    (raise                SG_SYMBOL_RAISE)
    (source-info          SG_SYMBOL_SOURCE_INFO)
    (constant             SG_SYMBOL_CONSTANT)
    ;; add later
    ))

;;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
