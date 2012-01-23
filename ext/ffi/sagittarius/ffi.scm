;; -*- mode: scheme; coding: utf-8; -*-
(load-dynamic-library "sagittarius--ffi")
(library (sagittarius ffi)
    (export open-shared-library
	    lookup-shared-library
	    close-shared-library
	    c-function
	    pointer->c-function

	    c-callback
	    free-c-callback
	    ;; malloc
	    c-malloc
	    c-free
	    ;; pointer
	    pointer?
	    integer->pointer
	    pointer->integer
	    allocate-pointer

	    ;; c-struct
	    define-c-struct
	    allocate-c-struct
	    size-of-c-struct
	    c-struct-ref
	    c-struct-set!

	    ;; typedef
	    define-c-typedef
	    ;; sizes
	    size-of-bool
	    size-of-char
	    size-of-short
	    size-of-unsigned-short
	    size-of-int
	    size-of-unsigned-int
	    size-of-long
	    size-of-unsigned-long
	    size-of-long-long
	    size-of-unsigned-long-long
	    size-of-void*
	    size-of-size_t
	    size-of-float
	    size-of-double
	    size-of-int8_t
	    size-of-int16_t
	    size-of-int32_t
	    size-of-int64_t
	    size-of-uint8_t
	    size-of-uint16_t
	    size-of-uint32_t
	    size-of-uint64_t
	    size-of-intptr_t
	    size-of-uintptr_t

	    ;; ref
	    pointer-ref-c-uint8
	    pointer-ref-c-int8
	    pointer-ref-c-uint16
	    pointer-ref-c-int16
	    pointer-ref-c-uint32
	    pointer-ref-c-int32
	    pointer-ref-c-uint64
	    pointer-ref-c-int64
	    pointer-ref-c-unsigned-char
	    pointer-ref-c-char
	    pointer-ref-c-unsigned-short
	    pointer-ref-c-short
	    pointer-ref-c-unsigned-int
	    pointer-ref-c-int
	    pointer-ref-c-unsigned-long
	    pointer-ref-c-long
	    pointer-ref-c-unsigned-long-long
	    pointer-ref-c-long-long
	    pointer-ref-c-float
	    pointer-ref-c-double
	    ;; set!
	    pointer-set-c-uint8!
	    pointer-set-c-int8!
	    pointer-set-c-uint16!
	    pointer-set-c-int16!
	    pointer-set-c-uint32!
	    pointer-set-c-int32!
	    pointer-set-c-uint64!
	    pointer-set-c-int64!
	    pointer-set-c-unsigned-char!
	    pointer-set-c-char!
	    pointer-set-c-unsigned-short!
	    pointer-set-c-short!
	    pointer-set-c-unsigned-int!
	    pointer-set-c-int!
	    pointer-set-c-unsigned-long!
	    pointer-set-c-long!
	    pointer-set-c-unsigned-long-long!
	    pointer-set-c-long-long!
	    pointer-set-c-float!
	    pointer-set-c-double!
	    ;; alignment
	    align-of-bool
	    align-of-char
	    align-of-short
	    align-of-unsigned-short
	    align-of-int
	    align-of-unsigned-int
	    align-of-long
	    align-of-unsigned-long
	    align-of-long-long
	    align-of-unsigned-long-long
	    align-of-void*
	    align-of-size_t
	    align-of-float
	    align-of-double
	    align-of-int8_t
	    align-of-int16_t
	    align-of-int32_t
	    align-of-int64_t
	    align-of-uint8_t
	    align-of-uint16_t
	    align-of-uint32_t
	    align-of-uint64_t
	    align-of-intptr_t
	    align-of-uintptr_t
	    ;; c-primitives
	    void
	    char short int long unsigned-short unsigned-int unsigned-long int8_t int16_t int32_t uint8_t uint16_t uint32_t size_t
	    int64_t uint64_t long-long unsigned-long-long
	    bool void* char* float double callback struct
	    ;; utility
	    null-pointer
	    null-pointer?
	    )
    (import (core)
	    (core base)
	    (core errors)
	    (core syntax)
	    (core misc)
	    (sagittarius)
	    (sagittarius vm)
	    (sagittarius ffi impl))

  (define void               'void)
  (define char               'char)
  (define short		     'short)
  (define int		     'int)
  (define long		     'long)
  (define unsigned-short     'unsigned-short)
  (define unsigned-int	     'unsigned-int)
  (define unsigned-long	     'unsigned-long)
  (define int8_t	     'int8_t)
  (define int16_t	     'int16_t)
  (define int32_t	     'int32_t)
  (define uint8_t	     'uint8_t)
  (define uint16_t	     'uint16_t)
  (define uint32_t	     'uint32_t)
  (define size_t	     'size_t)
  (define int64_t	     'int64_t)
  (define uint64_t	     'uint64_t)
  (define long-long	     'long-long)
  (define unsigned-long-long 'unsigned-long-long)
  (define bool		     'bool)
  (define void*		     'void*)
  (define char*		     'char*)
  (define float		     'float)
  (define double	     'double)
  (define callback           'callback)
  (define struct             'struct)

  (define null-pointer (integer->pointer 0))
  (define (null-pointer? p)
    (and (pointer? p)
	 (= (pointer->integer p) 0)))

  (define-syntax define-c-typedef
    (syntax-rules (* s*)      
      ((_ old (* new) rest ...)
       (begin
	 (define new void*)
	 (define-c-typedef old rest ...)))
      ((_ old (s* new) rest ...)
       (begin
	 (define new char*)
	 (define-c-typedef old rest ...)))
      ((_ old new rest ...)
       (begin
	 (define new old)
	 (define-c-typedef old rest ...)))
      ((_ old)
       #t)))

  (define (pointer->c-function pointer ret-type name arg-types)
    (let* ((stub-ret-type (assoc ret-type c-function-return-type-alist))
	   (signatures (list->string (make-sigunatures arg-types)))
	   (function (create-function-info pointer (cdr stub-ret-type) signatures
					   (car stub-ret-type) arg-types)))
      (unless stub-ret-type
	(assertion-violation 'c-function "wrong return type" ret-type))
      (lambda args
	(unless (= (length arg-types) (length args))
	  (assertion-violation name (format "wrong arguments number ~d required, but got ~d"
					    (length arg-types)
					    (length args)) args))
	(apply %ffi-call
	       (cdr stub-ret-type)
	       function
	       args))))

  (define (make-sigunatures arg-types)
    (map (lambda (arg-type)
	   (case arg-type
	     ((char short int long unsigned-short int8_t int16_t int32_t uint8_t uint16_t)
	      #\i)
	     ((unsigned-int unsigned-long uint32_t size_t)
	      #\u)
	     ((int64_t long-long)
	      #\x)
	     ((uint64_t unsigned-long-long)
	      #\U)
	     ((bool) #\b)
	     ((void* char*) #\p)
	     ((float) #\f)
	     ((double) #\d)
	     ((callback) #\c)
	     (else (assertion-violation 'make-sigunatures "invalid argument type" arg-type))))
       arg-types))

  (define-syntax c-function
    (lambda (x)
      (syntax-case x ()
	((_ lib ret func (args ...))
	 #'(make-c-function lib ret 'func (list args ...))))))

  (define (make-c-function lib ret-type name arg-types)
    (let ((func (lookup-shared-library lib (symbol->string name))))
      (unless func
	(assertion-violation 'c-function "c-function not found" name))
      (pointer->c-function func ret-type name arg-types)))


  ;; callback
  (define (make-callback-signature name ret args)
    (apply string
	   (map (lambda (a)
		  (cond ((assq a callback-argument-type-class) => cdr)
			(else (assertion-violation name (format "invalid argument type ~a" a)
						   (list ret args)))))
		args)))

  (define-syntax c-callback
    (lambda (x)
      (syntax-case x ()
	((_ ret (args ...) proc)
	 #'(make-c-callback ret (list args ...) proc)))))

  (define (make-c-callback ret args proc)
    (cond ((assq ret c-function-return-type-alist)
	   => (lambda (type)
		(create-c-callback (cdr type)
				 (make-callback-signature 'make-c-callback ret args)
				 proc)))
	  (else
	   (assertion-violation 'make-c-callback (format "invalid return type ~a" ret)
				(list ret args proc)))))

  ;; c-struct
  (define (make-c-struct name defs)
    (let ((layouts (map (lambda (def)
			  (cond ((and (eq? 'struct (car def))
				      (= (length def) 3))
				 `(,(caddr def) -1 struct . ,(cadr def)))
				((eq? 'callback (car def))
				 ;; speciall case
				 `(,(cadr def), #x16 . callback))
				((and (eq? 'array (cadr def))
				      (= (length def) 4)
				      (assq (car def) c-function-return-type-alist))
				 => (lambda (type)
				      `(,(cadddr def) ,(cdr type) ,(caddr def) . ,(car type))))
				((assq (car def) c-function-return-type-alist)
				 => (lambda (type)
				      `(,(cadr def) ,(cdr type) . ,(car type))))
				(else
				 (assertion-violation 'make-c-struct
						      (format "invalid struct declaration ~a" def)
						      (list name defs)))))
			defs)))
      (unless (unique-id-list? (map car layouts))
	(assertion-violation 'make-c-struct
			     "struct declaration contains duplicated member name"
			     (list name defs)))
      (create-c-struct name layouts)))

  ;; (define-c-struct name (int x) (int y) (struct st s))
  (define-syntax define-c-struct
    (lambda (x)
      (syntax-case x ()
	((_ name (type . rest) ...)
	 ;; black magic ...
	 #'(begin
	     ;; if there are more than one struct in the same library,
	     ;; and if one of them refere it, it cause unbound variable error.
	     ;; to avoid it, we need to do this. ugly...
	     (%insert-binding (vm-current-library)
			      'name
			      (make-c-struct 'name (map cons (list type ...) '(rest ...))))
	     (define name (make-c-struct 'name (map cons (list type ...) '(rest ...)))))))))

  (define c-function-return-type-alist
    '((void               . #x00)    ; FFI_RETURN_TYPE_VOID
      (bool               . #x01)    ; FFI_RETURN_TYPE_BOOL
      (char               . #x0c)    ; FFI_RETURN_TYPE_INT8_T
      (short              . #x02)    ; FFI_RETURN_TYPE_SHORT
      (int                . #x03)    ; FFI_RETURN_TYPE_INT
      (long               . #x04)    ; FFI_RETURN_TYPE_INTPTR
      (long-long          . #x12)    ; FFI_RETURN_TYPE_INT64_T
      (unsigned-short     . #x05)    ; FFI_RETURN_TYPE_USHORT
      (unsigned-int       . #x06)    ; FFI_RETURN_TYPE_UINT
      (unsigned-long      . #x07)    ; FFI_RETURN_TYPE_UINTPTR
      (unsigned-long-long . #x13)    ; FFI_RETURN_TYPE_UINT64_T
      (float              . #x08)    ; FFI_RETURN_TYPE_FLOAT
      (double             . #x09)    ; FFI_RETURN_TYPE_DOUBLE
      (void*              . #x14)    ; FFI_RETURN_TYPE_POINTER
      (char*              . #x0a)    ; FFI_RETURN_TYPE_STRING
      (size_t             . #x0b)    ; FFI_RETURN_TYPE_SIZE_T
      (int8_t             . #x0c)    ; FFI_RETURN_TYPE_INT8_T
      (uint8_t            . #x0d)    ; FFI_RETURN_TYPE_UINT8_T
      (int16_t            . #x0e)    ; FFI_RETURN_TYPE_INT16_T
      (uint16_t           . #x0f)    ; FFI_RETURN_TYPE_UINT16_T
      (int32_t            . #x10)    ; FFI_RETURN_TYPE_INT32_T
      (uint32_t           . #x11)    ; FFI_RETURN_TYPE_UINT32_T
      (int64_t            . #x12)    ; FFI_RETURN_TYPE_INT64_T
      (uint64_t           . #x13)))  ; FFI_RETURN_TYPE_UINT64_T

  (define callback-argument-type-class
    `((bool               . #\l)
      (char               . #\b)
      (short              . #\h)
      (int                . ,(if (= size-of-int 4) #\w #\q))
      (long               . ,(if (= size-of-long 4) #\w #\q))
      (long-long          . #\q)
      (unsigned-char      . #\B)
      (unsigned-short     . #\H)
      (unsigned-int       . ,(if (= size-of-int 4) #\W #\Q))
      (unsigned-long      . ,(if (= size-of-long 4) #\W #\Q))
      (unsigned-long-long . #\Q)
      (int8_t             . #\b)
      (int16_t            . #\h)
      (int32_t            . #\w)
      (int64_t            . #\Q)
      (uint8_t            . #\B)
      (uint16_t           . #\H)
      (uint32_t           . #\W)
      (uint64_t           . #\Q)
      (float              . #\f)
      (double             . #\d)
      (size_t             . ,(if (= size-of-size_t 4) #\W #\Q))
      (void*              . #\p)))
)