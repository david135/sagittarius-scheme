;;; -*- Scheme -*-
;;;
;;; types.scm - ASN.1 basic types
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


;; For now we just implemented only the part which RKCS#1 v1.5 encode requires.
(library (asn.1 types)
    (export
     TAG_BOOLEAN
     TAG_INTEGER
     TAG_BIT_STRING
     TAG_OCTET_STRING
     TAG_NULL
     TAG_OBJECT_IDENTIFIER
     TAG_OBJECT_DESCRIPTOR
     TAG_EXTERNAL
     TAG_REAL
     TAG_ENUMERATED
     TAG_UTF8_STRING
     TAG_RELATIVE_OID
     TAG_SEQUENCE
     TAG_SET
     TAG_NUMERIC_STRING
     TAG_PRINTABLE_STRING
     TAG_TELETEX_STRING
     TAG_VIDEOTEX_STRING
     TAG_IA5_STRING
     TAG_UTC_TIME
     TAG_GENERALIZED_TIME
     TAG_GRAPHIC_STRING
     TAG_VISIBLE_STRING
     TAG_GENERAL_STRING
     TAG_CHARACTER_STRING
     TAG_BMP_STRING

     CLASS_UNIVERSAL
     CLASS_APPLICATION
     CLASS_CONTEXT
     CLASS_PRIVATE

     TAG_PRIMITIVE
     TAG_CONSTRUCTIVE

     ;; type
     make-asn.1-type
     asn.1-type-name        asn.1-type-name-set!
     asn.1-type-tag	    asn.1-type-tag-set!
     asn.1-type-type	    asn.1-type-type-set!
     asn.1-type-child	    asn.1-type-child-set!
     asn.1-type-loop	    asn.1-type-loop-set!
     asn.1-type-optional    asn.1-type-optional-set!
     asn.1-type-defined-by  asn.1-type-defined-by-set!
     ;; util
     tag-explicit!
     tag-constructive!
     encode-tag
     ;; condition
     &asn.1-error asn.1-error?
     )
    (import (rnrs)
	    (sagittarius)
	    (sagittarius control))

  ;; ASN.1 tag values
  (define TAG_BOOLEAN		#x01)
  (define TAG_INTEGER		#x02)
  (define TAG_BIT_STRING	#x03)
  (define TAG_OCTET_STRING	#x04)
  (define TAG_NULL		#x05)
  (define TAG_OBJECT_IDENTIFIER	#x06)
  (define TAG_OBJECT_DESCRIPTOR #x07)
  (define TAG_EXTERNAL		#x08)
  (define TAG_REAL		#x09)
  (define TAG_ENUMERATED	#x0a)
  (define TAG_UTF8_STRING	#x0c)
  (define TAG_RELATIVE_OID	#x0d)
  (define TAG_SEQUENCE		#x10)
  (define TAG_SET		#x11)
  (define TAG_NUMERIC_STRING	#x12)
  (define TAG_PRINTABLE_STRING	#x13)
  (define TAG_TELETEX_STRING	#x14)
  (define TAG_VIDEOTEX_STRING	#x15)
  (define TAG_IA5_STRING	#x16)
  (define TAG_UTC_TIME		#x17)
  (define TAG_GENERALIZED_TIME	#x18)
  (define TAG_GRAPHIC_STRING	#x19)
  (define TAG_VISIBLE_STRING	#x1a)
  (define TAG_GENERAL_STRING	#x1b)
  (define TAG_CHARACTER_STRING	#x1c)
  (define TAG_BMP_STRING	#x1e)

  ;; ASN.1 tag classes
  (define CLASS_UNIVERSAL	#x00)
  (define CLASS_APPLICATION	#x40)
  (define CLASS_CONTEXT		#x80)
  (define CLASS_PRIVATE		#xc0)

  ;; primitive or constructive
  (define TAG_PRIMITIVE		#x00)
  (define TAG_CONSTRUCTIVE	#x20)

  (define-record-type asn.1-type
    (fields (mutable name)
	    (mutable tag)
	    (mutable type)
	    (mutable child)
	    (mutable loop)
	    (mutable optional)
	    (mutable defined-by))
    (protocol (lambda (p)
		(lambda args
		  (let-keywords args ((name #f)
				      (tag -1)
				      (type #f)
				      (child #f)
				      (defined-by #f)
				      (loop #f)
				      (optional #f))
		    (p name tag type child loop optional defined-by))))))

  (define (tag-explicit! type)
    (let ((new-op (make-asn.1-type :type (asn.1-type-type type)
				   :child (asn.1-type-child type)
				   :loop (asn.1-type-loop type)
				   :optional #f)))
      (asn.1-type-type-set! type "SEQUENCE")
      (asn.1-type-child-set! type (list new-op))
      (asn.1-type-loop-set! type #f)
      type))

  (define (tag-constructive! self)
    (when (and (asn.1-type-tag self)
	       (zero? (bitwise-and (asn.1-type-tag self)
				   TAG_CONSTRUCTIVE)))
      (asn.1-type-tag-set! self (bitwise-ior (asn.1-type-tag self)
					     TAG_CONSTRUCTIVE)))
    self)

  (define (encode-tag tag-class val)
    (define classes `(,CLASS_UNIVERSAL ,CLASS_APPLICATION ,CLASS_CONTEXT ,CLASS_PRIVATE))
    (unless (memv tag-class classes)
      (raise-asn.1-error 'encode-tag "Bad tag class" (number->string tag-class 16)))
    (unless (zero? (bitwise-and val #xffe00000))
      (raise-asn.1-error 'encode-tag "Tag value too big" (number->string val 16)))
    ;; bits: tttt tttt vvvv vvvv vvvv vvvv vvvv vvvv
    ;; t: class tag
    ;; v: value
    ;; for now we don't do any thing about encoding just merge
    ;; tag class is 1 byte
    (bitwise-and (bitwise-arithmetic-shift-left tag-class 24)
		 val))
	

  (define-condition-type &asn.1-error &error
    make-asn.1-error asn.1-error?)

  (define (raise-asn.1-error who message . irritants)
    (raise
     (apply condition
	    (filter values
		    (make-asn.1-error)
		    (and who (make-who-condition who))
		    (make-message-condition message)
		    (make-irritants-condition irritants)))))

)

;; Local Variables:
;; coding: utf-8
;; End: