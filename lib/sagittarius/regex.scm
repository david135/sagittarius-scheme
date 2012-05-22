;;; -*- Scheme -*-
;;;
;;; regex.scm - regular expression library
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

(library (sagittarius regex)
    (export compile-regex
	    regex-matcher
	    regex-matches
	    regex-find
	    regex-looking-at
	    regex-group
	    regex-capture-count
	    ;; pred
	    regex-pattern?
	    regex-matcher?
	    ;; accessor
	    regex-before
	    regex-after
	    regex-first
	    regex-last

	    ;; flags
	    CASE-INSENSITIVE
	    COMMENTS
	    MULTILINE
	    LITERAL
	    DOTALL
	    UNICODE-CASE

	    
	    ;; syntax-sugar
	    regex

	    ;; wrapper APIs
	    matches
	    looking-at

	    ;; modify
	    regex-replace-all
	    regex-replace-first

	    ;; utility
	    string-split

	    ;; clos
	    <pattern> <matcher>
	    ;; enable #/regex/
	    :export-reader-macro
	    )
    (import (rename (sagittarius regex impl) 
		    (regex-replace-first impl:regex-replace-first)
		    (regex-replace-all impl:regex-replace-all))
	    (core)
	    (core base)
	    (core errors)
	    (sagittarius)
	    (clos user))

  (define regex compile-regex)

  ;; complete match
  (define (matches reg text)
    (let ((matcher (regex-matcher reg text)))
      (if (regex-matches matcher)
	  matcher
	  #f)))

  (define (looking-at reg text)
    (let ((matcher (regex-matcher reg text)))
      (if (regex-looking-at matcher)
	  matcher
	  #f)))

  ;; since version 0.3.3, we have object-apply and
  ;; CLOS libraries are in lib.
  (define-method object-apply ((self <pattern>) (s <string>))
    (looking-at self s))
  (define-method object-apply ((self <matcher>) (group <integer>))
    (regex-group self group))
  (define-method object-apply ((self <matcher>) (group <symbol>))
    (case group
      ((before)
       (regex-before self))
      ((after)
       (regex-after self))
      (else
       (assertion-violation 
		    'matcher
		    (format "'after or 'before required but got ~a" group)
		    group))))
  
  ;; for convenience, we wrap
  (define (regex-replace-all reg text replacement)
    (let ((matcher (regex-matcher reg text)))
      (impl:regex-replace-all matcher replacement)))

  (define (regex-replace-first reg text replacement)
    (let ((matcher (regex-matcher reg text)))
      (impl:regex-replace-first matcher replacement)))

  (define (string-split text str/pattern)
    (let* ((p (cond ((regex-pattern? str/pattern) str/pattern)
		    ((string? str/pattern) (regex str/pattern))
		    ((char? str/pattern) (regex (list->string  
						 (list str/pattern))))
		    (else (assertion-violation
			   'string-split
			   "string or regex-pattern required" str/pattern))))
	   (m (regex-matcher p text)))
      (let loop ((r '())
		 (pos 0))
	(cond ((regex-find m)
	       (let ((first (regex-first m))
		     (last  (regex-last m)))
		 (loop (cons (substring text pos first) r)
		       last)))
	      (else (reverse! (cons (substring text pos (string-length text))
				    r)))))
      ))
  
)