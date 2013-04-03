;;; -*- Scheme -*-
;;;
;;; signature.scm - OAuth 1.0 library.
;;;  
;;;   Copyright (c) 2010-2012  Takashi Kato  <ktakashi@ymail.com>
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

(library (net oauth signature)
    (export signature-base-string
	    oauth-signature)
    (import (rnrs)
	    (net oauth misc)
	    (net oauth parameters)
	    (net oauth request-adapter)
	    (math)
	    (rfc hmac)
	    (rfc base64))

  ;; signature
  ;; for now we only support consumer so no default
  (define (signature-base-string :key (uri (request-uri))
				      (request-method (request-method))
				      (parameters (normalized-parameters)))
    ;; assume request-method is symbol
    (string-append (string-upcase (symbol->string request-method))
		   "&" (oauth-uri-encode (normalize-uri uri))
		   "&" (oauth-uri-encode
			(oauth-compose-query parameters))))

  ;; hash
  (define (oauth-signature method sbs consumer-secret
			   :optional (token-secret ""))
    (utf8->string
      (base64-encode
       (case method
	 ((:hmac-sha1)
	  (hash (hash-algorithm 
		 HMAC
		 :key (string->utf8 (string-append consumer-secret
						   "&"
						   (or token-secret
						       ""))))
		(string->utf8 sbs)))))))
  )