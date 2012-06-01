(import (rnrs)
	(clos user)
	(net oauth)
	(net oauth signature) ;; for oauth-signature
	(net oauth parameters);; for *signature-cache*
	(srfi :19 time)
	(srfi :27 random-bits)
	(srfi :39 parameters)
	(srfi :64 testing))
(define *request-object* (make-parameter #f))
(define *request-method* (make-parameter 'GET))
(define *request-uri*    (make-parameter "http://host.domain/path"))
(define *auth-parameters* (make-parameter '()))
(define *post-parameters* (make-parameter '()))
(define *get-parameters* (make-parameter '()))


(define (make-test-request-adapter)
  (make-request-adapter 
   :request-object (lambda ()
		     (or (*request-object*)(random-integer (greatest-fixnum))))
   :request-method (lambda (r) (*request-method*))
   :request-uri (lambda (r) (*request-uri*))
   :auth-parameters (lambda (r) (*auth-parameters*))
   :post-parameters (lambda (r) (*post-parameters*))
   :get-parameters (lambda (r) (*get-parameters*))))
(define (init-test-request-adapter)
  (init-request-adapter (make-test-request-adapter)))

(define *default-token* (make-request-token))

(init-test-request-adapter)

(define-syntax with-signed-request
  (lambda (x)
    (syntax-case x ()
      ((_ (specs ...) body ...)
       (let-keywords* #'(specs ...)
	   ((user-parameters '())
	    (version "1.0")
	    (timestamp (time-second (current-time)))
	    (nonce (random-integer (greatest-fixnum)))
	    (signature-override #f)
	    (signature-method "HMAC-SHA1")
	    (consumer-token (make-consumer-token))
	    (token *default-token*)
	    (verification-code #f))
       #`(begin
	   (register-token #,consumer-token)
	   (when #,token
	     (token-consumer #,token #,consumer-token)
	     (register-token #,token))
	   (parameterize ((*request-object* (random-integer (greatest-fixnum)))
			  (*request-method* 'GET)
			  (*request-uri* "/foo"))
	     (let* ((parameters (append #,user-parameters
					(list (list "oauth_version" #,version)
					      (list "oauth_signature_method" #,signature-method)
					      (list "oauth_consumer_key" (token-key #,consumer-token))
					      (list "oauth_timestamp" (number->string #,timestamp))
					      (list "oauth_nonce" (number->string #,nonce)))
					(if #,token
					    (list (list "oauth_token" (token-key #,token)))
					    '())
					(if (and #,token (is-a? #,token <request-token>))
					    (list (list "oauth_verifier" (or #,verification-code
									     (request-token-verification-code #,token))))
					    '())))
		    (signature (or #,signature-override
				   (oauth-signature :hmac-sha1
						    (signature-base-string
						     :parameters (sort-parameters parameters))
						    (token-secret #,consumer-token)
						    (if #,token 
							(token-secret #,token)
							"")))))
	       (parameterize ((*get-parameters* 
			       (cons (list "oauth_signature" signature)
				     parameters)))
		 (weak-hashtable-set! (*signature-cache*) (request) signature)
		 body ...)))))))))

(test-begin "OAuth tests")

(test-assert "check-version (valid)"
	     (parameterize ((*get-parameters* '(("oauth_version" "1.0"))))
	       (check-version)))

(test-error "check-version (invalid)" &bad-request
	    (parameterize ((*get-parameters* '(("oauth_version" "foo"))))
	       (check-version)))

(test-error "check-signature (invalid-method)" &bad-request
	    (with-signed-request 
	     (:signature-method "foo")
	     (check-signature)))
(test-error "check-signature (invalid)" &unauthorised
	    (with-signed-request
	     (:signature-override "haha")
	     (check-signature)))

(test-assert "check-signature (valid)"
	     (with-signed-request 
	      ()
	      (check-signature)))

(test-assert "check-signature (valid2)"
	     (with-signed-request 
	      ()
	      (check-signature)))

;; high-level API
;; phase 1
(test-assert "validate-request-token-request (oob)"
	     (with-signed-request
	      (:user-parameters '(("oauth_callback"  "oob")))
	      (is-a? (validate-request-token-request :allow-oob-callback? #t)
		     <request-token>)))

(test-error "validate-request-token-request (oob-disallowed)" &bad-request
	    (with-signed-request
	     (:user-parameters '(("oauth_callback"  "oob")))
	     (validate-request-token-request :allow-oob-callback? #f)))

(test-end)
