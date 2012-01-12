(add-load-path "./lib")
(add-load-path "./sitelib")
(import (srfi :0)
	(srfi :37)
	(rnrs)
	(sagittarius))

(let ((args (command-line)))
  (let-values (((test) (args-fold (cdr args)
				  '()
				  (lambda (option name arg . seed)
				    (assertion-violation 'run-test
							 "Unrecognized option"
							 name))
				  (lambda (operand test)
				    (values (cons operand test)))
				  '())))
    (define (r6rs-test)
      ;; for R6RS test suites
      (print "testing R6RS test suite")
      (flush-output-port (current-output-port))
      (add-load-path "./test/r6rs-test-suite")
      (load "./test/r6rs-test-suite/tests/r6rs/run.sps")
      (flush-output-port (current-output-port)))
    (define (r7rs-test)
      ;; for R7RS test
      ;; prepare for process-context
      (setenv "R7RS_TEST" "OK")
      (print "testing R7RS tests")
      (flush-output-port (current-output-port))
      (add-load-path "./test/r7rs-tests")
      (load "./test/r7rs-tests/tests/r7rs/run.scm")
      (flush-output-port (current-output-port)))

    (define (sitelib-test)
      ;; for sitelib
      (print "testing sitelib")
      (flush-output-port (current-output-port))
      (add-load-path "./test")
      (add-load-path "./ext/regex")
      (cond-expand
       (sagittarius.os.windows
	(add-dynamic-load-path "./build/modules"))
       (else
	(add-dynamic-load-path "./build")))
      (load "./test/tests.scm")
      (flush-output-port (current-output-port)))
    (define (ext-test)
      ;; for extensions
      (print "testing extensions")
      (flush-output-port (current-output-port))
      (set-current-directory "ext")
      (add-load-path "../lib")
      (add-load-path "../sitelib")
      (cond-expand
       (sagittarius.os.windows
	;; all-tests adds dynamic-load-path however it's for non windows environment
	(add-dynamic-load-path "../build/modules"))
       (else
	#t))
      (load "./all-tests.scm")
      (set-current-directory "..")
      (flush-output-port (current-output-port)))

    (if (null? test)
	(begin
	  (r6rs-test)
	  (r7rs-test)
	  (sitelib-test)
	  (ext-test))
	(for-each (lambda (test)
		    (case (string->symbol test)
		      ((r6rs) (r6rs-test))
		      ((r7rs) (r7rs-test))
		      ((sitelib) (sitelib-test))
		      ((ext) (ext-test))
		      (else
		       (error 'run-test
			      "unknown test" test))))
		  (reverse! test)))))
	  