(define myfunc
  (lambda ()
    (let ((path '())
	  (c #f))
      (let ((add (lambda (s)
		   (set! path (cons s path)))))
	(dynamic-wind
	    (lambda () (display '1)(newline))
	    (lambda () (car 'a))
	    (lambda () (display '3)(newline)))
	(if (< (length path) 6)
	    (c '-1)
	    (reverse path))))))
(let ()
  (display (myfunc))(newline))
