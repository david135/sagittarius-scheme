;;
#!compatible
(library (math prime)
    (export is-prime?
	    random-prime
	    lucas-lehmer?)
    (import (rnrs)
	    (sagittarius control)
	    (sagittarius)
	    (math helper)
	    (math random))

  (define *small-primes*
    '(3 5 7 11 13 17 19 23 29 31 37 41 43 
      47 53 59 61 67 71 73 79 83 89 97 101 
      103 107 109 113
      127 131 137 139 149 151 157 163 167 173
      179 181 191 193 197 199 211 223 227 229
      233 239 241 251 257 263 269 271 277 281
      283 293 307 311 313 317 331 337 347 349
      353 359 367 373 379 383 389 397 401 409
      419 421 431 433 439 443 449 457 461 463
      467 479 487 491 499 503 509 521 523 541
      547 557 563 569 571 577 587 593 599 601
      607 613 617 619 631 641 643 647 653 659
      661 673 677 683 691 701 709 719 727 733
      739 743 751 757 761 769 773 787 797 809
      811 821 823 827 829 839 853 857 859 863
      877 881 883 887 907 911 919 929 937 941
      947 953 967 971 977 983 991 997))

  ;; Lucas-Lehmer prime test
  (define (lucas-lehmer? n)
    ;; might be too naive?
    ;; if we need to use bignum directory for performance
    ;; we need to write this in C. might be sagittarius-math.c?
    (define (jacobi-symbol p n)
      (define (neg-pow e) (if (zero? (mod e 2)) -1 1))
      (cond ((zero? p)(if (= n 1) 1 0))
	    ((zero? (mod p 2))
	     (* (neg-pow (div (* (- p 1) (- n 1)) 4))
		(jacobi-symbol (mod n p) p)))
	    (else (* (neg-pow (div (- (* n n) 1) 8))
		     (jacobi-symbol (div p 2) n)))))
    (define (lucas-lehmer-sequence z k n)
      (do ((i (- (bitwise-length k) 2) (- i 1)) (u 1) (v 1))
	  ((zero? i) u)
	(let ((u2 (mod (* u v) n))
	      (v2 (mod (+ (* v v) (* d (* u u))) n)))
	  (when (bitwise-bit-set? v2 0)
	    (set! v2 (- v2 n)))
	  (set! v2 (bitwise-arithmetic-shift-right v2 1))
	  (set! u u2)
	  (set! v v2)
	  (when (bitwise-bit-set? k i)
	    (set! u2 (mod (+ u v) n))
	    (when (bitwise-bit-set? u2 0)
	      (set! u2 (- u2 n)))
	    (set! u2 (bitwise-arithmetic-shift-right u2 1))
	    (set! v2 (mod (+ v (* d u)) n))
	    (when (bitwise-bit-set? v2 0)
	      (set! v2 (- v2 n)))
	    (set! v2 (bitwise-arithmetic-shift-right v2 1))
	    (set! u u2)
	    (set! v v2)))))
    (define (step1 n)
      (do ((d 5 (if (< d 0) (+ (abs d) 2) (- (+ d 2)))))
	  ((= (jacobi-symbol d n) -1)) d))
    (let* ((d (step1 n))
	   (u (lucas-lehmer-sequence d (+ n 1) n)))
      (zero? (mod u n))))

  (define (miller-rabin? q k rand)
    (define (check-small-prime q)
      (let loop ((p *small-primes*))
	(cond ((null? p) #f)
	      ((zero? (mod q (car p))) #t)
	      (else
	       (loop (cdr p))))))
    (let ((q (abs q)))
      (cond ((even? q) #f) ;; obvious
	    ((= q 1) #f)   ;; 1 is not prime
	    ((memv q *small-primes*) #t)
	    ((check-small-prime q) #f) ;; multiple of small-primes
	    (else
	     ;; Miller Rabin test
	     (let* ((t (- q 1))
		    (d (if (zero? (bitwise-and t 1))
			   (do ((d (bitwise-arithmetic-shift-right t 1)
				   (bitwise-arithmetic-shift-right d 1)))
			       ((not (zero? (bitwise-and d 1))) d))
			   t)))
	       (let loop ((i 0))
		 (if (= i k)
		     #t
		     (let* ((a (+ (random rand (- q 2)) 1))
			    (t d)
			    (y (mod-expt a t q)))
		       ;; check 0, ..., q - 1
		       (let loop2 ()
			 (when (and (not (= t (- q 1)))
				    (not (= y 1))
				    (not (= y (- q 1))))
			   (set! y (mod (* y y) q))
			   (set! t (bitwise-arithmetic-shift-left t 1))
			   (loop2)))
		       (if (and (not (= y (- q 1)))
				(zero? (bitwise-and t 1)))
			   #f
			   (loop (+ i 1)))))))))))

  ;; Miller Rabin primality test
  (define (is-prime? q :optional (k 50) (rand (secure-random RC4)))
    #|
    (define bit-size (bitwise-length q))
    (define try-count (cond ((< bit-size 100)  50)
			    ((< bit-size 256)  27)
			    ((< bit-size 512)  15)
			    ((< bit-size 768)  8)
			    ((< bit-size 1024) 4)
			    (else              2)))
    (let ((n (min try-count k)))
      (and (miller-rabin? q n rand)
	   (or (= try-count 50)
	       (lucas-lehmer? q))))
    |#
    (miller-rabin? q k rand))

  (define (random-prime size :key (prng (secure-random RC4)))
    (let ((buf (make-bytevector size 0))
	  (index (- size 1)))
      (let loop ()
	(let* ((bv (read-random-bytes! prng buf size)))
	  (bytevector-u8-set!
	   bv 0 (bitwise-ior (bytevector-u8-ref bv 0) #x80 #x40))
	  (let ((b (bitwise-ior (bytevector-u8-ref bv index) #x01)))
	    (cond ((zero? (mod b 5)) (loop)) ;; ignore multiple number of 5
		  (else
		   (bytevector-u8-set! bv index b)
		   (let ((ret (bytevector->integer bv)))
		     (if (is-prime? ret)
			 ret
			 (loop))))))))))
  )