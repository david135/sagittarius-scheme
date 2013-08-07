;; -*- scheme -*-
(add-load-path "./socket")
(add-load-path "./threads")

(import (srfi :64 testing)
	(srfi :13 strings)
	(rnrs)
	(srfi :0 cond-expand)
	(clos user)
	(sagittarius socket)
	;; use thread for testing
	(sagittarius threads))

(define echo-server-socket (make-server-socket "5000"))

;; addr is client socket
(define (server-run)
  (let ((addr (socket-accept echo-server-socket)))
    (call-with-socket
     addr
     (lambda (sock)
       (let ((p (transcoded-port (socket-port sock)
				 (native-transcoder))))
	 (call-with-port
	  p
	  (lambda (p)
	    (let lp2 ((r (get-line p)))
	      (if (or (not (string? r)) (string=? r "test-end"))
		  (put-string p "")
		  (let ((res (string->utf8 (string-append r "\r\n"))))
		    (when (string=? r "wait")
		      ;; wait one sec
		      (thread-sleep! 1000))
		    (put-bytevector p res 0 (bytevector-length res) #t)
		    (lp2 (get-line p))))))))))))

(define server-thread (make-thread server-run))

(test-begin "(run-socket-test)")
;; start echo server
(thread-start! server-thread)

(test-error "ai-passive" assertion-violation?
	    (make-client-socket #f "5000" 0 0 AI_PASSIVE))

(let ((client-socket (make-client-socket "localhost" "5000")))
  (test-assert "socket?"(socket? client-socket))
  (test-equal "raw socket-send"
	      (+ (string-length "hello") 2) ;; for \r\n
	      (socket-send client-socket (string->utf8 "hello\r\n") 0))
  (test-equal "raw socket-recv"
	      (string->utf8 "hello\r\n")
	      (socket-recv client-socket (+ (string-length "hello") 2) 0))

  ;; make port
  (let ((port (socket-port client-socket)))
    (test-assert "port?" (port? port))
    (test-assert "binary-port?" (binary-port? port))
    (test-assert "input-port?" (input-port? port))
    (test-assert "output-port?" (output-port? port))

    (put-bytevector port (string->utf8 "put from port\r\n"))
    (test-equal "get-bytevector-n"
		(string->utf8 "put from port\r\n")
		(get-bytevector-n port
				  (string-length "put from port\r\n")))
    ;; textual
    (let ((text-port (transcoded-port port
				      (make-transcoder (utf-8-codec)
						       'crlf))))
      (put-string text-port "put from text port\r\n")
      (test-equal "get-line" "put from text port" (get-line text-port))
      ;; end test
      (put-string text-port "test-end\r\n")
      )))

(let ((client-socket (make-client-socket "localhost" "5000")))
  (socket-nonblocking! client-socket)
  (test-equal "raw nonblocking socket-send"
	      (+ (string-length "wait") 2)
	      (socket-send client-socket (string->utf8 "wait\r\n") 0))
  (test-equal "raw nonblocking socket-recv"
	      #f
	      (socket-recv client-socket (+ (string-length "hello\r\n") 2) 0))
  (socket-send client-socket (string->utf8 "test-end\r\n") 0)
  )

(thread-join! server-thread)

;; addr info slots
(let ((info (get-addrinfo "localhost" "5000" (make-hint-addrinfo
					      :family AF_INET
					      :socktype SOCK_DGRAM))))
  (test-assert "sockaddr?" (sockaddr? (addrinfo-sockaddr info)))
  (let ((s (make-socket AF_INET SOCK_DGRAM)))
    (test-equal "seocket-sendto" 4 
		(socket-sendto s #vu8(1 2 3 4) (addrinfo-sockaddr info)))))


(test-end)