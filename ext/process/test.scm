(add-load-path "./threads/")
(add-load-path "./process/")
(import (rnrs)
	(sagittarius process)
	(srfi :64 testing))

(test-begin "process tests")

(define-constant *process-name* "test-proc.bin")

(let ((proc (make-process *process-name* '("process"))))
  (test-assert "process?" (process? proc))
  ;; some how this doesn't work, why?
  ;; on debugger it works... i don't get it.
  (cond-expand
   (windows
    ;; for windows, we need to invoke the process otherwise it will remain
    ;; and become zonbie process.
    (process-run proc)
    )
   (else #t))
  #;(let ((r (process-run proc)))
    (test-equal "process-run" 0 r)
    (let* ((out (process-output-port proc))
	   (r (get-line (transcoded-port out (native-transcoder)))))
      (test-equal "output from process" "process" r))))

(test-end)