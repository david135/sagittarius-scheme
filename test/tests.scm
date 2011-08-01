;; -*- scheme -*-
(import (srfi :64)
	(tests r6rs+ vector)
	(tests r6rs+ string)
	(tests r6rs+ lists)
	(tests srfi :13)
	(tests srfi :14)
	(tests rfc base64)
	(tests rfc quoted-printable)
	(tests rfc mime))

(test-begin "sagittarius functionality tests start")
(run-r6rs+-vector-tests)
(run-r6rs+-string-tests)
(run-r6rs+-lists-tests)
(run-srfi-13-tests)
(run-srfi-14-tests)
(run-rfc-base64-tests)
(run-rfc-quoted-printable-tests)
(run-rfc-mime-test)
(test-end)