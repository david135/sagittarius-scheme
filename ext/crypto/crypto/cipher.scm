;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; cipher.scm Cryptographic library
;;; 
(library (crypto cipher)
    (export crypto-object?
	    cipher-keysize
	    cipher
	    cipher?
	    encrypt
	    decrypt
	    ;; padder
	    pkcs5-padder
	    ;; supported algorithms
	    Blowfish
	    X-Tea
	    RC2
	    RC5-32/12/b
	    RC6-32/20/b
	    SAFER+
	    SAFER-K64
	    SAFER-SK64
	    SAFER-K128
	    SAFER-SK128
	    AES
	    Twofish
	    DES
	    DES3
	    DESede
	    CAST5
	    CAST-128
	    Noekeon
	    Skipjack
	    Khazad
	    SEED
	    KASUMI
	    ;; supported modes
	    MODE_ECB
	    MODE_CBC
	    MODE_CFB
	    MODE_OFB
	    MODE_CTR
	    ;; ctr conter mode
	    CTR_COUNTER_LITTLE_ENDIAN
	    CTR_COUNTER_BIG_ENDIAN
	    LTC_CTR_RFC3686)
    (import (core)
	    (core base)
	    (sagittarius)
	    (sagittarius control)
	    (sagittarius crypto))

  ;; symmetric types
  (define Blowfish    "blowfish")
  (define X-Tea       "xtea")
  (define RC2         "rc2")
  (define RC5-32/12/b "rc5")
  (define RC6-32/20/b "rc6")
  (define SAFER+      "safer+")
  (define SAFER-K64   "safer-k64")
  (define SAFER-SK64  "safer-sk64")
  (define SAFER-K128  "safer-k128")
  (define SAFER-SK128 "safer-sk128")
  (define AES         "aes")
  (define Twofish     "twofish")
  (define DES         "des")
  (define DES3        "3des")
  (define DESede      DES3)
  (define CAST5       "cast5")
  (define CAST-128    CAST5)
  (define Noekeon     "noekeon")
  (define Skipjack    "skipjack")
  (define Khazad      "khazad")
  (define SEED        "seed")
  (define KASUMI      "kasumi")

  (define *symmetric-types*
    '("blowfish" "xtea" "rc2" "rc5" "rc6"
      "safer+" "safer-k64" "safer-sk64"
      "safer-k128" "safer-sk128"
      "aes" "twofish" "des" "3des"
      "cast5" "noekeon" "skipjack"
      "khazad" "seed" "kasumi"))

  (define (cipher-keysize type test)
    (suggest-keysize type test))

  ;; PKCS #5 padding.
  ;; reference http://www.rsa.com/rsalabs/node.asp?id=2127
  (define (pkcs5-padder bv block-size pad?)
    (if pad?
	(let* ((len (bytevector-length bv))
	       (mod (modulo len block-size))
	       (padding (- block-size mod)))
	  (when (zero? padding)
	    (set! padding 8))
	  (let ((new (make-bytevector (+ len padding) 0)))
	    ;; lazyness
	    (bytevector-fill! new padding)
	    (bytevector-copy! bv 0 new 0 len)
	    new))
	(let* ((len (bytevector-length bv))
	       (pad (bytevector-u8-ref bv (- len 1)))
	       (new (make-bytevector (- len pad) 0)))
	  (bytevector-copy! bv 0 new 0 (- len pad))
	  new)))

  (define-with-key (cipher type key 
			   :key (mode MODE_ECB)
			        (iv #f)
				(padder pkcs5-padder)
				(rounds 0)
				(ctr-mode CTR_COUNTER_LITTLE_ENDIAN))
    (unless (or (= mode MODE_ECB)
		(bytevector? iv))
      (assertion-violation 'cipher
			   "on the given mode iv id required"))
    (if (member type *symmetric-types*)
	(make-cipher type mode key iv rounds padder ctr-mode)
	(make-cipher type -1 key #f 0 #f 0)))
)
