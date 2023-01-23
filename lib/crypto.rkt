#lang racket

(require crypto
         crypto/libcrypto)

(provide make-password
         check-password)

(crypto-factories libcrypto-factory)

(define (make-password password #:salt [salt (crypto-random-bytes 128)])
  (let* ([bytes (string->bytes/utf-8 password)]
         [enc (scrypt bytes salt #:N (expt 2 14))])
    (values enc salt)))

(define (check-password #:unencrypted unencrypted
                        #:encrypted encrypted
                        #:salt salt)
  (let*-values ([(enc _) (make-password unencrypted #:salt salt)])
    (equal? enc encrypted)))
