#lang racket

(require web-server/servlet-env
         "dispatch.rkt")

(provide start)

(define (start)
  (serve/servlet app-dispatch
                 #:launch-browser? #f
                 #:servlet-path "/"
                 #:port 8000
                 #:servlet-regexp #rx""))
