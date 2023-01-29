#lang racket/base

(require
  (for-syntax racket
              syntax/parse))

(require racket/promise
         racket/list
         net/cookies
         net/url-string
         web-server/http/request-structs
         web-server/http/response-structs
         "../app/dispatch.rkt"
         "../lib/web/session.rkt")

(provide authenticated-request
         make-authenticated-request
         with-authenticated-request)

(define (authenticated-request #:url url
                               #:method [method #"GET"]
                               #:post-data [post-data #f]
                               #:user-id [user-id 123])
  (make-request method (string->url url)
                (list (header #"Cookie"
                              (cookie->set-cookie-header
                               (create-session-cookie #:user-id user-id))))
                (delay empty) post-data "1.2.3.4" 80 "4.3.2.1"))

(define make-authenticated-request
  (make-keyword-procedure
   (lambda (kws kw-args . args)
     (app-dispatch (keyword-apply authenticated-request kws kw-args args)))))

(define with-authenticated-request
  (make-keyword-procedure
   (lambda (kws kw-args . args)
     (let* ([url (car args)]
            [f (last args)]
            [req-args (drop-right (cdr args) 1)]
            [req (keyword-apply authenticated-request
                                #:url url
                                kws kw-args req-args)]
            [res (app-dispatch req)]
            [op (open-output-string)])
       ((response-output res) op)
       (f res op)))))
