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

(provide current-response
         current-response-body
         current-response-code
         current-response-headers
         current-response-header
         with-session
         with-app-request
         with-authenticated-app-request)

(define current-session-cookie-header (make-parameter #f))
(define current-response (make-parameter #f))
(define current-response-headers (make-parameter #f))
(define current-response-body (make-parameter #f))
(define current-response-code (make-parameter #f))

(define (make-session-cookie-header user-id)
  (header #"Cookie"
          (cookie->set-cookie-header
           (create-session-cookie #:user-id user-id))))

(define-syntax (with-session stx)
  (syntax-parse stx
    [(with-session (~optional (~seq #:user-id user-id))
       e ...)
     #'(let ([cookie-header (make-session-cookie-header (~? user-id 123))])
         (parameterize ([current-session-cookie-header cookie-header])
           e ...))]))

(define (make-app-request #:url url
                          #:method [method #"GET"]
                          #:post-data [post-data #f])
  (let ([req (make-request method (string->url url)
                           (filter header? (list (current-session-cookie-header)))
                           (delay empty) post-data "1.2.3.4" 80 "4.3.2.1")])
    (app-dispatch req)))

(define-syntax (with-app-request stx)
  (syntax-parse stx
    [(with-app-request url:expr
       (~seq (~seq kw:keyword kv:expr) ...)
       e ...)
     #'(let* ([res (keyword-apply make-app-request
                                  #:url url
                                  (syntax->datum #'(kw ...))
                                  (syntax->datum #'(kv ...))
                                  empty)]
              [op (open-output-string)])
         ((response-output res) op)
         (parameterize ([current-response res]
                        [current-response-body (get-output-string op)]
                        [current-response-code (response-code res)]
                        [current-response-headers (response-headers res)])
           e ...))]))

(define-syntax (with-authenticated-app-request stx)
  (syntax-parse stx
    [(with-authenticated-app-request
         url:expr
       (~optional (~seq #:user-id user-id))
       (~seq (~seq kw:keyword kv:expr) ...)
       e ...)
     #'(with-session #:user-id (~? user-id 123)
         (with-app-request url (syntax->datum #'((#:kw kv) ...))
           e ...))]))

(define (current-response-header field)
  (let ([header (headers-assq field (current-response-headers))])
    (and header
         (header-value header))))
