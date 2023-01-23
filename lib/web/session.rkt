#lang racket

(require gregor
         web-server/servlet
         web-server/http/id-cookie
         "../format.rkt"
         "../random.rkt")

(provide current-request current-session current-user-id
         session? session-user-id session-flash lookup-session
         update-session-cookie create-session-cookie clear-session-coookie
         authenticated? destroy-session)

(define current-request (make-parameter #f))
(define current-session (make-parameter #f))
(define current-user-id (make-parameter #f))

(struct session (user-id flash))
(define session-cookie-name "session")
(define sessions (make-hash))

(define (get-session-cookie req)
  (findf (lambda (cookie)
           (equal? session-cookie-name
                   (client-cookie-name cookie)))
         (request-cookies req)))

(define (lookup-session req)
  (let/cc return
    (unless (request? req)
      (return (session #f #f)))
    (define session-cookie (get-session-cookie req))
    (unless session-cookie
      (return (session #f #f)))
    (hash-ref sessions
              (client-cookie-value session-cookie)
              (session #f #f))))

(define (destroy-session req)
  (let/cc return
    (define session-cookie (get-session-cookie req))
    (unless session-cookie
      (return #f))
    (hash-remove! sessions
                  (client-cookie-value session-cookie))))

(define (create-session user-id flash)
  (let ([key (random-string)]
        [data (session user-id flash)])
    (hash-set! sessions key data)
    key))

(define (create-session-cookie #:user-id user-id #:flash [flash #f])
  (make-cookie session-cookie-name
               (create-session user-id flash)
               #:path "/"
               #:expires (date->rfc7231 (+years (now/utc) 1))))

(define (update-session-cookie session #:user-id [user-id #f] #:flash [flash #f])
  (let ([orig-user-id (session-user-id session)]
        [orig-flash (session-flash session)])
    (create-session-cookie #:user-id (or user-id orig-user-id)
                           #:flash (or flash orig-flash))))

(define (clear-session-coookie)
  (logout-id-cookie session-cookie-name #:path "/"))

(define authenticated?
  (case-lambda
    [() (authenticated? (current-request))]
    [(req) (let ([session (lookup-session req)])
             (and session (session-user-id session)))]))
