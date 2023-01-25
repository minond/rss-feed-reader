#lang racket

(require net/rfc6455/conn-api
         net/cookies/server
         web-server/http/request-structs
         "../web/session.rkt")

(provide ws-conn-session-key
         lookup-ws-session)

(define (lookup-ws-session ws-conn)
  (lookup-session (ws-conn-session-key ws-conn)))

(define (ws-conn-session-key ws-conn)
  (let* ([headers (ws-conn-base-headers ws-conn)]
         [cookie-header (or (findf (lambda (header)
                                     (equal? #"Cookie" (header-field header)))
                                   headers)
                            "")]
         [cookie-value (cookie-header->alist
                        (header-value cookie-header))]
         [session-cookie (assoc #"session" cookie-value)]
         [session-key (and (pair? session-cookie)
                           (cdr session-cookie))])
    (bytes->string/utf-8 session-key)))
