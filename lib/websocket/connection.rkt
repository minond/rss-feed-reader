#lang racket

(require json
         net/rfc6455
         net/rfc6455/conn-api
         net/cookies/server
         web-server/http/request-structs
         "../web/session.rkt"
         "session.rkt")

(provide lookup-connection
         ws-send
         authenticated-ping-pong)

(define connections (make-hash))
(define (lookup-connection key)
  (hash-ref connections key #f))

(define (ws-send session-key message)
  (let ([ws-conn (lookup-connection session-key)])
    (when (ws-conn? ws-conn)
      (ws-send! ws-conn (encode message)))))

(define (encode message)
  (if (hash? message)
      (jsexpr->string message)
      message))

(define (authenticated-ping-pong ws-conn state)
  (let ([session-key (ws-conn-session-key ws-conn)]
        [session (lookup-ws-session ws-conn)])
    (when (authenticated? session)
      (hash-set! connections session-key ws-conn)

      (let loop ()
        (match (ws-recv ws-conn #:payload-type 'text)
          [(? eof-object?)
           (void)]
          ["ping"
           (ws-send! ws-conn "pong")
           (loop)]
          [else
           (loop)]))

      (hash-remove! connections session-key)
      (ws-close! ws-conn))))
