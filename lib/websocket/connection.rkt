#lang racket/base

(require racket/match
         racket/list
         json
         net/rfc6455
         net/rfc6455/conn-api
         net/cookies/server
         web-server/http/request-structs
         "../web/session.rkt"
         "session.rkt")

(provide lookup-connections
         clear-connections
         ws-send
         authenticated-ping-pong)

(define connections (make-hash))

(define (lookup-connections key)
  (hash-ref connections key '()))

(define (clear-connections)
  (hash-clear! connections))

(define (ws-send session-key message)
  (for ([ws-conn (lookup-connections session-key)])
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
      ; XXX Need to sync access to lookup-connections by session key
      (let ([ws-conns (lookup-connections session-key)])
        (hash-set! connections session-key (cons ws-conn ws-conns)))

      (let loop ()
        (match (ws-recv ws-conn #:payload-type 'text)
          [(? eof-object?)
           (void)]
          ["ping"
           (ws-send! ws-conn "pong")
           (loop)]
          [else
           (loop)]))

      ; XXX Need to sync access to lookup-connections by session key
      (let ([ws-conns (remove ws-conn (lookup-connections session-key))])
        (hash-set! connections session-key ws-conns)
        (when (empty? ws-conns)
          (hash-remove! connections session-key))))

    (ws-close! ws-conn)))
