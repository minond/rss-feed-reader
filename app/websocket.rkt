#lang racket

(require net/rfc6455
         net/rfc6455/conn-api
         net/cookies/server
         web-server/http/request-structs
         "../lib/web/session.rkt"
         "../lib/websocket/session.rkt")

(provide start/ws
         ws-send
         connections
         lookup-connection)

(define connections (make-hash))
(define (lookup-connection key)
  (hash-ref connections key #f))

(define (ws-send session-key message)
  (let ([ws-conn (lookup-connection session-key)])
    (when (ws-conn? ws-conn)
      (ws-send! ws-conn message))))

(define (handler ws-conn state)
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

(define (start/ws)
  (ws-serve handler
            #:port 8082))
