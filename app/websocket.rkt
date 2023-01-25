#lang racket

(require net/rfc6455
         net/rfc6455/conn-api
         net/cookies/server
         web-server/http/request-structs
         "../lib/web/session.rkt"
         "../lib/websocket.rkt")

(provide start/ws)

(define (start/ws)
  (ws-serve authenticated-ping-pong
            #:port 8082))
