#lang racket/base

(require net/rfc6455
         net/rfc6455/conn-api
         net/cookies/server
         web-server/http/request-structs
         "../lib/web/session.rkt"
         "../lib/websocket.rkt")

(provide start/ws
         ws-send/feed-update
         clear-connections)

(define (start/ws)
  (log-info "starting websocket server")
  (ws-serve authenticated-ping-pong
            #:port 8082))

(define (ws-send/feed-update session-key)
  (ws-send session-key #hasheq((notification . "feed-update"))))
