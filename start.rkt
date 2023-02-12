#lang racket/base

(require "app/servlet.rkt"
         "app/websocket.rkt"
         "app/workers.rkt")

(define stop/feed-worker (make-user-feed-sync-worker))
(define stop/user-worker (make-user-background-sync-worker))
(define stop/ws (start/ws))

(start/servlet)

(clear-connections)
(stop/ws)
(stop/feed-worker)
(stop/user-worker)
