#lang racket/base

(require "app/servlet.rkt"
         "app/websocket.rkt"
         "app/workers.rkt")

(define stop/worker (make-user-feed-sync-worker))
(define stop/ws (start/ws))

(start/servlet)

(clear-connections)
(stop/ws)
(stop/worker)
