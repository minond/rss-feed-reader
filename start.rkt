#lang racket

(require "app/servlet.rkt")
(require "app/websocket.rkt")

(start/ws)
(start/servlet)
