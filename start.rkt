#lang racket

(require "app/servlet.rkt"
         "app/websocket.rkt")

(start/ws)
(start/servlet)