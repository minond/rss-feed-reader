#lang racket

(require gregor)

(provide date->rfc7231)

(define (date->rfc7231 date)
  (~t date "E, d MMM yyyy HH:mm:ss"))
