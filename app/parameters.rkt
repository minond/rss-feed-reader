#lang racket

(provide current-database-connection)

(define current-database-connection (make-parameter #f))
