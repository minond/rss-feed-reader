#lang racket

(provide reduce-by-pair
         find-pair)

(define (reduce-by-pair f xs [id '()])
  (if (< (length xs) 2)
      (append id xs)
      (reduce-by-pair f (cdr xs) (f id (car xs) (cadr xs)))))

(define (find-pair key xs #:default [default null])
  (or (findf (lambda (x)
               (eq? key (car x))) xs)
      default))
