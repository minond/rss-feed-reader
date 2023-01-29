#lang racket/base

(require (prefix-in : scribble/html/xml)
         (prefix-in : scribble/html/html)
         (prefix-in : scribble/html/extra))

(provide :spacer
         horizontal
         vertical
         small
         medium
         large)

(define-values (horizontal vertical small medium large)
  (values 'horizontal 'vertical 'small 'medium 'large))


(define (:spacer #:direction direction
                 #:size [size medium])
  (:div 'class: (format "spacer ~a ~a" direction size)))
