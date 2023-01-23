#lang racket

(require threading)

(provide string-chop
         strip-xml
         strip-html)

(define (string-chop str maxlen #:end [end ""])
  (if (<= (string-length str) maxlen)
      str
      (string-append (string-trim (substring str 0 maxlen)) end)))

(define (strip-xml str)
  (~> str
      (string-replace _ "<![CDATA[" "" #:all? #t)
      (string-replace _ "]]>" "" #:all? #t)))

(define (strip-html str)
  (~> str
      (strip-xml _)
      (regexp-replace* #rx"(<([^>]+)>)" _ "")))
