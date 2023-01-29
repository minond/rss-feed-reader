#lang racket/base

(require racket/string
         rackunit
         "../utils.rkt")

(module+ test
  (require rackunit/text-ui)
  (run-tests article-handlers-tests))

(define article-handlers-tests
  (test-suite
   "Article endpoints"

   (test-case
    "Zero state in articles view directs user to subscribe to a new feed"
    (with-authenticated-request "/articles"
      (lambda (res op)
        (check string-contains?
               (get-output-string op)
               "There are no articles to show at this time."))))

   (test-case
    "Zero state in articles view directs user to subscribe to a new feed"
    (with-authenticated-request "/articles"
      (lambda (res op)
        (check string-contains?
               (get-output-string op)
               "There are no articles to show at this time."))))))
