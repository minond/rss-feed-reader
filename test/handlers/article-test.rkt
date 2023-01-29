#lang racket/base

(require racket/string
         rackunit
         rackunit/text-ui
         "../utils.rkt")

(run-tests
 (test-suite
  "Article endpoints"

  (test-case
   "Zero state in articles view directs user to subscribe to a new feed"
   (with-authenticated-request "/articles"
     (lambda (res op)
       (check string-contains?
              (get-output-string op)
              "There are no articles to show at this time."))))))
