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
    "Unauthenticated requests are redirected to login page"
    (with-app-request "/articles"
      (check-equal?
             (current-response-code)
             301)
      (check-equal?
             (current-response-header #"Location")
             #"/sessions/new")))

   (test-case
    "Zero state in articles view directs user to subscribe to a new feed"
    (with-authenticated-app-request "/articles"
      (check string-contains?
             (current-response-body)
             "There are no articles to show at this time.")))))
