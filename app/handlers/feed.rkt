#lang racket

(require deta
         "../../lib/web.rkt"
         "../../lib/crypto.rkt"
         "../parameters.rkt"
         "../components.rkt"
         "../models.rkt"
         "../workers/feed-download.rkt")

(provide /feeds/new
         /feeds/create)

(define (/feeds/new req)
  (render :page (:feed-form)))

(define (/feeds/create req)
  (let* ([rss (get-binding 'rss req)]
         [exists (lookup (current-database-connection)
                         (find-feed-by-rss #:user-id (current-user-id)
                                           #:rss rss))])
    (unless exists
      (schedule-feed-download (current-user-id) rss))
    (with-flash #:alert (and (not exists) "Downloading feed data and articles.")
      #:notice (and exists "This feed already exists.")
      (redirect "/articles"))))
