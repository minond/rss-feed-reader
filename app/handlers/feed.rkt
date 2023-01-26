#lang racket

(require deta
         "../../lib/web.rkt"
         "../../lib/crypto.rkt"
         "../parameters.rkt"
         "../components.rkt"
         "../models.rkt"
         "../workers/feed-download.rkt")

(provide /feeds
         /feeds/new
         /feeds/create)

(define (/feeds req)
  (let ([feed-stats (sequence->list
                     (in-entities (current-database-connection)
                                  (select-feed-stats #:user-id (current-user-id))))])
    (render :page (:feed-list feed-stats))))

(define (/feeds/new req)
  (render :page (:feed-form)))

(define (/feeds/create req)
  (let* ([rss (get-binding 'rss req)]
         [exists (lookup (current-database-connection)
                         (find-feed-by-rss #:user-id (current-user-id)
                                           #:rss rss))])
    (unless exists
      (schedule-feed-download rss (current-user-id) (session-key (current-session))))
    (with-flash #:alert (and (not exists) "Downloading feed data and articles.")
      #:notice (and exists "This feed already exists.")
      (redirect "/articles"))))
