#lang racket/base

(require racket/sequence
         db
         deta
         "../../lib/web.rkt"
         "../../lib/crypto.rkt"
         "../parameters.rkt"
         "../components.rkt"
         "../models.rkt"
         "../workers/feed-download.rkt")

(provide /feeds
         /feeds/new
         /feeds/create
         /feeds/<id>/subscribe
         /feeds/<id>/unsubscribe
         /feeds/<id>/articles)

(define page-size 10)

(define (/feeds req)
  (let ([feed-stats (sequence->list
                     (in-entities (current-database-connection)
                                  (select-feed-stats #:user-id (current-user-id))))])
    (render (:feed-list feed-stats))))

(define (/feeds/new req)
  (render (:feed-form)))

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

(define (/feeds/<id>/subscribe req id)
  (query (current-database-connection) (subscribe-to-feed #:id id
                                                          #:user-id (current-user-id)))
  (with-flash #:alert "Subscribed to feed."
    (redirect-back)))

(define (/feeds/<id>/unsubscribe req id)
  (query (current-database-connection) (unsubscribe-from-feed #:id id
                                                              #:user-id (current-user-id)))
  (with-flash #:alert "Unsubscribed from feed."
    (redirect-back)))

(define (/feeds/<id>/articles req id)
  (let* ([current-page (or (string->number (get-parameter 'page req)) 1)]
         [page-count (ceiling (/ (lookup (current-database-connection)
                                         (count-articles-by-feed #:feed-id id
                                                                 #:user-id (current-user-id)))
                                 page-size))]
         [offset (* (- current-page 1) page-size)]
         [feed (lookup (current-database-connection)
                       (find-feed-by-id #:id id
                                        #:user-id (current-user-id)))]
         [articles (sequence->list
                    (in-entities (current-database-connection)
                                 (select-articles-by-feed #:feed-id id
                                                          #:user-id (current-user-id)
                                                          #:limit page-size
                                                          #:offset offset)))])
    (render (:article-list feed articles current-page page-count))))
