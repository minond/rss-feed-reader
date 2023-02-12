#lang racket/base

(require racket/string
         threading
         gregor
         deta)

(provide (schema-out feed)
         (schema-out feed-stats)
         make-feed
         select-feed-stats
         select-feeds-in-need-of-sync
         find-feed-by-id
         find-feed-by-rss
         subscribe-to-feed
         unsubscribe-from-feed)

(define-schema feed
  ([id id/f #:primary-key #:auto-increment]
   [user-id id/f]
   [rss string/f #:contract non-empty-string?]
   [link string/f #:contract non-empty-string?]
   [title string/f #:contract non-empty-string?]
   [(subscribed #t) boolean/f]
   [last-sync-attempted-at datetime/f #:nullable]
   [last-sync-completed-at datetime/f #:nullable]
   [(created-at (now/utc)) datetime/f]))

(define-schema feed-stats
  #:virtual
  ([id id/f]
   [title string/f]
   [link string/f]
   [subscribed boolean/f]
   [last-sync-completed-at datetime/f #:nullable]
   [created-at datetime/f]
   [total-count integer/f]
   [archived-count integer/f]
   [unarchived-count integer/f]))

(define (select-feed-stats #:user-id user-id)
  (~> (from feed #:as f)
      (select f.id f.title f.link f.subscribed f.last-sync-completed-at f.created-at
              (count a.id)
              (sum (iif (= a.archived #t) 1 0))
              (sum (iif (= a.archived #f) 1 0)))
      (join article #:as a #:on (= f.id a.feed_id))
      (where (= f.user-id ,user-id))
      (group-by f.id f.title)
      (order-by ([f.title]))
      (project-onto feed-stats-schema)))

(define (select-feeds-in-need-of-sync #:older-than [older-than (-hours (now/utc) 12)]
                                      #:limit [lim 10])
  (~> (from feed #:as f)
      (where (and (= f.subscribed #t)
                  (or
                   (< f.last-sync-attempted-at ,(~t older-than "yyyy-MM-dd'T'HH:mm:ss"))
                   (is f.last-sync-attempted-at null))))
      (order-by ([f.last-sync-attempted-at]))
      (limit ,lim)))

(define (find-feed-by-id #:id id #:user-id user-id)
  (~> (from feed #:as f)
      (where (and (= f.id ,id)
                  (= f.user-id ,user-id)))
      (limit 1)))

(define (find-feed-by-rss #:user-id user-id  #:rss rss)
  (~> (from feed #:as f)
      (where (and (= f.user-id ,user-id)
                  (= f.rss ,rss)))
      (limit 1)))

(define (subscribe-to-feed #:id id #:user-id user-id)
  (~> (from feed #:as f)
      (update [subscribed #t])
      (where (and (= f.id ,id)
                  (= f.user-id ,user-id)))))

(define (unsubscribe-from-feed #:id id #:user-id user-id)
  (~> (from feed #:as f)
      (update [subscribed #f])
      (where (and (= f.id ,id)
                  (= f.user-id ,user-id)))))
