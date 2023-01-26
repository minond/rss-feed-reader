#lang racket

(require threading
         deta)

(provide (struct-out feed)
         (struct-out feed-stats)
         make-feed
         select-feed-stats
         find-feed-by-id
         find-feed-by-rss)

(define-schema feed
  ([id id/f #:primary-key #:auto-increment]
   [user-id id/f]
   [rss string/f #:contract non-empty-string?]
   [link string/f #:contract non-empty-string?]
   [title string/f #:contract non-empty-string?]
   [(enabled #t) boolean/f]))

(define-schema feed-stats
  #:virtual
  ([id id/f]
   [title string/f]
   [link string/f]
   [enabled boolean/f]
   [total-count integer/f]
   [archived-count integer/f]
   [unarchived-count integer/f]))

(define (select-feed-stats #:user-id user-id)
  (~> (from feed #:as f)
      (select f.id f.title f.link f.enabled
              (count a.id)
              (sum (iif (= a.archived #t) 1 0))
              (sum (iif (= a.archived #f) 1 0)))
      (join article #:as a #:on (= f.id a.feed_id))
      (where (= f.user-id ,user-id))
      (group-by f.id f.title)
      (order-by ([f.enabled] [f.title]))
      (project-onto feed-stats-schema)))

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
