#lang racket

(require threading
         deta)

(provide (struct-out feed)
         make-feed
         find-feed-by-id
         find-feed-by-rss)

(define-schema feed
  ([id id/f #:primary-key #:auto-increment]
   [user-id id/f]
   [rss string/f #:contract non-empty-string?]
   [link string/f #:contract non-empty-string?]
   [title string/f #:contract non-empty-string?]
   [(enabled #t) boolean/f]))

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
