#lang racket

(require threading
         gregor
         deta)

(provide (struct-out article)
         make-article
         count-articles
         select-articles
         find-article-by-id
         find-article-by-link
         archive-article-by-id)

(define-schema article
  ([id id/f #:primary-key #:auto-increment]
   [user-id id/f]
   [feed-id id/f]
   [link string/f #:contract non-empty-string?]
   [title string/f #:contract non-empty-string?]
   [date datetime/f]
   [content string/f #:contract non-empty-string?]
   [(archived #f) boolean/f]
   [(created-at (now/utc)) datetime/f]))

(define (count-articles #:user-id user-id #:archived [archived #f])
  (~> (from article #:as a)
      (select (count a.id))
      (where (and (= a.user-id ,user-id)
                  (= a.archived ,archived)))))

(define (select-articles #:user-id user-id
                         #:archived [archived #f]
                         #:limit lim
                         #:offset [off 0])
  (~> (from article #:as a)
      (where (and (= a.user-id ,user-id)
                  (= a.archived ,archived)))
      (order-by ([date #:desc]))
      (offset ,off)
      (limit ,lim)))

(define (find-article-by-id #:id id #:user-id user-id)
  (~> (from article #:as a)
      (where (and (= a.id ,id)
                  (= a.user-id ,user-id)))
      (limit 1)))

(define (find-article-by-link #:user-id user-id #:link link)
  (~> (from article #:as a)
      (where (and (= a.user-id ,user-id)
                  (= a.link ,link)))
      (limit 1)))

(define (archive-article-by-id #:id id #:user-id user-id)
  (~> (from article #:as a)
      (update [archived #t])
      (where (and (= a.id ,id)
                  (= a.user-id ,user-id)))))
