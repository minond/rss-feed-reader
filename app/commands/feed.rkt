#lang racket/base

(require racket/function
         racket/match
         racket/exn
         deta
         gregor
         "../parameters.rkt"
         "../models.rkt"
         (prefix-in rss: "../../lib/rss.rkt"))

(provide run
         create-feed
         update-feed)

(struct create-feed (user-id rss) #:transparent)
(struct update-feed (user-id feed-id) #:transparent)

(define (run cmd)
  (printf "[INFO] processing ~a\n" cmd)
  (define-values (saved-feed remote-feed)
    (find-or-create-feed cmd))

  (update-one! (current-database-connection)
               (update-feed-last-sync-attempted-at saved-feed (const (now/utc))))
  (create-articles-for-feed saved-feed remote-feed)
  (update-one! (current-database-connection)
               (update-feed-last-sync-completed-at saved-feed (const (now/utc)))))

(define (find-or-create-feed cmd)
  (match cmd
    [(create-feed user-id rss)
     (let* ([remote-feed (rss:feed! rss)]
            [saved-feed (insert-one! (current-database-connection)
                                     (make-feed #:user-id user-id
                                                #:rss (rss:feed-rss remote-feed)
                                                #:link (rss:feed-link remote-feed)
                                                #:title (rss:feed-title remote-feed)))])
       (values saved-feed remote-feed))]

    [(update-feed user-id feed-id)
     (let* ([saved-feed (lookup (current-database-connection)
                                (find-feed-by-id #:id feed-id
                                                 #:user-id user-id))]
            [remote-feed (rss:feed! (feed-rss saved-feed))])
       (values saved-feed remote-feed))]))

(define (create-articles-for-feed saved-feed remote-feed)
  (printf "[INFO] saving new article records for ~a\n" (feed-rss saved-feed))

  (let ([user-id (feed-user-id saved-feed)]
        [feed_id (feed-id saved-feed)])
    (for ([article (rss:feed-articles remote-feed)])
      (define link (rss:article-link article))
      (unless (lookup (current-database-connection)
                      (find-article-by-link #:user-id user-id
                                            #:link link))
        (printf "[INFO] saving article record for ~a\n" (rss:article-link article))
        (insert-one! (current-database-connection)
                     (make-article #:user-id user-id
                                   #:feed-id feed_id
                                   #:link (rss:article-link article)
                                   #:title (rss:article-title article)
                                   #:date (rss:article-date article)
                                   #:content (rss:article-content article))))))

  (printf "[INFO] done saving article records for ~a\n" (feed-rss saved-feed)))
