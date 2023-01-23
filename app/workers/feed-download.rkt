#lang racket

(require deta
         "../parameters.rkt"
         "../models.rkt"
         (prefix-in rss: "../../lib/rss.rkt"))

(provide schedule-feed-download)

(define (schedule-feed-download user-id rss)
  (thread-send feed-download-thread (list user-id rss)))

(define feed-download-thread
  (thread
   (lambda ()
     (let loop ()
       (match-define (list user-id rss) (thread-receive))
       (define feed (rss:feed! rss))

       (printf "saving feed ~a\n" rss)
       (define saved-feed
         (or
          (lookup (current-database-connection)
                  (find-feed-by-rss #:user-id user-id
                                    #:rss rss))
          (insert-one! (current-database-connection)
                       (make-feed #:user-id user-id
                                  #:rss (rss:feed-rss feed)
                                  #:link (rss:feed-link feed)
                                  #:title (rss:feed-title feed)))))
       (printf "feed id: ~a\n" (feed-id saved-feed))

       (for ([article (rss:feed-articles feed)])
         (define link (rss:article-link article))
         (unless (lookup (current-database-connection)
                         (find-article-by-link #:user-id user-id
                                               #:link link))
           (printf "saving article ~a\n" (rss:article-link article))
           (insert-one! (current-database-connection)
                        (make-article #:user-id user-id
                                      #:feed-id (feed-id saved-feed)
                                      #:link (rss:article-link article)
                                      #:title (rss:article-title article)
                                      #:date (rss:article-date article)
                                      #:content (rss:article-content article)))))
       (printf "done processing ~a\n" rss)
       (loop)))))
