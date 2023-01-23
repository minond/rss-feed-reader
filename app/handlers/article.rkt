#lang racket

(require db
         deta
         ; web-server/servlet
         "../../lib/web.rkt"
         ; "../../lib/crypto.rkt"
         "../parameters.rkt"
         "../components.rkt"
         "../models.rkt")

(provide /articles
         /arcticles/show
         /articles/archive)

(define page-size 10)

(define (/articles req)
  (if (not (authenticated? req))
      (redirect "/sessions/new")
      (let* ([current-page (or (string->number (get-parameter 'page req)) 1)]
             [page-count (ceiling (/ (lookup (current-database-connection)
                                             (count-articles #:user-id (current-user-id)
                                                             #:archived #f)) page-size))]
             [offset (* (- current-page 1) page-size)]
             [articles (sequence->list
                        (in-entities (current-database-connection)
                                     (select-articles #:user-id (current-user-id)
                                                      #:archived #f
                                                      #:limit page-size
                                                      #:offset offset)))])
        (render :page (:articles-list articles current-page page-count)))))

(define (/arcticles/show req id)
  (if (not (authenticated? req))
      (redirect "/sessions/new")
      (let* ([article (lookup (current-database-connection)
                              (find-article-by-id #:id id
                                                  #:user-id (current-user-id)))]
             [feed (lookup (current-database-connection)
                           (find-feed-by-id #:id (article-feed-id article)
                                            #:user-id (current-user-id)))])
        (render :page (:article-full feed article)))))

(define (/articles/archive req id)
  (if (not (authenticated? req))
      (redirect "/sessions/new")
      (begin
        (query (current-database-connection) (archive-article-by-id id))
        (with-flash #:alert "Article archived."
          (redirect "/articles")))))