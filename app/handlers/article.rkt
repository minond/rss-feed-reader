#lang racket/base

(require (except-in racket/list group-by)
         racket/sequence
         db
         deta
         "../../lib/web.rkt"
         "../parameters.rkt"
         "../components.rkt"
         "../models.rkt")

(provide /articles
         /arcticles/<id>/show
         /articles/<id>/archive
         /articles/<id>/unarchive)

(define page-size 10)

(define (/articles req)
  (let* ([current-page (or (string->number (get-parameter 'page req)) 1)]
         [page-count (ceiling (/ (lookup (current-database-connection)
                                         (count-articles #:user-id (current-user-id)
                                                         #:archived #f))
                                 page-size))]
         [offset (* (- current-page 1) page-size)]
         [articles (sequence->list
                    (in-entities (current-database-connection)
                                 (select-articles #:user-id (current-user-id)
                                                  #:archived #f
                                                  #:limit page-size
                                                  #:offset offset)))])
    (render :page (:article-previews articles current-page page-count))))

(define (/arcticles/<id>/show req id)
  (let* ([article (lookup (current-database-connection)
                          (find-article-by-id #:id id
                                              #:user-id (current-user-id)))]
         [feed (lookup (current-database-connection)
                       (find-feed-by-id #:id (article-feed-id article)
                                        #:user-id (current-user-id)))])
    (render :page (:article-full feed article))))

(define (/articles/<id>/archive req id)
  (query (current-database-connection) (archive-article-by-id #:id id
                                                              #:user-id (current-user-id)))
  (with-flash #:alert "Article archived."
    (redirect-back)))

(define (/articles/<id>/unarchive req id)
  (query (current-database-connection) (unarchive-article-by-id #:id id
                                                                #:user-id (current-user-id)))
  (with-flash #:alert "Article unarchived."
    (redirect-back)))
