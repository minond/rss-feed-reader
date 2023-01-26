#lang racket

(require gregor
         "../models/article.rkt"
         "../models/feed.rkt"
         "../../lib/string.rkt"
         "pagination.rkt"
         "feed.rkt"
         "shared.rkt"
         (prefix-in : scribble/html/xml)
         (prefix-in : scribble/html/html)
         (prefix-in : scribble/html/extra))

(provide :article-full
         :article-list)

(define (:article-full feed article)
  (let ([datetime (~t (article-date article) "y-M-d HH:mm:ss")]
        [humandate (~t (article-date article) "MMMM d, yyyy")])
    (:article
     (:h1 (:a 'href: (article-link article)
              (article-title article)))
     (:h4 (feed-title feed))
     (:time 'datetime: datetime humandate)
     (:p (:literal (strip-xml (article-content article)))))))

(define (:article-list articles current-page page-count)
  (if (empty? articles)
      (list
       (:spacer #:direction horizontal
                #:size large)
       (:p 'class: "tc"
           "There are no articles to show at this time. Use the form below to
           subscribe to a feed.")
       (:feed-form))
      (list
       (map :article-preview articles)
       (:pagination current-page page-count))))

(define (:article-preview article)
  (let ([datetime (~t (article-date article) "y-M-d HH:mm:ss")]
        [humandate (~t (article-date article) "MMMM d, yyyy")])
    (:article 'class: "article-preview show-on-hover-container"
              (:h4
               (:a 'href: (format "/articles/~a" (article-id article))
                   (article-title article)))
              (:p (string-chop (strip-html (article-content article)) 300 #:end "…"))
              (:time 'datetime: datetime humandate)
              (:spacer #:direction horizontal
                       #:size small)
              (:a 'class: "action show-on-hover"
                  'href: (article-link article)
                  'target: "_blank"
                  "read")
              (:spacer #:direction horizontal
                       #:size small)
              (:a 'class: "action show-on-hover"
                  'href: (format "/articles/~a/archive" (article-id article))
                  "archive"))))
