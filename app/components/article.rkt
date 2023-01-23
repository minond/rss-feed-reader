 #lang racket

(require gregor
         "../models/article.rkt"
         "../models/feed.rkt"
         "../../lib/string.rkt"
         "pagination.rkt"
         (prefix-in : scribble/html/xml)
         (prefix-in : scribble/html/html)
         (prefix-in : scribble/html/extra))

(provide :article-full
         :articles-list)

(define (:article-full feed article)
  (let ([datetime (~t (article-date article) "y-M-d HH:mm:ss")]
        [humandate (~t (article-date article) "MMMM d, yyyy")])
    (:article
     (:h1 (:a 'href: (article-link article) (article-title article)))
     (:h4 (feed-title feed))
     (:time 'datetime: datetime humandate)
     (:p (:literal (strip-xml (article-content article)))))))

(define (:articles-list articles current-page page-count)
  (append
   (map (lambda (article)
          (:article-row null article)) articles)
   (:pagination current-page page-count)))

(define (:article-row feed article)
  (let ([datetime (~t (article-date article) "y-M-d HH:mm:ss")]
        [humandate (~t (article-date article) "MMMM d, yyyy")])
    (:article 'class: "row"
              (:h4
               (:a 'href: (format "/articles/~a" (article-id article))
                   (article-title article)))
              #;(:h5 (feed-title feed))
              (:p (string-chop (strip-html (article-content article)) 300 #:end "…"))
              (:time 'datetime: datetime humandate)
              (:a 'class: "pl1 action showonhover" 'href: (article-link article) 'target: "_blank" "read")
              #;(:a 'class: "pl1 action showonhover" 'href: "#save" "save")
              (:a 'class: "pl1 action showonhover" 'href: (format "/articles/~a/archive" (article-id article)) "archive"))))
