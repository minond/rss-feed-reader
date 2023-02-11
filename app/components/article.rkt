#lang racket/base

(require racket/string
         racket/match
         racket/list
         gregor
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
         :article-list
         :article-previews)

(define (:article-full feed article)
  (let ([datetime (~t (article-date article) "y-M-d HH:mm:ss")]
        [humandate (~t (article-date article) "MMMM d, yyyy")])
    (:article
     (:h2 (:a 'href: (article-link article)
              'target: '_blank
              (article-title article)))
     (:h4 (feed-title feed))
     (:time 'datetime: datetime humandate)
     (:p (:literal (strip-xml (article-content article)))))))

(define (:article-list feed articles current-page page-count)
  (list
   (:spacer #:direction vertical #:size small)
   (:table
    (:thead
     (:th)
     (:th "Title")
     (:th 'class: "wsnw" "Added on")
     (:th ""))
    (:tbody (map :article-row articles)))
   (:spacer #:direction horizontal #:size small)
   (:pagination current-page page-count)))

(define (:article-row article)
  (let-values ([(route class) (if (article-archived article)
                                  (values "/articles/~a/unarchive" "archived")
                                  (values "/articles/~a/archive" "unarchived"))])
    (:tr 'class: (string-join (list "article-row" class))
         (:td 'class: "tc"
              (:a 'href: (format route (article-id article))
                  'class: (format "article-archive-toggle ~a" class)))
         (:td (:a 'href: (format "/articles/~a" (article-id article))
                  (article-title article)))
         (:td 'class: "wsnw" (~t (article-date article) "M/d/y"))
         (:td 'class: "wsnw" (:a 'href: (article-link article)
                                 'target: '_blank
                                 "Visit page")))))

(define (:article-previews articles current-page page-count scheduled)
  (match (cons (empty? articles) scheduled)
    [(cons #t #t)
     (list
      (:spacer #:direction horizontal
               #:size large)
      (:p 'class: "tc"
          "We're getting articles for you, hold tight!"))]
    [(cons #t #f)
     (list
      (:spacer #:direction horizontal
               #:size large)
      (:p 'class: "tc"
          "There are no articles to show at this time. Use the form below to
           subscribe to a feed.")
      (:feed-form))]
    [else
     (list
      (map :article-preview articles)
      (:pagination current-page page-count))]))

(define (:article-preview article)
  (let ([datetime (~t (article-date article) "y-M-d HH:mm:ss")]
        [humandate (~t (article-date article) "MMMM d, yyyy")])
    (:article 'class: "article-preview show-on-hover-container"
              (:a 'href: (format "/articles/~a" (article-id article))
                  (:h3 (article-title article))
                  (:p (string-chop (strip-html (article-content article)) 300 #:end "…")))
              (:time 'datetime: datetime humandate)
              (:spacer #:direction horizontal
                       #:size small)
              (:a 'class: "action show-on-hover"
                  'href: (article-link article)
                  'target: '_blank
                  "read")
              (:spacer #:direction horizontal
                       #:size small)
              (:a 'class: "action show-on-hover"
                  'href: (format "/articles/~a/archive" (article-id article))
                  "archive"))))
