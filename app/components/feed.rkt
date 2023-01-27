#lang racket

(require gregor
         (prefix-in : scribble/html/xml)
         (prefix-in : scribble/html/html)
         (prefix-in : scribble/html/extra)
         "../models/feed.rkt"
         "shared.rkt")

(provide :feed-list
         :feed-form)

(define (:feed-list feed-stats)
  (list
   (:spacer #:direction vertical #:size small)
   (:table
    (:thead
     (:th)
     (:th "Title")
     (:th "Articles")
     (:th "Archived")
     (:th "Date subscribed")
     (:th ""))
    (:tbody (map :feed-row feed-stats)))))

(define (:feed-row feed)
  (let-values ([(route class) (if (feed-stats-subscribed feed)
                                  (values "/feeds/~a/unsubscribe" "subscribed")
                                  (values "/feeds/~a/subscribe" "unsubscribed"))])
    (:tr 'class: (string-join (list "feed-row" class))
     (:td 'class: "tc"
          (:a 'href: (format route (feed-stats-id feed))
                             'class: (format "feed-subscription-toggle ~a" class))
          (:td (:a 'href: (feed-stats-link feed)
                   'target: '_blank
                   (feed-stats-title feed)))
          (:td (feed-stats-total-count feed))
          (:td (feed-stats-archived-count feed))
          (:td (~t (feed-stats-created-at feed) "MMMM d, yyyy"))
          (:td (:a 'href: (feed-stats-link feed)
                   'target: '_blank
                   "Visit site"))))))

(define (:feed-form)
  (:form 'action: "/feeds/create"
         'method: "post"
         (:input 'type: "url"
                 'name: "rss"
                 'required: "true"
                 'autofocus: "true"
                 'placeholder: "RSS URL")
         (:input 'type: "submit"
                 'value: "Add")))
