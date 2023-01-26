#lang racket

(require "../models/feed.rkt"
         "shared.rkt"
         (prefix-in : scribble/html/xml)
         (prefix-in : scribble/html/html)
         (prefix-in : scribble/html/extra))

(provide :feed-list
         :feed-form)

(define (:feed-list feed-stats)
  (list
   (:spacer #:direction vertical #:size small)
   (:table
    (:thead
     (:th "Enabled")
     (:th "Title")
     (:th "Articles")
     (:th "Archived")
     (:th "Date subscribed"))
    (:tbody (map :feed-row feed-stats)))))

(define (:feed-row feed)
  (:tr
   (:td (if (feed-stats-enabled feed) "Yes" "No"))
   (:td (:a 'href: (feed-stats-link feed)
            'target: '_blank
            (feed-stats-title feed)))
   (:td (feed-stats-total-count feed))
   (:td (feed-stats-archived-count feed))
   (:td "December 27, 2022")))

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
