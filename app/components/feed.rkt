#lang racket

(require (prefix-in : scribble/html/xml)
         (prefix-in : scribble/html/html)
         (prefix-in : scribble/html/extra))

(provide :feed-form)

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
