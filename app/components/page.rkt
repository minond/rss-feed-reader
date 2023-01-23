#lang racket

(require "../../lib/web/session.rkt"
         "../../lib/web/flash.rkt"
         (prefix-in : scribble/html/xml)
         (prefix-in : scribble/html/html)
         (prefix-in : scribble/html/extra))

(provide :page)

(define (:flash kind text)
  (:div 'class: (format "flash ~a" kind)
        (:div 'class: "flash-text" text)))

(define (:page content)
  (let ([css (port->string (open-input-file "styles.css"))])
    (:xml->string
     (list (:doctype 'html)
           (:html
            (:head
             (:meta 'charset: "utf-8")
             (:meta 'name: "viewport"
                    'content: "width=device-width, initial-scale=1.0")
             (:title "feeder")
             (:style css))
            (:body
             (:header
              (:table
               (:tr
                (:td
                 (:a 'href: "/" "feeder"))
                (:td 'class: "actions"
                     (if (authenticated?)
                         (list (:a 'href: "/feeds/new" "Add feed")
                               (:a 'href: "/sessions/destroy" "Sign out"))
                         null)))))
             (:div 'class: "separator")
             (let ([alert (read-flash 'alert)]
                   [notice (read-flash 'notice)])
               (list (and alert (:flash 'alert alert))
                     (and notice (:flash 'notice notice))))
             (:main content)))))))
