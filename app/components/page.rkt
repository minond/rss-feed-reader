#lang racket

(require css-expr
         "../../lib/web/session.rkt"
         "../../lib/web/flash.rkt"
         (prefix-in : scribble/html/xml)
         (prefix-in : scribble/html/html)
         (prefix-in : scribble/html/extra))

(provide :page)

(define (:flash kind text)
  (:div 'class: (format "flash ~a" kind)
        (:div 'class: "flash-text" text)))

(define (:page content)
  (:xml->string
   (list (:doctype 'html)
         (:html
          (:head
           (:meta 'charset: "utf-8")
           (:meta 'name: "viewport"
                  'content: "width=device-width, initial-scale=1.0")
           (:title "Feeder")
           (:style (:literal css))
           (:body
            (:header
             (:table
              (:tr
               (:td
                (:a 'href: "/" "Feeder"))
               (:td 'class: "actions"
                    (if (authenticated?)
                        (list (:a 'href: "/feeds/new" "Add feed")
                              (:a 'href: "/feeds" "Manage feeds")
                              (:a 'href: "/sessions/destroy" "Sign out"))
                        null)))))
            (:div 'class: "separator")
            (let ([alert (read-flash 'alert)]
                  [notice (read-flash 'notice)])
              (list (and alert (:flash 'alert alert))
                    (and notice (:flash 'notice notice))))
            (:main content)
            (:script/inline 'type: "text/javascript" js)))))))

(define js
  (port->string (open-input-file "client/index.js")))

(define font-styles-url
  "https://fonts.googleapis.com/css2?family=Libre+Baskerville:ital,wght@0,400;0,700;1,400&display=swap")

(define border-color-light (css-expr (apply rgb 187 187 187)))
(define border-color-normal (css-expr (apply rgb 138 138 138)))
(define failure-color-light (css-expr (apply rgb 255 240 240)))
(define failure-color-normal (css-expr (apply rgb 233 170 170)))
(define separator-color-light (css-expr (apply rgb 235 235 235)))
(define separator-color-normal (css-expr (apply rgb 223 223 223)))
(define success-color-light (css-expr (apply rgb 245 255 245)))
(define success-color-normal (css-expr (apply rgb 163 223 163)))
(define text-color-light (css-expr (apply rgb 83 83 83)))

(define content-max-width (css-expr 50em))

(define css
  (css-expr->css
   (css-expr
    [@import ,font-styles-url]

    [body #:cursor default
          #:font-family "Libre Baskerville"
          #:margin 0
          #:line-height 1.6
          #:font-size 16px
          #:color |#444|
          #:padding 0]
    [main #:padding 1em]
    [header #:font-weight bold
            #:padding (.5em 1em)
            [table #:width 100%
                   #:border-collapse collapse]
            [.actions #:text-align right
                      [a #:font-size 0.8em
                         #:margin-left 1.5em
                         #:color ,@text-color-light
                         #:font-weight 100]]]
    [header main
            #:margin (0 auto)
            #:max-width ,@content-max-width]
    [main
     [table #:width 100%
            #:border-collapse collapse
            [td th
                #:padding 0.75em
                #:margin 0]]
     [th #:text-align left
         #:font-weight 900]
     [td #:border-top (1px solid ,@border-color-light)]]
    [a #:color initial
       #:text-decoration none]
    [a:hover #:text-decoration underline]
    [h1 h2 h3
        #:line-height 1.2]
    [h4 h5
        #:margin (0 0 1em 0)]
    [form #:margin (3em auto)
          #:max-width 40em
          [(attribute input (= type "url"))
           (attribute input (= type "input"))
           (attribute input (= type "email"))
           (attribute input (= type "password"))
           #:border-width (1px 1px 1.5px 1px)
           #:border-style solid
           #:border-color (,@border-color-light
                           ,@border-color-light
                           ,@border-color-normal
                           ,@border-color-light)
           #:padding 0.5em
           #:width 100%
           #:font-size 1.1em
           #:margin (0.25em 0)]
          [(attribute input (= type "button"))
           (attribute input (= type "submit"))
           (attribute input (= type "cancel"))
           #:font-size 1.1em
           #:margin-top 0.5em
           #:margin-righ: 0.5em]
          [a #:font-size 0.8em]]

    [.flash-text #:margin (0 auto)
                 #:max-width ,@content-max-width
                 #:padding .75em]
    [.flash.alert #:border-bottom (1px solid ,@success-color-normal)
                  #:background-color ,@success-color-light]
    [.flash.notice #:border-bottom (1px solid ,@failure-color-normal)
                   #:background-color ,@failure-color-light]

    [.separator #:border-bottom (1px solid ,@separator-color-normal)]
    [.spacer #:display inline-block]
    [.spacer.vertical.small #:height 1em]
    [.spacer.vertical.medium #:height 2em]
    [.spacer.vertical.large #:height 4em]
    [.spacer.horizontal.small #:width 1em]
    [.spacer.horizontal.medium #:width 2em]
    [.spacer.horizontal.large #:width 4em]
    [.tc #:text-align center]

    [.feed-subscription-toggle #:height .9em
                               #:width .9em
                               #:border-radius .9em
                               #:border (1px solid ,@border-color-normal)
                               #:margin (0 auto)
                               #:padding 0
                               #:display block
                               #:transition (background-color 100ms)]
    [.feed-subscription-toggle.subscribed #:background-color ,@success-color-normal]
    [.feed-subscription-toggle.unsubscribed #:background-color ,@failure-color-normal]
    [.feed-subscription-toggle.subscribed:hover #:background-color ,@failure-color-normal]
    [.feed-subscription-toggle.unsubscribed:hover #:background-color ,@success-color-normal]

    [article
     [time .action
           #:font-size 0.75em
           #:color ,@text-color-light]]
    [article.row #:border-bottom (1px solid ,@separator-color-light)
                 #:padding (2em 0)
                 [p #:font-size 0.9em
                    #:color ,@text-color-light]]
    [article.row
     [.showonhover #:opacity 0
                   #:transition (opacity .2s)]]
    [article.row:hover
     [.showonhover #:opacity 1]]

    [.page-links #:padding 1em
                 #:text-align center
                 #:overflow scroll]
    [.page-link #:margin (0 .75em)
                #:color ,@text-color-light
                #:display inline-block
                #:min-width 20px]
    [.page-link.current #:color black
                        #:font-weight bold]
    [.page-skip #:margin (0 .5em)
                #:display inline-block])))
