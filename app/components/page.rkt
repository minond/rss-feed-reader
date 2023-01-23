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
           (:title "feeder")
           (:style (:literal css))
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

(define light-text (css-expr (apply rgb 83 83 83)))

(define css
  (css-expr->css
   (css-expr
    [@import "https://fonts.googleapis.com/css2?family=Libre+Baskerville:ital,wght@0,400;0,700;1,400&display=swap"]

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
            [table #:width 100%]
            [.actions #:text-align right
                      [a #:font-size 0.8em
                         #:margin-left 1.5em
                         #:color (apply rgb 66 66 66)
                         #:font-weight 100]]]
    [header main
            #:margin (0 auto)
            #:max-width 50em]
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
           #:border-color ((apply rgb 187 187 187)
                           (apply rgb 187 187 187)
                           (apply rgb 138 138 138)
                           (apply rgb 187 187 187))
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
                 #:max-width 50em
                 #:padding .75em]
    [.flash.alert #:border-bottom (1px solid (apply rgb 163 223 163))
                  #:background-color (apply rgb 245 255 245)]
    [.flash.notice #:border-bottom (1px solid (apply rgb 233 170 170))
                   #:background-color (apply rgb 255 240 240)]

    [.separator #:border-bottom (1px solid (apply rgb 223 223 223))]
    [.pl1 #:padding-left 1em]

    [article
     [time .action
           #:font-size 0.75em
           #:color ,@light-text]]
    [article.row #:min-height 9em
                 #:border-bottom (1px solid (apply rgb 235 235 235))
                 #:padding (2em 0)
                 [p #:font-size 0.9em
                    #:color ,@light-text]]
    [article.row
     [.showonhover #:opacity 0
                   #:transition (opacity .2s)]]
    [article.row:hover
     [.showonhover #:opacity 1]]

    [.page-links #:padding 1em
                 #:text-align center
                 #:overflow scroll]
    [.page-link #:margin (0 .75em)
                #:color (apply rgb 71 71 71)
                #:display inline-block
                #:min-width 20px]
    [.page-link.current #:color black
                        #:font-weight bold]
    [.page-skip #:margin (0 .5em)
                #:display inline-block]

    [.empty #:color red])))
