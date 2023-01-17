#lang racket

(require db
         threading
         deta
         gregor
         web-server/servlet
         web-server/servlet-env
         (prefix-in : scribble/html/xml)
         (prefix-in : scribble/html/html)
         (prefix-in : scribble/html/extra)
         (prefix-in rss: "rss.rkt"))


(define (integer->boolean i)
  (eq? i 1))

(define (string-chop str maxlen #:end [end ""])
  (if (<= (string-length str) maxlen)
    str
    (string-append (string-trim (substring str 0 maxlen)) end)))

(define (strip-html str)
  (regexp-replace* #rx"(<([^>]+)>)" str ""))

(define (reduce-by-pair f xs [id '()])
  (if (< (length xs) 2)
    (append id xs)
    (reduce-by-pair f (cdr xs) (f id (car xs) (cadr xs)))))

(define (find-pair key xs #:default [default null])
  (or (findf (lambda (x)
               (eq? key (car x))) xs)
      default))

(define (get-parameter key req)
  (get-binding key req #:default ""))

(define (get-binding key req #:default [default null])
  (cdr (find-pair key (request-bindings req)
                  #:default (cons key default))))

(define-syntax-rule (unless condition action)
  (cond [(not condition) action]))


(define *page-size* 10)

(define *pool*
  (connection-pool
    (lambda ()
      (sqlite3-connect #:database "feeder.db" #:mode 'create))))

(define *conn*
  (connection-pool-lease *pool*))

(define-schema feed
               ([id id/f #:primary-key #:auto-increment]
                [rss string/f #:contract non-empty-string?]
                [link string/f #:contract non-empty-string?]
                [title string/f #:contract non-empty-string?]
                [(enabled #t) boolean/f]))

(define-schema article
               ([id id/f #:primary-key #:auto-increment]
                [feedid id/f]
                [link string/f #:contract non-empty-string?]
                [title string/f #:contract non-empty-string?]
                [date datetime/f]
                [content string/f #:contract non-empty-string?]
                [(archived #f) boolean/f]))

(create-table! *conn* 'feed)
(create-table! *conn* 'article)

(define count-articles
  (~> (from article #:as a)
      (select (count a.id))))

(define (select-articles #:archived [archived #f] #:limit [lim *page-size*] #:offset [off 0])
  (~> (from article #:as a)
      (where (= a.archived ,archived))
      (offset ,off)
      (limit ,lim)))

(define (find-article-by-id id)
  (~> (from article #:as a)
      (where (= a.id ,id))
      (limit 1)))

(define (archive-article-by-id id)
  (~> (from article #:as a)
      (update [archived #t])
      (where (= id ,id))))

(define (find-feed-by-id id)
  (~> (from feed #:as f)
      (where (= f.id ,id))
      (limit 1)))

(define (find-feed-by-rss rss)
  (~> (from feed #:as f)
      (select 1)
      (where (= f.rss ,rss))
      (limit 1)))

(define css
  (port->string
    (open-input-file "styles.css")))

(define (view:page content)
  (:xml->string
    (list (:doctype 'html)
          (:html
            (:head
              (:meta 'charset: "utf-8")
              (:meta 'name: "viewport" 'content: "width=device-width, initial-scale=1.0")
              (:title "feeder")
              (:style css))
            (:body
              (:header
                (:table
                  (:tr
                    (:td
                      (:a 'href: "/" "feeder"))
                    (:td
                      (:a 'class: "add-feed" 'href: "/add" "+")))))
              (:div 'class: "separator")
              (:main content))))))

(define (view:add-feed)
  (view:page
    (:form 'class: "add-feed-form"
           'method: "post"
           (:input 'type: "url"
                   'name: "rss"
                   'autofocus: "true"
                   'placeholder: "https://your.blog.net/feed.rss")
           (:input 'type: "submit"
                   'value: "Add"))))

(define (view:article feed article)
  (let ([datetime (~t (article-date article) "y-M-d HH:mm:ss")]
        [humandate (~t (article-date article) "MMMM d, yyyy")])
    (view:page
      (:article
        (:h1 (:a 'href: (article-link article) (article-title article)))
        (:h4 (feed-title feed))
        (:time 'datetime: datetime humandate)
        (:p (:literal (article-content article)))))))

(define (view:articles articles current-page page-count)
  (view:page
    (append
      (map (lambda (article)
             (partial:article-row null article)) articles)
      (page-links current-page page-count))))

(define (page-links current-page page-count)
  (if (eq? page-count 1)
    null
    (let ([numbers (page-numbers current-page page-count)])
      (:div 'class: "page-links"
            (and (> current-page 1)
                 (:a 'class: "page-link"
                     'href: (format "?page=~a" (- current-page 1)) "<"))
            (map (lambda (num)
                   (match num
                     ['skip (:span 'class: "page-skip" "…")]
                     [else (:a 'class: (if (eq? num current-page) "current-page page-link" "page-link")
                               'href: (format "?page=~a" num) num)])) numbers)
            (and (< current-page page-count)
                 (:a 'class: "page-link"
                     'href: (format "?page=~a" (+ current-page 1)) ">"))))))

(define (page-numbers current-page page-count)
  (if (< page-count 10)
    (inclusive-range 1 page-count)
    (let* ([start (inclusive-range 1 3)]
           [middle (inclusive-range (- current-page 2) (+ current-page 2))]
           [end (inclusive-range (- page-count 3) page-count)]
           [whole (filter (lambda (num)
                            (and (positive? num) (<= num page-count)))
                          (sort (remove-duplicates (append start middle end)) <))])
      (reduce-by-pair (lambda (acc a b)
                        (if (eq? (- b a) 1)
                          (append acc (list a))
                          (append acc (list a 'skip)))) whole))))

(define (partial:article-row feed article)
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


(define (route:new-feed req)
  (response/output
    (lambda (op)
      (display (view:add-feed) op))))

(define (route:create-feed req)
  (let* ([rss (get-binding 'rss req)]
         [exists (integer->boolean (lookup *conn* (find-feed-by-rss rss)))])
    (unless exists
      (let* ([feed (rss:feed! rss)]
             [articles (rss:feed-articles feed)])
        (define saved-feed
          (insert-one! *conn* (make-feed #:rss (rss:feed-rss feed)
                                         #:link (rss:feed-link feed)
                                         #:title (rss:feed-title feed))))
        (apply insert! *conn* (map (lambda (article)
                                     (make-article #:feedid (feed-id saved-feed)
                                                   #:link (rss:article-link article)
                                                   #:title (rss:article-title article)
                                                   #:date (rss:article-date article)
                                                   #:content (rss:article-content article))) articles))))
    (redirect-to "/articles" permanently)))

(define (route:arcticles req)
  (response/output
    (let* ([current-page (or (string->number (get-parameter 'page req)) 1)]
           [page-count (ceiling (/ (lookup *conn* count-articles) *page-size*))]
           [offset (* (- current-page 1) *page-size*)]
           [articles (sequence->list (in-entities *conn* (select-articles #:offset offset)))])
      (lambda (op)
        (display (view:articles articles current-page page-count) op)))))

(define (route:arcticle req id)
  (response/output
    (let* ([article (lookup *conn* (find-article-by-id id))]
           [feed (lookup *conn* (find-feed-by-id (article-feedid article)))])
      (lambda (op)
        (display (view:article feed article) op)))))

(define (route:arcticle-archive req id)
  (query *conn* (archive-article-by-id id))
  (redirect-to "/articles" permanently))

(define-values (app-dispatch app-url)
  (dispatch-rules
    [("add") route:new-feed]
    [("add") #:method "post" route:create-feed]
    [("articles") route:arcticles]
    [("articles" (integer-arg)) route:arcticle]
    [("articles" (integer-arg) "archive") route:arcticle-archive]
    [else route:arcticles]))

(define (server req)
  (app-dispatch req))

(serve/servlet server
               #:launch-browser? #f
               #:servlet-path "/"
               #:port 8000
               #:servlet-regexp #rx"")
