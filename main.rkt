#lang racket

(require db
         threading
         deta
         deta/reflect
         gregor
         web-server/servlet
         web-server/servlet-env
         (prefix-in : scribble/html/xml)
         (prefix-in : scribble/html/html)
         (prefix-in : scribble/html/extra)
         (prefix-in rss: "rss.rkt"))

(provide start)


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


(define *page-size* 10)

(define *pool*
  (connection-pool
    (lambda ()
      (sqlite3-connect #:database "feeder.db" #:mode 'create))))

(define *conn*
  (connection-pool-lease *pool*))

; Needed to we can reload this module in the repl
(schema-registry-allow-conflicts? #t)

(define-schema feed
               ([id id/f #:primary-key #:auto-increment]
                [rss string/f #:unique #:contract non-empty-string?]
                [link string/f #:contract non-empty-string?]
                [title string/f #:contract non-empty-string?]
                [(enabled #t) boolean/f]))

(define-schema article
               ([id id/f #:primary-key #:auto-increment]
                [feedid id/f]
                [link string/f #:unique #:contract non-empty-string?]
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
      (order-by ([date #:desc]))
      (offset ,off)
      (limit ,lim)))

(define (find-article-by-id id)
  (~> (from article #:as a)
      (where (= a.id ,id))
      (limit 1)))

(define (find-article-by-link link)
  (~> (from article #:as a)
      (where (= a.link ,link))
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
      (where (= f.rss ,rss))
      (limit 1)))

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
                      (:td
                        (:a 'class: "add-feed"
                            'href: "/feeds/new" "+")))))
                (:div 'class: "separator")
                (:main content)))))))

(define (:feed-form)
  (:form 'class: "add-feed-form"
         'action: "/feeds/create"
         'method: "post"
         (:input 'type: "url"
                 'name: "rss"
                 'autofocus: "true"
                 'placeholder: "https://your.blog.net/feed.rss")
         (:input 'type: "submit"
                 'value: "Add")))

(define (:article-full feed article)
  (let ([datetime (~t (article-date article) "y-M-d HH:mm:ss")]
        [humandate (~t (article-date article) "MMMM d, yyyy")])
    (:article
      (:h1 (:a 'href: (article-link article) (article-title article)))
      (:h4 (feed-title feed))
      (:time 'datetime: datetime humandate)
      (:p (:literal (article-content article))))))

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

(define (:pagination current-page page-count)
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


(define (/feeds/new req)
  (response/output
    (lambda (op)
      (display (:page (:feed-form)) op))))

(define (/feeds/create req)
  (let* ([rss (get-binding 'rss req)]
         [exists (lookup *conn* (find-feed-by-rss rss))])
    (unless exists
      (schedule-feed-download rss))
    (redirect-to "/articles" permanently)))

(define (/articles req)
  (response/output
    (let* ([current-page (or (string->number (get-parameter 'page req)) 1)]
           [page-count (ceiling (/ (lookup *conn* count-articles) *page-size*))]
           [offset (* (- current-page 1) *page-size*)]
           [articles (sequence->list (in-entities *conn* (select-articles #:offset offset)))])
      (lambda (op)
        (display (:page (:articles-list articles current-page page-count)) op)))))

(define (/arcticles/show req id)
  (response/output
    (let* ([article (lookup *conn* (find-article-by-id id))]
           [feed (lookup *conn* (find-feed-by-id (article-feedid article)))])
      (lambda (op)
        (display (:page (:article-full feed article)) op)))))

(define (/articles/archive req id)
  (query *conn* (archive-article-by-id id))
  (redirect-to "/articles" permanently))

(define-values (app-dispatch app-url)
  (dispatch-rules
    [("feeds" "new") /feeds/new]
    [("feeds" "create") #:method "post" /feeds/create]
    [("articles") /articles]
    [("articles" (integer-arg)) /arcticles/show]
    [("articles" (integer-arg) "archive") /articles/archive]
    [else /articles]))

(define (server req)
  (app-dispatch req))

(define (start)
  (serve/servlet server
                 #:launch-browser? #f
                 #:servlet-path "/"
                 #:port 8000
                 #:servlet-regexp #rx""))

(define (schedule-feed-download rss)
  (thread-send feed-download-thread rss))

(define feed-download-thread
  (thread
    (lambda ()
      (let loop ()
        (define rss (thread-receive))
        (define feed (rss:feed! rss))

        (printf "saving feed ~a\n" rss)
        (define saved-feed
          (or
            (lookup *conn* (find-feed-by-rss rss))
            (insert-one! *conn* (make-feed #:rss (rss:feed-rss feed)
                                           #:link (rss:feed-link feed)
                                           #:title (rss:feed-title feed)))))
        (printf "feed id: ~a\n" (feed-id saved-feed))

        (for ([article (rss:feed-articles feed)])
          (unless (lookup *conn* (find-article-by-link (rss:article-link article)))
            (printf "saving article ~a\n" (rss:article-link article))
            (insert-one! *conn* (make-article #:feedid (feed-id saved-feed)
                                              #:link (rss:article-link article)
                                              #:title (rss:article-title article)
                                              #:date (rss:article-date article)
                                              #:content (rss:article-content article)))))
        (printf "done processing ~a\n" rss)
        (loop)))))
