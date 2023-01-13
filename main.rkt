#lang racket

(require db)
(require xml)
(require xml/path)
(require net/url-string)
(require request/param)
(require gregor)

(require web-server/dispatch)
(require web-server/servlet)
(require web-server/servlet-env)
(require web-server/http/redirect)

(require (prefix-in h: scribble/html/xml))
(require (prefix-in h: scribble/html/html))
(require (prefix-in h: scribble/html/extra))


(define (string->datetime str)
  (let ([parse (lambda (format)
    (with-handlers
      ([exn:gregor:parse?
         (lambda (e) #f)])
      (parse-datetime str format)))])
    (or (parse "eee, d MMM y HH:mm:ss Z")
        (parse "eee,  d MMM y HH:mm:ss Z")
        (parse "eee, d MMM y HH:mm:ss 'GMT'")
        (parse "y-M-d HH:mm:ss"))))

(define (list->string xs [sep " "])
  (string-join (map ~a xs) sep))

(define (integer->boolean i)
  (eq? i 1))

(define (boolean->integer b)
  (if b 1 0))

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

(define (get-binding key req #:default [default null])
  (cdr (find-pair key (request-bindings req)
                  #:default (cons key default))))

(define-syntax-rule (unless condition action)
  (cond [(not condition) action]))


(struct feed (id link title enabled articles))
(struct article (id feedid link title date content archived))


(define (feed! url)
  (process-feed url (download-feed url)))

(define (download-feed url)
  (let* ([response (get (string->url url))]
         [body (http-response-body response)]
         [root (read-xml (open-input-string body))]
         [elem (document-element root)])
    (xml->xexpr elem)))

(define (process-feed url xexpr)
  (if (se-path* '(feed) xexpr)
    (process-atom-feed url xexpr)
    (process-rss-feed url xexpr)))

(define (process-atom-feed url xexpr)
  (let* ([title (se-path* '(feed title) xexpr)]
         [link (se-path* '(feed link #:href) xexpr)]
         [entry-data (se-path*/list '(feed) xexpr)]
         [entries (filter (lambda (entry)
                            (and (pair? entry) (eq? (car entry) 'entry))) entry-data)]
         [articles (let ([title null]
                         [link null]
                         [date null]
                         [content null])
                     (map (lambda (item)
                            (for ([part item] #:when (pair? part))
                              (let ([tag (car part)]
                                    [value (cddr part)])
                                (match tag
                                  ['title (set! title (car value))]
                                  ['link (set! link (list->string (se-path*/list '(link #:href) part)))]
                                  ['published (set! date (iso8601->datetime (car value)))]
                                  ['updated (set! date (iso8601->datetime (car value)))]
                                  ['summary (set! content (if (and (list? (cddr part)) (cdata? (caddr part)))
                                                         (cdata-string (caddr part))
                                                         (list->string (cddr part) "")))]
                                  [else null])))
                            (article null null link title date content #f)) entries))])
    (feed null (or url link) title #t articles)))

(define (process-rss-feed url xexpr)
  (let* ([title (se-path* '(rss channel title) xexpr)]
         [link (se-path* '(rss channel link) xexpr)]
         [item-data (se-path*/list '(rss channel) xexpr)]
         [items (filter (lambda (entry)
                          (and (pair? entry) (eq? (car entry) 'item))) item-data)]
         [articles (let ([title null]
                         [link null]
                         [date null]
                         [content null])
                     (map (lambda (item)
                            (for ([part item] #:when (pair? part))
                              (let ([tag (car part)])
                                (match tag
                                  ['title (set! title (caddr part))]
                                  ['link (set! link (caddr part))]
                                  ['pubDate (set! date (string->datetime (caddr part)))]
                                  ['description (set! content (if (and (list? (cddr part)) (cdata? (caddr part)))
                                                             (cdata-string (caddr part))
                                                             (list->string (cddr part) "")))]
                                  [else null])))
                            (article null null link title date content #f)) items))])
    (feed null (or url link) title #t articles)))


(define *pool*
  (connection-pool
    (lambda ()
      (sqlite3-connect #:database "feeder.db" #:mode 'create))))

(define *conn*
  (connection-pool-lease *pool*))

(define *page-size* 10)


(define query/feed-table-create
  "create table if not exists feeds (
    id integer primary key autoincrement,
    link text,
    title text,
    enabled boolean
  )")
(define stmt/feed-exists (prepare *conn* "select id from feeds where link = ?"))
(define stmt/feed-insert (prepare *conn* "insert into feeds (link, title, enabled) values (?, ?, ?) returning id"))
(define stmt/feed-select (prepare *conn* "select id, link, title, enabled from feeds"))
(define stmt/feed-select-by-id (prepare *conn* "select id, link, title, enabled from feeds where id = ? limit 1"))

(define query/article-table-create
  "create table if not exists articles (
    id integer primary key autoincrement,
    feed_id bigint,
    link text,
    title text,
    date datetime,
    content text,
    archived boolean
  )")
(define stmt/article-exists (prepare *conn* "select id from articles where link = ?"))
(define stmt/article-insert (prepare *conn* "insert into articles (feed_id, link, title, date, content, archived) values (?, ?, ?, ?, ?, ?) returning id"))
(define stmt/article-count-unarchived (prepare *conn* "select count(*) from articles where not archived"))
(define stmt/article-count-all (prepare *conn* "select count(*) from articles"))
(define stmt/article-select-unarchived-by-feedid (prepare *conn* "select id, feed_id, link, title, date, content, archived from articles where feed_id = ? and not archived"))
(define stmt/article-select-all-by-feedid (prepare *conn* "select id, feed_id, link, title, date, content, archived from articles where feed_id = ?"))
(define stmt/article-select-by-id (prepare *conn* "select id, feed_id, link, title, date, content, archived from articles where id = ? limit 1"))
(define stmt/article-select-unarchived (prepare *conn* "select id, feed_id, link, title, date, content, archived from articles where not archived limit ? offset ?"))
(define stmt/article-update-archive-by-id (prepare *conn* "update articles set archived = true where id = ?"))


(define (setup! conn)
  (query-exec conn query/feed-table-create)
  (query-exec conn query/article-table-create))

(define (record-exists conn stmt . args)
  (let ([query (bind-prepared-statement stmt args)])
    (not (false? (query-maybe-value conn query)))))

(define (insert-feed conn f)
  (if (or (not (null? (feed-id f))) (record-exists conn stmt/feed-exists (feed-link f)))
    f
    (let* ([url (feed-link f)]
           [title (feed-title f)]
           [enabled (boolean->integer (feed-enabled f))]
           [id (query-value conn stmt/feed-insert url title enabled)])
      (feed id url title (feed-enabled f) (feed-articles f)))))

(define (load-feeds conn #:with-articles [with-articles #t])
  (let ([rows (query-rows conn stmt/feed-select)])
    (map (lambda (row)
           (let* ([id (vector-ref row 0)]
                  [articles (if with-articles (load-articles-by conn #:feedid id) '())])
             (vector->feed row articles)))
         rows)))

(define (load-feed-by conn #:id id)
  (let ([row (query-row conn stmt/feed-select-by-id id)])
    (vector->feed row)))

(define (vector->feed vec [articles '()])
  (feed (vector-ref vec 0)
        (vector-ref vec 1)
        (vector-ref vec 2)
        (integer->boolean (vector-ref vec 3))
        articles))

(define (insert-article conn a f)
  (if (or (not (null? (article-id a))) (record-exists conn stmt/article-exists (article-link a)))
    a
    (let* ([feedid (feed-id f)]
           [url (article-link a)]
           [title (article-title a)]
           [date (~t (article-date a) "y-M-d HH:mm:ss")]
           [content (article-content a)]
           [archived (boolean->integer (article-archived a))])
      (article (query-value conn stmt/article-insert feedid url title date content archived)
               feedid url title date content (article-archived a)))))

(define (load-articles conn #:limit [limit *page-size*] #:offset [offset 0])
  (let ([rows (query-rows conn stmt/article-select-unarchived limit offset)])
    (map vector->article rows)))

(define (load-articles-by conn #:feedid feedid #:archived [archived #f])
  (let* ([query (if archived stmt/article-select-all-by-feedid stmt/article-select-unarchived-by-feedid)]
         [rows (query-rows conn query feedid)])
    (map vector->article rows)))

(define (load-article-by conn #:id id)
  (let ([row (query-row conn stmt/article-select-by-id id)])
    (vector->article row)))

(define (count-articles conn #:archived [archived #f])
  (let* ([query (if archived stmt/article-count-all stmt/article-count-unarchived)]
         [row (query-row conn query)])
    (vector-ref row 0)))

(define (archive-article-by conn #:id id)
  (query conn stmt/article-update-archive-by-id id))

(define (vector->article vec)
  (article (vector-ref vec 0)
           (vector-ref vec 1)
           (vector-ref vec 2)
           (vector-ref vec 3)
           (string->datetime (vector-ref vec 4))
           (vector-ref vec 5)
           (integer->boolean (vector-ref vec 6))))

(define css
  (port->string
    (open-input-file "styles.css")))

(define (view:page content)
  (h:xml->string
    (list (h:doctype 'html)
          (h:html
            (h:head
              (h:meta 'charset: "utf-8")
              (h:meta 'name: "viewport" 'content: "width=device-width, initial-scale=1.0")
              (h:title "feeder")
              (h:style css))
            (h:body
              (h:header
                (h:table
                  (h:tr
                    (h:td
                      (h:a 'href: "/" "feeder"))
                    (h:td
                      (h:a 'class: "add-feed" 'href: "/add" "+")))))
              (h:div 'class: "separator")
              (h:main content))))))

(define (view:add-feed)
  (view:page
    (h:form 'class: "add-feed-form"
            'method: "post"
            (h:input 'type: "url"
                     'name: "link"
                     'autofocus: "true"
                     'placeholder: "https://your.blog.net/feed.rss")
            (h:input 'type: "submit"
                     'value: "Add"))))

(define (view:article feed article)
  (let ([datetime (~t (article-date article) "y-M-d HH:mm:ss")]
        [humandate (~t (article-date article) "MMMM d, yyyy")])
    (view:page
      (h:article
        (h:h1 (h:a 'href: (article-link article) (article-title article)))
        (h:h4 (feed-title feed))
        (h:time 'datetime: datetime humandate)
        (h:p (h:literal (article-content article)))))))

(define (view:articles articles current-page page-count)
  (view:page
    (append
      (map (lambda (article)
             (partial:article-row null article)) articles)
      (page-links current-page page-count))))

(define (page-links current-page page-count)
  (let ([numbers (page-numbers current-page page-count)])
    (h:div 'class: "page-links"
           (and (> current-page 1)
                (h:a 'class: "page-link"
                     'href: (format "?page=~a" (- current-page 1)) "<"))
           (map (lambda (num)
                  (match num
                    ['skip (h:span 'class: "page-skip" "…")]
                    [else (h:a 'class: (if (eq? num current-page) "current-page page-link" "page-link")
                               'href: (format "?page=~a" num) num)])) numbers)
           (and (< current-page page-count)
                (h:a 'class: "page-link"
                     'href: (format "?page=~a" (+ current-page 1)) ">")))))

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
    (h:article 'class: "row"
               (h:h4
                 (h:a 'href: (format "/articles/~a" (article-id article))
                      (article-title article)))
               #;(h:h5 (feed-title feed))
               (h:p (string-chop (strip-html (article-content article)) 300 #:end "…"))
               (h:time 'datetime: datetime humandate)
               (h:a 'class: "pl1 action showonhover" 'href: (article-link article) 'target: "_blank" "read")
               #;(h:a 'class: "pl1 action showonhover" 'href: "#save" "save")
               (h:a 'class: "pl1 action showonhover" 'href: (format "/articles/~a/archive" (article-id article)) "archive"))))


(define (route:new-feed req)
  (response/output
    (lambda (op)
      (display (view:add-feed) op))))

(define (route:create-feed req)
  (let* ([link (get-binding 'link req)]
         [exists (record-exists *conn* stmt/feed-exists link)])
    (unless exists
      (let ([feed (insert-feed *conn* (feed! link))])
        (for ([article (feed-articles feed)])
          (insert-article *conn* article feed))
        (redirect-to "/articles" permanently)))
    (redirect-to "/articles" permanently)))

(define (route:arcticles req)
  (response/output
    (let* ([current-page (string->number (get-binding 'page req #:default "1"))]
           [page-count (ceiling (/ (count-articles *conn*) *page-size*))]
           [offset (* (- current-page 1) *page-size*)]
           [articles (load-articles *conn* #:offset offset)])
      (lambda (op)
        (display (view:articles articles current-page page-count) op)))))

(define (route:arcticle req id)
  (response/output
    (let* ([article (load-article-by *conn* #:id id)]
           [feed (load-feed-by *conn* #:id (article-feedid article))])
      (lambda (op)
        (display (view:article feed article) op)))))

(define (route:arcticle-archive req id)
  (begin
    (archive-article-by *conn* #:id id)
    (redirect-to "/articles" permanently)))

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
