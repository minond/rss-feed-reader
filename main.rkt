#lang racket

(require db)
(require xml)
(require xml/path)
(require net/url-string)
(require request/param)
(require gregor)


(define (string->datetime str)
  (let ([parse (lambda (format)
    (with-handlers
      ([exn:gregor:parse?
         (lambda (e) #f)])
      (parse-datetime str format)))])
    (or (parse "eee, d MMM y HH:mm:ss Z")
        (parse "eee,  d MMM y HH:mm:ss Z")
        (parse "eee, d MMM y HH:mm:ss 'GMT'"))))

(define (list->string xs [sep " "])
  (string-join (map ~a xs) sep))


(struct feed (id link title enabled articles))
(struct article (id feedid link title date content archived))


(define (feed! url)
  (process-feed (download-feed url)))

(define (download-feed url)
  (let* ([response (get (string->url url))]
         [body (http-response-body response)]
         [root (read-xml (open-input-string body))]
         [elem (document-element root)])
    (xml->xexpr elem)))

(define (process-feed xexpr)
  (if (se-path* '(feed) xexpr)
    (process-atom-feed xexpr)
    (process-rss-feed xexpr)))

(define (process-atom-feed xexpr)
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
    (feed null link title #t articles)))

(define (process-rss-feed xexpr)
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
    (feed null link title #t articles)))


(define *pool*
  (connection-pool
    (lambda ()
      (sqlite3-connect #:database "feeder.db" #:mode 'create))))

(define *conn*
  (connection-pool-lease *pool*))


(define query/feed-table-create
  "create table if not exists feeds (
    id integer primary key autoincrement,
    link text,
    title text,
    enabled boolean
  )")
(define stmt/feed-exists (prepare *conn* "select id from feeds where id = ?"))
(define stmt/feed-insert (prepare *conn* "insert into feeds (link, title, enabled) values (?, ?, ?) returning id"))
(define stmt/feed-select (prepare *conn* "select id, link, title, enabled from feeds"))

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
(define stmt/article-exists (prepare *conn* "select id from articles where id = ?"))


(define (setup! conn)
  (query-exec conn query/feed-table-create)
  (query-exec conn query/article-table-create))

(define (record-exists conn stmt id)
  (if (null? id)
    #f
    (let ([query (bind-prepared-statement stmt (list id))])
      (not (false? (query-maybe-value conn query))))))

(define (insert-feed conn f)
  (if (record-exists conn stmt/feed-exists (feed-id f))
    f
    (let* ([url (feed-link f)]
           [title (feed-title f)]
           [enabled (if (feed-enabled f) 1 0)]
           [id (query-value conn stmt/feed-insert url title enabled)])
      (feed id url title (feed-enabled f) (feed-articles f)))))

(define (load-feeds conn)
  (let ([rows (query-rows conn stmt/feed-select)])
    (map (lambda (row)
           (feed (vector-ref row 0)
                 (vector-ref row 1)
                 (vector-ref row 2)
                 (vector-ref row 3)
                 '())) rows)))
