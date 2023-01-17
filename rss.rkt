#lang racket

(require xml
         xml/path
         net/url-string
         request/param
         gregor)

(provide feed! feed feed-id feed-rss feed-link feed-title feed-enabled
         feed-articles article article-id article-feedid article-link
         article-title article-date article-content article-archived)

(define (feed! rss)
  (process rss (download rss)))

; (struct feed (url link title articles))
; (struct article (link title date content))
(struct feed (id rss link title enabled articles))
(struct article (id feedid link title date content archived))

(define (download rss)
  (let* ([response (get (string->url rss))]
         [body (http-response-body response)]
         [root (read-xml (open-input-string body))]
         [elem (document-element root)])
    (xml->xexpr elem)))

(define (process rss xexpr)
  (if (se-path* '(feed) xexpr)
    (process-atom rss xexpr)
    (process-rss rss xexpr)))

(define (process-atom rss xexpr)
  (let* ([title (se-path* '(feed title) xexpr)]
         [link (se-path* '(feed link #:href) xexpr)]
         [entry-data (se-path*/list '(feed) xexpr)]
         [entries (filter (lambda (entry)
                            (and (pair? entry)
                                 (eq? (car entry) 'entry))) entry-data)]
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
    (feed null rss link title #t articles)))

(define (process-rss rss xexpr)
  (let* ([title (se-path* '(rss channel title) xexpr)]
         [link (se-path* '(rss channel link) xexpr)]
         [item-data (se-path*/list '(rss channel) xexpr)]
         [items (filter (lambda (entry)
                          (and (pair? entry)
                               (eq? (car entry) 'item))) item-data)]
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
    (feed null rss link title #t articles)))

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
