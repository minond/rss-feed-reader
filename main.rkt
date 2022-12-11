#lang racket

(require xml)
(require xml/path)
(require net/url-string)
(require request/param)
(require gregor)

(define (feed! url)
  (process-feed (download-feed url)))

(struct feed (title link articles))
(struct article (title link date text))

(define (download-feed url)
  (let* ([response (get (string->url url))]
         [body (http-response-body response)]
         [root (read-xml (open-input-string body))]
         [elem (document-element root)])
    (xml->xexpr elem)))

(define (process-atom-feed xexpr)
  (let* ([title (se-path* '(feed title) xexpr)]
         [link (se-path* '(feed link #:href) xexpr)]
         [entry-data (se-path*/list '(feed) xexpr)]
         [entries (filter (lambda (entry)
                            (and (pair? entry) (eq? (car entry) 'entry))) entry-data)]
         [articles (let ([title null]
                         [link null]
                         [date null]
                         [text null])
                     (map (lambda (item)
                            (for ([part item] #:when (pair? part))
                              (let ([tag (car part)]
                                    [value (cddr part)])
                                (match tag
                                  ['title (set! title (car value))]
                                  ['link (set! link (list->string (se-path*/list '(link #:href) part)))]
                                  ['published (set! date (iso8601->datetime (car value)))]
                                  ['updated (set! date (iso8601->datetime (car value)))]
                                  ['summary (set! text (if (and (list? (cddr part)) (cdata? (caddr part)))
                                                         (cdata-string (caddr part))
                                                         (list->string (cddr part) "")))]
                                  [else null])))
                            (article title link date text)) entries))])
    (feed title link articles)))

(define (process-rss-feed xexpr)
  (let* ([title (se-path* '(rss channel title) xexpr)]
         [link (se-path* '(rss channel link) xexpr)]
         [item-data (se-path*/list '(rss channel) xexpr)]
         [items (filter (lambda (entry)
                          (and (pair? entry) (eq? (car entry) 'item))) item-data)]
         [articles (let ([title null]
                         [link null]
                         [date null]
                         [text null])
                     (map (lambda (item)
                            (for ([part item] #:when (pair? part))
                              (let ([tag (car part)])
                                (match tag
                                  ['title (set! title (caddr part))]
                                  ['link (set! link (caddr part))]
                                  ['pubDate (set! date (string->datetime (caddr part)))]
                                  ['description (set! text (if (and (list? (cddr part)) (cdata? (caddr part)))
                                                             (cdata-string (caddr part))
                                                             (list->string (cddr part) "")))]
                                  [else null])))
                            (article title link date text)) items))])
    (feed title link articles)))

(define (process-feed xexpr)
  (if (se-path* '(feed) xexpr)
    (process-atom-feed xexpr)
    (process-rss-feed xexpr)))

(define (string->datetime str)
  (let ([parse (lambda (format)
    (with-handlers
      ([exn:gregor:parse?
         (lambda (e) #f)])
      (parse-datetime str format)))])
    (or (format "eee, d MMM y HH:mm:ss Z")
        (format "eee,  d MMM y HH:mm:ss Z")
        (format "eee, d MMM y HH:mm:ss 'GMT'"))))

(define (list->string xs [sep " "])
  (string-join (map ~a xs) sep))

(define urls '("https://bernsteinbear.com/feed.xml"
               "https://2ality.com/feeds/posts.atom"
               "https://eli.thegreenplace.net/feeds/all.atom.xml"
               "https://matt.might.net/articles/feed.rss"
               "https://blog.regehr.org/feed"
               "https://feeds.feedburner.com/martinkl"
               "http://steve-yegge.blogspot.com/feeds/posts/default?alt=rss"
               "https://stevelosh.com/rss.xml"
               "http://lambda-the-ultimate.org/rss.xml"
               "https://esoteric.codes/rss"))

(for ([url urls])
  (define feed (feed! url))
  (printf "---------------------------------------------\n")
  (printf "title: ~A\n" (feed-title feed))
  (printf "link: ~A\n" (feed-link feed))
  (printf "feed_url: ~A\n" url)
  (for ([article (feed-articles feed)])
    (printf "title: ~A\n" (article-title article))
    (printf "date: ~A\n" (article-date article))
    (printf "link: ~A\n" (article-link article))
    (printf "text: ~A\n" (article-text article)))
  (printf "---------------------------------------------\n"))
