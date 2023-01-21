#lang racket

(require racket/random
         crypto
         crypto/libcrypto
         db
         threading
         deta
         deta/reflect
         gregor
         web-server/servlet
         web-server/servlet-env
         web-server/http/cookie
         web-server/http/id-cookie
         (prefix-in : scribble/html/xml)
         (prefix-in : scribble/html/html)
         (prefix-in : scribble/html/extra)
         (prefix-in rss: "rss.rkt"))

(provide start)

(crypto-factories libcrypto-factory)

(define (date->rfc7231 date)
  (~t date "E, d MMM yyyy 00:00:00"))

(define (string-chop str maxlen #:end [end ""])
  (if (<= (string-length str) maxlen)
      str
      (string-append (string-trim (substring str 0 maxlen)) end)))

(define (strip-xml str)
  (~> str
      (string-replace _ "<![CDATA[" "" #:all? #t)
      (string-replace _ "]]>" "" #:all? #t)))

(define (strip-html str)
  (~> str
      (strip-xml _)
      (regexp-replace* #rx"(<([^>]+)>)" _ "")))

(define (reduce-by-pair f xs [id '()])
  (if (< (length xs) 2)
      (append id xs)
      (reduce-by-pair f (cdr xs) (f id (car xs) (cadr xs)))))

(define (find-pair key xs #:default [default null])
  (or (findf (lambda (x)
               (eq? key (car x))) xs)
      default))

(define (get-parameter key req #:default [default ""])
  (get-binding key req #:default default))

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

; For interactive mode
(schema-registry-allow-conflicts? #t)

(define-schema user
  ([id id/f #:primary-key #:auto-increment]
   [email string/f #:unique #:contract non-empty-string?]
   [encrypted-password binary/f]
   [salt binary/f]))

(define-schema feed
  ([id id/f #:primary-key #:auto-increment]
   [user-id id/f]
   [rss string/f #:unique #:contract non-empty-string?]
   [link string/f #:contract non-empty-string?]
   [title string/f #:contract non-empty-string?]
   [(enabled #t) boolean/f]))

(define-schema article
  ([id id/f #:primary-key #:auto-increment]
   [user-id id/f]
   [feed-id id/f]
   [link string/f #:unique #:contract non-empty-string?]
   [title string/f #:contract non-empty-string?]
   [date datetime/f]
   [content string/f #:contract non-empty-string?]
   [(archived #f) boolean/f]))

(create-table! *conn* 'user)
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

(define (find-user-by-email email)
  (~> (from user #:as u)
      (where (= email ,email))
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

(define (:login-form [email null] [show-error #f])
  (:form 'action: "/sessions/create"
         'method: "post"
         (and show-error
              (:div 'class: "error-message"
                    "Invalid credentials, please try again."))
         (:input 'type: "email"
                 'name: "email"
                 'value: email
                 'required: "true"
                 'autofocus: "true"
                 'autocapitalize: "false"
                 'placeholder: "Email")
         (:input 'type: "password"
                 'name: "password"
                 'required: "true"
                 'placeholder: "Password")
         (:input 'type: "submit"
                 'value: "Login")
         (:a 'href: "/users/new"
             "or register instead")))

(define (:user-form [email null] [password-mismatch null])
  (:form 'action: "/users/create"
         'method: "post"
         (and password-mismatch
              (:div 'class: "error-message"
                    "Your password and confirmation did not match, please try again."))
         (:input 'type: "email"
                 'name: "email"
                 'value: email
                 'required: "true"
                 'autofocus: "true"
                 'autocapitalize: "false"
                 'placeholder: "Email")
         (:input 'type: "password"
                 'name: "password"
                 'required: "true"
                 'placeholder: "Password")
         (:input 'type: "password"
                 'name: "password-confirm"
                 'required: "true"
                 'placeholder: "Password confirmation")
         (:input 'type: "submit"
                 'value: "Register")
         (:a 'href: "/sessions/new"
             "or login instead")))

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

(define (:article-full feed article)
  (let ([datetime (~t (article-date article) "y-M-d HH:mm:ss")]
        [humandate (~t (article-date article) "MMMM d, yyyy")])
    (:article
     (:h1 (:a 'href: (article-link article) (article-title article)))
     (:h4 (feed-title feed))
     (:time 'datetime: datetime humandate)
     (:p (:literal (strip-xml (article-content article)))))))

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


(define (/sessions/new req)
  (let ([email (get-parameter 'email req)]
        [show-error (get-parameter 'invalid req #:default #f)])
    (render (:login-form email show-error))))

(define (/sessions/create req)
  (let* ([email (get-parameter 'email req)]
         [password (get-parameter 'password req)]
         [user (lookup *conn* (find-user-by-email email))])
    (if (and user (check-password password user))
        (redirect-to "/articles" permanently
                     #:headers (list
                                (cookie->header
                                 (create-session-cookie #:user-id (user-id user)))))
        (redirect-to
         (format "/sessions/new?email=~a&invalid=true" email)
         permanently))))

(define (/sessions/destroy req)
  (destroy-session req)
  (redirect-to "/sessions/new" permanently
               #:headers (list
                          (cookie->header
                           (clear-session-coookie)))))

(define (/users/new req)
  (let ([email (get-parameter 'email req)]
        [password-mismatch (get-parameter 'password-mismatch req #:default #f)])
    (render (:user-form email password-mismatch))))

(define (/users/create req)
  (let* ([email (get-parameter 'email req)]
         [password (get-parameter 'password req)]
         [password-confirm (get-parameter 'password-confirm req)])
    (if (not (equal? password password-confirm))
        (redirect-to
         (format "/users/new?email=~a&password-mismatch=true" email)
         permanently)
        (begin
          (let-values ([(encrypted-password salt) (make-password password)])
            (define user
              (insert-one! *conn*
                           (make-user #:email email
                                      #:salt salt
                                      #:encrypted-password encrypted-password)))
            (define session-cookie
              (create-session-cookie #:user-id (user-id user)))
            (redirect-to "/feeds/new" permanently
                         #:headers (list
                                    (cookie->header session-cookie))))))))

(define (/feeds/new req)
  (if (not (authenticated? req))
      (redirect-to "/sessions/new")
      (render (:feed-form))))

(define (/feeds/create req)
  (if (not (authenticated? req))
      (redirect-to "/sessions/new")
      (let* ([session (lookup-session req)]
             [user-id (session-user-id session)]
             [rss (get-binding 'rss req)]
             [exists (lookup *conn* (find-feed-by-rss rss))])
        (unless exists
          (schedule-feed-download user-id rss))
        (redirect-to "/articles" permanently))))

(define (/articles req)
  (if (not (authenticated? req))
      (redirect-to "/sessions/new")
      (let* ([current-page (or (string->number (get-parameter 'page req)) 1)]
             [page-count (ceiling (/ (lookup *conn* count-articles) *page-size*))]
             [offset (* (- current-page 1) *page-size*)]
             [articles (sequence->list (in-entities *conn* (select-articles #:offset offset)))])
        (render (:articles-list articles current-page page-count)))))

(define (/arcticles/show req id)
  (if (not (authenticated? req))
      (redirect-to "/sessions/new")
      (let* ([article (lookup *conn* (find-article-by-id id))]
             [feed (lookup *conn* (find-feed-by-id (article-feed-id article)))])
        (render (:article-full feed article)))))

(define (/articles/archive req id)
  (if (not (authenticated? req))
      (redirect-to "/sessions/new")
      (begin
        (query *conn* (archive-article-by-id id))
        (redirect-to "/articles" permanently))))

(define-values (app-dispatch app-url)
  (dispatch-rules
   [("sessions" "new") /sessions/new]
   [("sessions" "create") #:method "post" /sessions/create]
   [("sessions" "destroy") #:method "delete"  /sessions/destroy]
   [("sessions" "destroy") /sessions/destroy]
   [("users" "new") /users/new]
   [("users" "create") #:method "post" /users/create]
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

(define (render content)
  (response/output
   (lambda (op)
     (display (:page content) op))))

(define (schedule-feed-download user-id rss)
  (thread-send feed-download-thread (list user-id rss)))

(define feed-download-thread
  (thread
   (lambda ()
     (let loop ()
       (match-define (list user-id rss) (thread-receive))
       (define feed (rss:feed! rss))

       (printf "saving feed ~a\n" rss)
       (define saved-feed
         (or
          (lookup *conn* (find-feed-by-rss rss))
          (insert-one! *conn* (make-feed #:user-id user-id
                                         #:rss (rss:feed-rss feed)
                                         #:link (rss:feed-link feed)
                                         #:title (rss:feed-title feed)))))
       (printf "feed id: ~a\n" (feed-id saved-feed))

       (for ([article (rss:feed-articles feed)])
         (unless (lookup *conn* (find-article-by-link (rss:article-link article)))
           (printf "saving article ~a\n" (rss:article-link article))
           (insert-one! *conn* (make-article #:user-id user-id
                                             #:feed-id (feed-id saved-feed)
                                             #:link (rss:article-link article)
                                             #:title (rss:article-title article)
                                             #:date (rss:article-date article)
                                             #:content (rss:article-content article)))))
       (printf "done processing ~a\n" rss)
       (loop)))))

(define charset
  (map integer->char
       (append (inclusive-range 48 57)
               (inclusive-range 65 90)
               (inclusive-range 97 122))))

(define (random-item xs)
  (sequence-ref xs (random (sequence-length xs))))

(define (random-string [len 32])
  (list->string
   (map (lambda (x)
          (random-item charset))
        (make-list len 0))))

(define session-cookie-name "session")
(define sessions (make-hash))
(struct session (user-id))

(define (get-session-cookie req)
  (findf (lambda (cookie)
           (equal? session-cookie-name
                   (client-cookie-name cookie)))
         (request-cookies req)))

(define (lookup-session req)
  (let/cc return
    (define session-cookie (get-session-cookie req))
    (unless session-cookie
      (return #f))
    (hash-ref sessions
              (client-cookie-value session-cookie)
              #f)))

(define (destroy-session req)
  (let/cc return
    (define session-cookie (get-session-cookie req))
    (unless session-cookie
      (return #f))
    (hash-remove! sessions
                  (client-cookie-value session-cookie))))

(define (create-session #:user-id user-id)
  (let ([key (random-string)]
        [data (session user-id)])
    (hash-set! sessions key data)
    key))

(define (create-session-cookie #:user-id user-id)
  (make-cookie session-cookie-name
               (create-session #:user-id user-id)
               #:path "/"
               #:expires (date->rfc7231 (+years (today/utc) 1))))

(define (clear-session-coookie)
  (logout-id-cookie session-cookie-name #:path "/"))

(define (authenticated? req)
  (not (eq? #f (lookup-session req))))

(define (make-password password #:salt [salt (crypto-random-bytes 128)])
  (let* ([bytes (string->bytes/utf-8 password)]
         [enc (scrypt bytes salt #:N (expt 2 14))])
    (values enc salt)))

(define (check-password password user)
  (let*-values ([(salt) (user-salt user)]
                [(enc _) (make-password password #:salt salt)])
    (equal? enc (user-encrypted-password user))))
