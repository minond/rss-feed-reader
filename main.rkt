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
         (prefix-in rss: "lib/rss.rkt")
         "lib/pair.rkt"
         "lib/string.rkt"
         "lib/crypto.rkt"
         "lib/web/utils.rkt"
         "lib/web/session.rkt"
         "lib/web/flash.rkt"
         "app/models.rkt")

(provide start)

(define *page-size* 10)

(define *pool*
  (connection-pool
   (lambda ()
     (sqlite3-connect #:database "feeder.db" #:mode 'create))))

(define *conn*
  (connection-pool-lease *pool*))

(create-table! *conn* 'user)
(create-table! *conn* 'feed)
(create-table! *conn* 'article)

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

(define (:flash kind text)
  (:div 'class: (format "flash ~a" kind)
        (:div 'class: "flash-text" text)))

(define (:login-form [email null])
  (:form 'action: "/sessions/create"
         'method: "post"
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

(define (:user-form [email null])
  (:form 'action: "/users/create"
         'method: "post"
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
  (let* ([email (get-parameter 'email req)])
    (render :page (:login-form email))))

(define (/sessions/create req)
  (let* ([email (get-parameter 'email req)]
         [password (get-parameter 'password req)]
         [user (lookup *conn* (find-user-by-email email))])
    (if (and user (check-password #:unencrypted password
                                  #:encrypted (user-encrypted-password user)
                                  #:salt (user-salt user)))
        (redirect-to "/articles" permanently
                     #:headers (list
                                (cookie->header
                                 (create-session-cookie #:user-id (user-id user)))))
        (with-flash #:notice "Invalid credentials, please try again."
          (redirect (format "/sessions/new?email=~a" email))))))

(define (/sessions/destroy req)
  (destroy-session req)
  (redirect-to "/sessions/new" permanently
               #:headers (list
                          (cookie->header
                           (clear-session-coookie)))))

(define (/users/new req)
  (let* ([email (get-parameter 'email req)])
    (render :page (:user-form email))))

(define (/users/create req)
  (let* ([email (get-parameter 'email req)]
         [password (get-parameter 'password req)]
         [password-confirm (get-parameter 'password-confirm req)])
    (if (not (equal? password password-confirm))
        (with-flash #:notice "Your password and confirmation did not match, please try again."
          (redirect (format "/users/new?email=~a" email)))
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
                                  (cookie->header session-cookie)))))))

(define (/feeds/new req)
  (if (not (authenticated? req))
      (redirect "/sessions/new")
      (render :page (:feed-form))))

(define (/feeds/create req)
  (if (not (authenticated? req))
      (redirect "/sessions/new")
      (let* ([rss (get-binding 'rss req)]
             [exists (lookup *conn* (find-feed-by-rss #:user-id (current-user-id)
                                                      #:rss rss))])
        (unless exists
          (schedule-feed-download (current-user-id) rss))
        (with-flash #:alert (and (not exists) "Downloading feed data and articles.")
          #:notice (and exists "This feed already exists.")
          (redirect "/articles")))))

(define (/articles req)
  (if (not (authenticated? req))
      (redirect "/sessions/new")
      (let* ([current-page (or (string->number (get-parameter 'page req)) 1)]
             [page-count (ceiling (/ (lookup *conn* (count-articles #:user-id (current-user-id)
                                                                    #:archived #f)) *page-size*))]
             [offset (* (- current-page 1) *page-size*)]
             [articles (sequence->list
                        (in-entities *conn* (select-articles #:user-id (current-user-id)
                                                             #:archived #f
                                                             #:limit *page-size*
                                                             #:offset offset)))])
        (render :page (:articles-list articles current-page page-count)))))

(define (/arcticles/show req id)
  (if (not (authenticated? req))
      (redirect "/sessions/new")
      (let* ([article (lookup *conn* (find-article-by-id #:id id
                                                         #:user-id (current-user-id)))]
             [feed (lookup *conn* (find-feed-by-id #:id (article-feed-id article)
                                                   #:user-id (current-user-id)))])
        (render :page (:article-full feed article)))))

(define (/articles/archive req id)
  (if (not (authenticated? req))
      (redirect "/sessions/new")
      (begin
        (query *conn* (archive-article-by-id id))
        (with-flash #:alert "Article archived."
          (redirect "/articles")))))

(define-values (app-dispatch app-url)
  (dispatch-rules
   [("sessions" "new") (route /sessions/new)]
   [("sessions" "create") #:method "post" (route /sessions/create)]
   [("sessions" "destroy") #:method "delete"  (route /sessions/destroy)]
   [("sessions" "destroy") (route /sessions/destroy)]
   [("users" "new") (route /users/new)]
   [("users" "create") #:method "post" (route /users/create)]
   [("feeds" "new") (route /feeds/new)]
   [("feeds" "create") #:method "post" (route /feeds/create)]
   [("articles") (route /articles)]
   [("articles" (integer-arg)) (route /arcticles/show)]
   [("articles" (integer-arg) "archive") (route /articles/archive)]
   [else (route /articles)]))

(define (start)
  (serve/servlet app-dispatch
                 #:launch-browser? #f
                 #:servlet-path "/"
                 #:port 8000
                 #:servlet-regexp #rx""))

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
          (lookup *conn* (find-feed-by-rss #:user-id user-id
                                           #:rss rss))
          (insert-one! *conn* (make-feed #:user-id user-id
                                         #:rss (rss:feed-rss feed)
                                         #:link (rss:feed-link feed)
                                         #:title (rss:feed-title feed)))))
       (printf "feed id: ~a\n" (feed-id saved-feed))

       (for ([article (rss:feed-articles feed)])
         (define link (rss:article-link article))
         (unless (lookup *conn* (find-article-by-link #:user-id user-id
                                                      #:link link))
           (printf "saving article ~a\n" (rss:article-link article))
           (insert-one! *conn* (make-article #:user-id user-id
                                             #:feed-id (feed-id saved-feed)
                                             #:link (rss:article-link article)
                                             #:title (rss:article-title article)
                                             #:date (rss:article-date article)
                                             #:content (rss:article-content article)))))
       (printf "done processing ~a\n" rss)
       (loop)))))
