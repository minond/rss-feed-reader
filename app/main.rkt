#lang racket

(require db
         deta
         web-server/servlet-env
         "parameters.rkt"
         "dispatch.rkt")

(provide start)

(define pool
  (connection-pool
   (lambda ()
     (sqlite3-connect #:database "feeder.db" #:mode 'create))))

(current-database-connection (connection-pool-lease pool))

(create-table! (current-database-connection) 'user)
(create-table! (current-database-connection) 'feed)
(create-table! (current-database-connection) 'article)

(define (start)
  (serve/servlet app-dispatch
                 #:launch-browser? #f
                 #:servlet-path "/"
                 #:port 8000
                 #:servlet-regexp #rx""))
