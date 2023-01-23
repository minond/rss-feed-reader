#lang racket

(require db
         deta
         deta/reflect
         "parameters.rkt"
         "models/article.rkt"
         "models/feed.rkt"
         "models/user.rkt")

(provide (all-from-out "models/article.rkt")
         (all-from-out "models/feed.rkt")
         (all-from-out "models/user.rkt"))

; For interactive mode
(schema-registry-allow-conflicts? #t)

(define pool
  (connection-pool
   (lambda ()
     (sqlite3-connect #:database "feeder.db" #:mode 'create))))

(current-database-connection (connection-pool-lease pool))

(create-table! (current-database-connection) 'user)
(create-table! (current-database-connection) 'feed)
(create-table! (current-database-connection) 'article)
