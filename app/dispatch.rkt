#lang racket

(require web-server/servlet
         web-server/servlet-env
         "../lib/web.rkt"
         "handlers.rkt")

(provide app-dispatch app-url)

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
