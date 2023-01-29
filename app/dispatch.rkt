#lang racket/base

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
   [("feeds") (authenticated-route /feeds)]
   [("feeds" "new") (authenticated-route /feeds/new)]
   [("feeds" "create") #:method "post" (authenticated-route /feeds/create)]
   [("feeds" (integer-arg) "unsubscribe") #:method "put" (authenticated-route /feeds/<id>/unsubscribe)]
   [("feeds" (integer-arg) "unsubscribe") (authenticated-route /feeds/<id>/unsubscribe)]
   [("feeds" (integer-arg) "subscribe") #:method "put" (authenticated-route /feeds/<id>/subscribe)]
   [("feeds" (integer-arg) "subscribe") (authenticated-route /feeds/<id>/subscribe)]
   [("feeds" (integer-arg) "articles") (authenticated-route /feeds/<id>/articles)]
   [("articles") (authenticated-route /articles)]
   [("articles" (integer-arg)) (authenticated-route /arcticles/<id>/show)]
   [("articles" (integer-arg) "archive") #:method "put" (authenticated-route /articles/<id>/archive)]
   [("articles" (integer-arg) "archive") (authenticated-route /articles/<id>/archive)]
   [("articles" (integer-arg) "unarchive") #:method "put" (authenticated-route /articles/<id>/unarchive)]
   [("articles" (integer-arg) "unarchive") (authenticated-route /articles/<id>/unarchive)]
   [else (authenticated-route /articles)]))
