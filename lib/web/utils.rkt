#lang racket

(require threading
         web-server/servlet
         "../pair.rkt"
         "session.rkt"
         "flash.rkt")

(provide get-parameter
         get-binding
         route
         render
         redirect)

(define (get-parameter key req #:default [default ""])
  (get-binding key req #:default default))

(define (get-binding key req #:default [default null])
  (cdr (find-pair key (request-bindings req)
                  #:default (cons key default))))

(define ((route handler) req . args)
  (let ([session (lookup-session req)])
    (parameterize ([current-request req]
                   [current-session session]
                   [current-user-id (and (session? session)
                                         (session-user-id session))]
                   [current-flash (and (session? session)
                                       (session-flash session))])
      (apply handler (cons req args)))))

(define (render :page content)
  (let ([request (current-request)]
        [session (current-session)]
        [user-id (current-user-id)]
        [flash (current-flash)])
    (response/output
     (lambda (op)
       (parameterize ([current-request request]
                      [current-session session]
                      [current-user-id user-id]
                      [current-flash flash])
         (display (:page content) op))))))

(define (redirect url)
  (~> (current-session)
      (update-session-cookie _ #:flash (current-flash))
      (cookie->header _)
      (list _)
      (redirect-to url permanently #:headers _)))