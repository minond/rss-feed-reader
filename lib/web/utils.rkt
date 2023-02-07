#lang racket/base

(require racket/function
         racket/string
         json
         threading
         web-server/servlet
         (prefix-in : scribble/html/xml)
         "../pair.rkt"
         "session.rkt"
         "flash.rkt")

(provide default-layout
         get-parameter
         get-binding
         authenticated-route
         route
         render
         redirect
         redirect-back)

(define default-layout (make-parameter identity))

(define (get-parameter key req #:default [default ""])
  (get-binding key req #:default default))

(define (get-binding key req #:default [default null])
  (cdr (or (assoc key (request-bindings req))
           (cons key default))))

(define ((authenticated-route handler) req . args)
  (let ([session (lookup-session req)])
    (if (not (authenticated? session))
        (redirect-to (new-session-route) permanently)
        (apply-handler handler session req args))))

(define ((route handler) req . args)
  (apply-handler handler (lookup-session req) req args))

(define apply-handler
  (case-lambda
    [(handler req args)
     (apply-handler handler (lookup-session req) req args)]
    [(handler session req args)
     (parameterize ([current-request req]
                    [current-session session]
                    [current-user-id (and (session? session)
                                          (session-user-id session))]
                    [current-flash (and (session? session)
                                        (session-flash session))])
       (apply handler (cons req args)))]))

(define (render content #:layout [layout (default-layout)])
  (let* ([request (current-request)]
         [session (current-session)]
         [user-id (current-user-id)]
         [flash (current-flash)]
         [json? (wants-json? request)]
         [content-type (if json?
                           #"application/json; charset=utf-8"
                           #"text/html; charset=utf-8")])
    (response/output
     #:headers (list (header #"Content-Type" content-type))
     (lambda (op)
       (parameterize ([current-request request]
                      [current-session session]
                      [current-user-id user-id]
                      [current-flash flash])
         (display (if json?
                      (jsexpr->string (hash 'html (:xml->string content)))
                      (layout content)) op))))))

(define (wants-json? req)
  (let ([accept (assq 'accept (request-headers req))])
    (and accept
         (string-contains?
          (cdr accept)
          "json"))))

(define (redirect url)
  (~> (current-session)
      (update-session-cookie _ #:flash (current-flash))
      (cookie->header _)
      (list _)
      (redirect-to url permanently #:headers _)))

(define (redirect-back)
  (let ([referer (assq 'referer (request-headers (current-request)))])
    (if referer
        (redirect (cdr referer))
        (redirect "/"))))
