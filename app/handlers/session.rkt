#lang racket/base

(require deta
         web-server/servlet
         "../../lib/web.rkt"
         "../../lib/crypto.rkt"
         "../parameters.rkt"
         "../components.rkt"
         "../models.rkt")

(provide /sessions/new
         /sessions/create
         /sessions/destroy)

(define (/sessions/new req)
  (let* ([email (get-parameter 'email req)])
    (render :page (:login-form email))))

(define (/sessions/create req)
  (let* ([email (get-parameter 'email req)]
         [password (get-parameter 'password req)]
         [user (lookup (current-database-connection) (find-user-by-email email))])
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
