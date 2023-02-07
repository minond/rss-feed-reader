#lang racket/base

(require deta
         web-server/servlet
         "../../lib/web.rkt"
         "../../lib/crypto.rkt"
         "../parameters.rkt"
         "../components.rkt"
         "../models.rkt")

(provide /users/new
         /users/create)

(define (/users/new req)
  (let* ([email (get-parameter 'email req)])
    (render (:user-form email))))

(define (/users/create req)
  (let* ([email (get-parameter 'email req)]
         [password (get-parameter 'password req)]
         [password-confirm (get-parameter 'password-confirm req)])
    (if (not (equal? password password-confirm))
        (with-flash #:notice "Your password and confirmation did not match, please try again."
          (redirect (format "/users/new?email=~a" email)))
        (let-values ([(encrypted-password salt) (make-password password)])
          (define user
            (insert-one! (current-database-connection)
                         (make-user #:email email
                                    #:salt salt
                                    #:encrypted-password encrypted-password)))
          (define session-cookie
            (create-session-cookie #:user-id (user-id user)))
          (redirect-to "/feeds/new" permanently
                       #:headers (list
                                  (cookie->header session-cookie)))))))
