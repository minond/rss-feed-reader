#lang racket

(require threading
         deta)

(provide (struct-out user)
         make-user
         find-user-by-email)

(define-schema user
  ([id id/f #:primary-key #:auto-increment]
   [email string/f #:unique #:contract non-empty-string?]
   [encrypted-password binary/f]
   [salt binary/f]))

(define (find-user-by-email email)
  (~> (from user #:as u)
      (where (= email ,email))
      (limit 1)))