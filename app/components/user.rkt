#lang racket/base

(require (prefix-in : scribble/html/xml)
         (prefix-in : scribble/html/html)
         (prefix-in : scribble/html/extra))

(provide :user-form)

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
