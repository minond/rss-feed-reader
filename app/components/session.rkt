#lang racket

(require (prefix-in : scribble/html/xml)
         (prefix-in : scribble/html/html)
         (prefix-in : scribble/html/extra))

(provide :login-form)

(define (:login-form [email null])
  (:form 'action: "/sessions/create"
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
         (:input 'type: "submit"
                 'value: "Login")
         (:a 'href: "/users/new"
             "or register instead")))
