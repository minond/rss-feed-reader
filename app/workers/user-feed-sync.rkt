#lang racket/base

(require racket/match
         "../commands.rkt"
         "../websocket.rkt")

(provide schedule)

(define (schedule cmd session-key)
  (thread-send user-feed-sync-thread (list cmd session-key)))

(define user-feed-sync-thread
  (thread
   (lambda ()
     (let loop ()
       (match-define (list cmd session-key) (thread-receive))

       (with-handlers ([exn:fail? (lambda (e)
                                    (printf "[ERROR] ~a\n" e))])
         (run cmd)
         (ws-send/feed-update session-key))
       (loop)))))
