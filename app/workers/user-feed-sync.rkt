#lang racket/base

(require racket/match
         "../commands.rkt"
         "../websocket.rkt")

(provide schedule-user-feed-sync
         make-user-feed-sync-worker)

(define (schedule-user-feed-sync cmd session-key)
  (thread-send user-feed-sync-thread (list cmd session-key)))

(define user-feed-sync-thread #f)
(define (make-user-feed-sync-worker [cust (make-custodian)])
  (define thd
    (parameterize ([current-custodian cust])
      (thread
       (lambda ()
         (printf "[INFO] starting make-user-feed-sync-worker\n")
         (let loop ()
           (match (thread-receive)
             ['stop
              (printf "[INFO] stopping make-user-feed-sync-worker\n")
              (void)]
             [(list cmd session-key)
              (with-handlers ([exn:fail? (lambda (e)
                                           (printf "[ERROR] ~a\n" e))])
                (run cmd)
                (ws-send/feed-update session-key))
              (loop)]))))))

  (set! user-feed-sync-thread thd)
  (lambda ()
    (thread-send thd 'stop)))
