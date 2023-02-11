#lang racket/base

(require racket/match
         racket/async-channel
         "../commands.rkt"
         "../websocket.rkt")

(provide schedule-user-feed-sync
         make-user-feed-sync-worker)

(define cmd-ch (make-async-channel))

(define (schedule-user-feed-sync cmd session-key)
  (async-channel-put cmd-ch (list cmd session-key)))

(define (make-user-feed-sync-worker [cust (make-custodian)])
  (define thd
    (parameterize ([current-custodian cust])
      (thread
       (lambda ()
         (printf "[INFO] starting user-feed-sync-worker\n")
         (let loop ()
           (sync
            (handle-evt (thread-receive-evt)
                        (lambda (_)
                          (printf "[INFO] stopping user-feed-sync-worker\n")))
            (handle-evt cmd-ch
                        (lambda (args)
                          (match-define (list cmd session-key) args)
                          (with-handlers ([exn:fail? (lambda (e)
                                                       (printf "[ERROR] ~a\n" e))])
                            (run cmd)
                            (ws-send/feed-update session-key))
                          (loop)))))))))

  (lambda ()
    (thread-send thd 'stop)))
