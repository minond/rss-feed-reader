#lang racket/base

(require racket/match
         racket/async-channel
         "../commands.rkt"
         "../websocket.rkt"
         "../../lib/worker.rkt")

(provide schedule-user-feed-sync
         make-user-feed-sync-worker
         make-user-background-sync-worker)

(define user-sync-time-ms 60000)
(define cmd-ch (make-async-channel))

(define (schedule-user-feed-sync cmd session-key)
  (async-channel-put cmd-ch (list cmd session-key)))

(define (make-user-feed-sync-worker)
  (make-worker #:name 'user-feed-sync-worker
               #:channel cmd-ch
               #:handler (lambda (args)
                           (match-define (list cmd session-key) args)
                           (with-handlers ([exn:fail? (lambda (e) (printf "[ERROR] ~a\n" e))])
                             (run cmd)
                             (ws-send/feed-update session-key)))))

(define (make-user-background-sync-worker)
  (make-worker #:name 'user-background-sync-worker
               #:interval user-sync-time-ms
               #:handler (lambda (_) (run (update-feeds)))))
