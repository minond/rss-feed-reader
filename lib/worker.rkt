#lang racket/base

(require racket/function)

(provide make-worker)

(define (make-worker #:name name
                     #:cust [cust (current-custodian)]
                     #:channel [ch #f]
                     #:interval [interval #f]
                     #:handler handler)
  (define thd
    (parameterize ([current-custodian cust])
      (thread
       (lambda ()
         (printf "[INFO] starting ~a\n" name)
         (let loop ()
           (apply sync
                  (filter identity
                          (list
                           (handle-evt (thread-receive-evt)
                                       (lambda (-)
                                         (printf "[INFO] stopping ~a\n" name)))

                           (cond
                             [ch
                              (handle-evt ch
                                          (lambda (arg)
                                            (handler arg)
                                            (loop)))]
                             [interval
                              (handle-evt (alarm-evt (+ (current-inexact-milliseconds) interval))
                                          (lambda (args)
                                            (handler args)
                                            (loop)))]
                             [else #f])))))))))

  (lambda ()
    (thread-send thd 'stop)))
