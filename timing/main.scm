(use-modules (srfi srfi-19))

(define-module (myutil time)
  #:export (time))

(define-syntax time
  (syntax-rules ()
    [(time expr expr* ...)
     (begin
       (define start-time (current-time time-monotonic))
       expr
       expr* ...
       (define end-time (current-time time-monotonic))
       (let* ([diff (time-difference end-time start-time)]
              [elapsed-ns (+ (/ (time-nanosecond diff) 1e9)
                             (time-second diff))])
         (display (format #t "~fs~%" elapsed-ns))))]))
