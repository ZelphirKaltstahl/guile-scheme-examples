(define-module (myutil stream)
  #:export (make-stream
            stream-car
            stream-cdr
            stream-get-nth))

(define (make-stream proc start)
  (cons start
        (λ ()
          (make-stream proc (proc start)))))

(define stream-car car)

(define (stream-cdr stream)
  ((cdr stream)))

(define (stream-get-nth stream n)
  (cond [(= n 0)
         (stream-car stream)]
        [else
         (stream-get-nth (stream-cdr stream)
                         (- n 1))]))

(make-stream (lambda (x) (+ x 1)) 0)

(stream-get-nth (make-stream (lambda (x) (+ x 1)) 0)
                100000)
(stream-get-nth (make-stream (lambda (x) (+ x 1)) 0)
                1000000)
(stream-get-nth (make-stream (lambda (x) (+ x 1)) 0)
                10000000)
