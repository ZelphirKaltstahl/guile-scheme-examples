(define-module (myutil stream)
  #:export (make-stream
            stream-car
            stream-cdr
            stream-next
            stream-get-nth
            natural-numbers))

(define (make-stream proc start)
  (cons start
        (λ ()
          (make-stream proc (proc start)))))

(define stream-car car)

(define (stream-cdr stream)
  ((cdr stream)))

(define stream-next stream-cdr)  ; convenience procedure

(define (stream-get-nth stream n)
  (cond [(= n 0)
         (stream-car stream)]
        [else
         (stream-get-nth (stream-cdr stream)
                         (- n 1))]))

(define natural-numbers
  (make-stream (λ (x) (+ x 1))
               0))


(define (numerator fraction)
  (car fraction))
(define (denominator fraction)
  (cdr fraction))
(define (make-fraction num denom)
  (cons num denom))
(define (next-rational-fraction fraction)
  (let ([num (numerator fraction)]
        [denom (denominator fraction)])
    (cond [(= num 1)
           (if (odd? denom)
               (make-fraction num
                              (1+ denom))
               (make-fraction (1+ num)
                              (1- denom)))]
          [(= denom 1)
           (if (odd? num)
               (make-fraction (1- num)
                              (1+ denom))
               (make-fraction (1+ num)
                              denom))]
          [else (if (odd? (+ num denom))
                    (make-fraction (1+ num)
                                   (1- denom))
                    (make-fraction (1- num)
                                   (1+ denom)))])))
(define rational-numbers
  (make-stream next-rational-fraction
               (make-fraction 1 1)))

(define (position-in-stream elem a-stream)
  (define (iter stream n)
    (cond [(equal? (stream-car stream) elem) n]
          [else (iter (stream-next stream) (1+ n))]))
  (iter a-stream 0))
