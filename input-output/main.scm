(use-modules (ice-9 textual-ports)  ; for textual reading and writing procedures
             (ice-9 binary-ports)  ; not sure if needed
             (ice-9 rdelim)  ; for `eof-object?`
             (ice-9 optargs)  ; for keyword arguments
             (srfi srfi-1))  ; not sure if needed


(define* (get-string-from-file file-path #:key (encoding "UTF-8"))
  (call-with-input-file file-path
    (λ (port)
      (get-string-all port))))


(define* (get-lines-from-file file-path #:key (encoding "UTF-8"))
  ;; another common encoding is: "ISO-8859-1"
  ;; see http://www.iana.org/assignments/character-sets for more
  (define (get-lines-from-port port)
    (let ([line (get-line port)])
      (cond [(eof-object? line) '()]
            [else
             (cons line
                   (get-lines-from-port port))])))
  (call-with-input-file file-path
    (lambda (port)
      (set-port-encoding! port encoding)
      (get-lines-from-port port))))


(define* (put-lines-to-file file-path lines
                            #:key
                            (encoding "UTF-8")
                            (mode 'replace))
  (let* ([lines-joined (string-join lines "\n")]
         [new-content
          (cond [(eq? mode 'append)
                 (string-append (get-string-from-file
                                 file-path
                                 #:encoding encoding)
                                "\n"
                                lines-joined)]
                [(equal? mode 'replace) lines-joined]
                [else lines-joined])])
    (call-with-output-file file-path
      (lambda (port)
        (set-port-encoding! port encoding)
        (put-string port new-content)))))
