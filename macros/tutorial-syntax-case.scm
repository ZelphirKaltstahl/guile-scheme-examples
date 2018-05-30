;; =========
;; EXAMPLE 1
;; =========
;; using syntax-case - a first example
(define-syntax when
  (syntax-rules ()
    ((_ test e e* ...)
     (if test (begin e e* ...)))))

(define-syntax when
  (lambda (x)                         ; wrap it in a lambda when using
                                        ; syntax-case
                                      ; "[...] the lambda wrapper is simply a
                                        ; leaky implementation detail."
    (syntax-case x ()                 ; The first argument is not the
                                        ; ellipsis identifier anymore.
                                        ; In parens are probably still the literals.
                                      ; "[...] syntax transformers are just
                                        ; functions that transform syntax to syntax.
      ((_ test e e* ...)              ; One can apparently still use _ instead of
                                        ; a name.
       #'(if test (begin e e* ...))   ; #' is equivalent to (syntax <?>)
       ))))

;; is equivalent to:
(define-syntax when
  (lambda (x)
    (syntax-case x ()
      ((_ test e e* ...)
       (syntax (if test
                   (begin e e* ...)))))))


;; =========
;; EXAMPLE 2
;; =========
;; How are syntax-case macros more powerful than syntax-rules macros?
(define-syntax add1
  (lambda (x)
    (syntax-case x ()
      ((_ exp)
       (syntax (+ exp 1))))))

;; relying on previous add1 definition
(define-syntax add1!
  (lambda (x)
    (syntax-case x ()
      [(_ var) (identifier? (syntax var))
       ;; (1) apparently one can define conditions here --> guard clause!
       ;; (2) `identifier?` asks if a syntax object is an identifer.
       ;; To the expander `var` is more than only a symbol.
       ;; This way `identifer?` can know about this.
       (syntax (set! var (add1 var)))])))

(define foo 0)
(add1! foo)  ; foo ⇒ 1
(add1! "not-an-identifier")  ; ⇒ error

;; =========
;; EXAMPLE 3
;; =========
;; For example, in
;; (aif (foo) (bar it))
;; `it`
;; would be bound to the result of
;; (foo).

;; (aif (foo) (bar it))
;;      --v--      ^
;;        |        |
;;        `-->-->--´
;;     result goes here
;;
;; The following doesn't work:
(define-syntax aif
  (lambda (x)
    (syntax-case x ()
      [(_ test then else)
       (syntax
        (let ((it test))
          (if it then else)))])))
