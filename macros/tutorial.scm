;; FROM: https://www.gnu.org/software/guile/manual/html_node/Defining-Macros.html#Defining-Macros

;; global syntax definition
(define-syntax when
  (syntax-rules ()
    ((when condition exp ...)
     (if condition
         (begin exp ...)))))

;; local syntax definition
(let-syntax ([unless (syntax-rules ()
                       ((unless condition exp ...)
                        (if (not condition)
                            (begin exp ...))))])
  (unless #t
    (primitive-exit 1))
  "rock rock rock")

(letrec-syntax ([my-or
                 (syntax-rules ()
                   ((my-or)
                    #t)
                   ((my-or exp)
                    exp)
                   ((my-or exp rest ...)
                    (let ((t exp))
                      (if t
                          t
                          (my-or rest ...)))))])
  (my-or #f "rockaway beach"))


;; EXAMPLE 2
;; kwote as alias for quote
(define-syntax kwote
  (syntax-rules ()
    ((kwote exp)
     (quote exp))))
(kwote (foo . bar))

;; EXAMPLE 3
;; let macro

;; matching improper lists
(define-syntax let1
  (syntax-rules ()
    ((_ (var val) . exps)               ; apparently one can use the _ to match
                                        ; the name of the macro (?)
                                        ; exps is second part of a pair
                                        ; it is a pattern
                                        ; In fact, it does not matter what we write
                                        ; as first part in the match case for
                                        ; syntax-rules.
     (let ((var val)) . exps))))

;; problem: it would match (let1 (foo 'bar) . baz)
;; Why is that a problem?

;; improved version
(define-syntax let1
  (syntax-rules ()
    ((_ (var val) exp ...)  ; exp ... will match the so called body
     (let ((var val)) exp ...))))

;; problem: body can be empty

;; further improved version
(define-syntax let1
  (syntax-rules ()
    ((_ (var val) exp exp* ...)         ; A common idiom is to name the
                                        ; ellipsized pattern variable with
                                        ; an asterisk.
     (let ((var val)) exp exp* ...))))

;; EXAMPLE 4
;; matching vectors
(define-syntax letv  ; let vector
  (syntax-rules ()
    ((_ #((var val) ...) exp exp* ...)
     (let ((var val) ...) exp exp* ...))))
(letv #((foo 'bar)) foo)

;; EXAMPLE 5
;; What goes into the parentheses of syntax-rules?: syntax-rules (???)
;; Literals!
;; Parts which are matched against literally.
;; Not interpreted as "standing for a pattern".
;; That is how we can distinguish between literally meant stuff and
;; stuff that stands for patterns.

(define-syntax cond1
  (syntax-rules (=> else) ; The following things are to be taken literally:
                          ; => else
    [(cond1 test => fun)
     (let ((exp test))
       (if exp
           (fun exp)  ; apply fun to the tested argument
           #f))]
    [(cond1 test exp exp* ...)  ; if we have multiple expressions,
                                ; we can use begin to run them all
     (if test
         (begin exp
                exp*
                ...))]
    [(cond1 else exp exp* ...)  ; if we have an else we do not need to test more
     (begin exp
            exp*
            ...)]))

(define (square x)
  (* x x))

(cond1 10 => square)  ; 100

(let ((=> #t))  ; => is used as variable containing #t
  (cond1 10 => square))  ; will go into the multiple expressions case

;; EXAMPLE 6
;; Defining a macro, which defines macros.

(define-syntax define-matcher-macro
  (syntax-rules ()
    ((_ name lit)                       ; expecting form:
                                        ; (define-matcher-macro
                                        ;  <something-containing-a-name>
                                        ;  <something-containing-a-literal>)
     (define-syntax name  ; if it is a match, define the syntax (<some-name> ...
       (syntax-rules ()
         ((_ lit) #t)  ; true if it is applied to the literal in lit
         ((_ else) #f)  ; false otherwise
         )))))

(define-matcher-macro is-literal-foo? "foo")

(is-literal-foo? "foo")
⇒ #t
(is-literal-foo? "bar")
⇒ #f
(let ((foo "foo"))
  (is-literal-foo? foo))
⇒ #f


;; EXAMPLE 7
;; TODO: Explain how this even works!
;; Reporting errors at macro-expansion time (read time, compile time).
(define-syntax simple-let
  (syntax-rules ()
    [(simple-let (head ... ((x . y) val) . tail)
                                        ; (1) head ... can also be zero times?
                                        ; (2) what is `. tail` matching?
                                        ; (3) can I not use two ellipsis on the
                                        ; same level instead of `. tail`?
                 body1
                 body2 ...)
     (syntax-error "expected an identifier but got"
                   (x . y))]
    ;; if there ((a . b) val) is not matched
    [(simple-let ((name val) ...)
                 body1
                 body2 ...)
     ((lambda (name ...)
        body1
        body2 ...)
      val ...)]))

;; simply working
(simple-let ([a 3])
            (+ a 4))

;; caught
(simple-let ([(a . b) 3])  ; Q: What is `. tail` matching in this one?
            (+ a 4))
(simple-let ([a 3] [(b . c) 3])  ; Q: What is `. tail` matching in this one?
            (+ a b))

;; not caught
(simple-let ([a 3] [(b . c) 3] [d 4])  ; Q: Why is `. tail` not matching `[d 4]`?
            (+ a b))



;; EXAMPLE 8
;; Specifying a Custom Ellipsis Identifier.
(define-syntax define-quotation-macros
  (syntax-rules ()
    ((_ (macro-name head-symbol) ...)  ; arbitrary number of macro-name head-symbol pairs
                                        ; which will be defined.
                                        ; macro-name is not a literal.
     (begin (define-syntax macro-name
              (syntax-rules
                  ::: ()  ; the ellipsis identifier can be given as a first
                                        ; argument to syntax-rules before the parentheses.
                  ((_ x :::)
                   (quote (head-symbol x :::)))))
            ...))))

(define-quotation-macros
  (quote-a a)
  (quote-b b)
  (quote-c c))  ; define multiple macros, one for each given pair
; (quote-a 1 2 3) ⇒ (a 1 2 3)
