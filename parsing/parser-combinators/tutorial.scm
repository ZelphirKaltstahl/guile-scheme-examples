(use-modules (parser-combinators)
             (srfi srfi-41))

;; writing your own parse character function

#;(define (parse-c-char stream)
  (if (eqv? (stream-car stream) #\c)
      (parse-result #\c (stream-cdr stream))))

#;(parse-c-char (list->stream '(#\c #\c #\c)))

;; using a library function

#;((parse-char #\c) (list->stream '(#\c #\c #\c)))


;; EXAMPLE 1
;; implementing your own parse-char function
(define (parse-char char)
  "Create a parser that succeeds when the next character in the stream
   is CHAR."
  (lambda (stream)  ; we return a function which accepts
    (stream-match stream  ; using
                  (() %parse-failure)  ; if empty list then fail
                  ((head . tail) (if (equal? head char)
                                        ; if there is head and tail
                                        ; check if the character is
                                        ; equal to the char
                                     (parse-result head tail)
                                        ; the parsing result is the
                                        ; head and the remaining tail
                                     %parse-failure
                                        ; otherwise parsing fails
                                     )))))

(let ([my-char-parser (parse-char #\c)])
  (my-char-parser (list->stream '(#\c #\c #\c))))


;; EXAMPLE 2
;; set of characters
#;((parse-char-set '(#\a #\b #\c)) (list->stream '(#\c #\c #\c)))  ; does not work

;; EXAMPLE 3
;; parse a string part
(define string->stream (compose list->stream string->list))
((parse-string "ccc") (string->stream "ccc means chaos computer club"))

;; EXAMPLE 4
;; any char
(parse-any-char (string->stream "scheme is awesome"))
(parse-any-char (string->stream "\nthis starts with a newline"))

;; ===============
;; CONTROL PARSERS
;; ===============

;; EXAMPLE 5
;; (parse-any parser ...)
;; (parse-each parser ...)
;; (parse-zero-or-more parser)
;; (parse-one-or-more parser)

;; general any char matched
((parse-each parse-any-char parse-any-char parse-any-char) (string->stream "gnu"))

;; only parsing g then n then u
(define parse-gnu
  (parse-each (parse-char #\g)
              (parse-char #\n)
              (parse-char #\u)))

;; try on "gnu"
(parse-gnu (string->stream "gnu"))
;; try on "ccc"
(parse-gnu (string->stream "ccc"))

;; using parse-zero-or-more
(define parse-aaagnu
  (parse-each
   (parse-zero-or-more (parse-char #\a))
   (parse-char #\g)
   (parse-char #\n)
   (parse-char #\u)))

(parse-aaagnu (string->stream "aagnu"))  ; works with arbitrary amount of a in front
(parse-aaagnu (string->stream "gnu"))  ; also with 0 a

(define parse-must-have-aaa-gnu
  (parse-each
   (parse-one-or-more (parse-char #\a))
   (parse-char #\g)
   (parse-char #\n)
   (parse-char #\u)))

(parse-must-have-aaa-gnu (string->stream "aaagnu"))  ; this succeeds
(parse-must-have-aaa-gnu (string->stream "gnu"))  ; this fails

(define parse-a-or-b
  (parse-any (parse-char #\a)
             (parse-char #\b)))

(parse-a-or-b (string->stream "a"))
(parse-a-or-b (string->stream "b"))
(parse-a-or-b (string->stream "c"))

;; ======================
;; OUTPUT BUILDER PARSERS
;; ======================

;; They take a parser as argument.
;; They process the parser's output.
;; Then they return. (what?)

;; EXAMPLE 6
;; (parse-map proc parser)
;; (parse-match parser matcher ...)

(define (parse-char char)
  "Create a parser that succeeds when the next character in the stream
   is CHAR."
  (lambda (stream)  ; we return a function which accepts
    (stream-match stream  ; using
                  (() %parse-failure)  ; if empty list then fail
                  ((head . tail) (if (equal? head char)
                                        ; if there is head and tail
                                        ; check if the character is
                                        ; equal to the char
                                     (parse-result head tail)
                                        ; the parsing result is the
                                        ; head and the remaining tail
                                     %parse-failure
                                        ; otherwise parsing fails
                                     )))))
(define parse-gnu
  (parse-each (parse-char #\g)
              (parse-char #\n)
              (parse-char #\u)))

(define string->stream (compose list->stream string->list))

(define parse-gnu*
  (parse-map
   ;; map takes a procedure, as usual
   (lambda (lst)
     `(b ,(list->string lst)))  ; sxml expression
   ;; and a parser, which will generate a list which is mapped
   ;; (the parse-gnu parser from above, copied so that the example is standalone)
   parse-gnu))

(parse-gnu* (string->stream "gnu is awesome"))

;; ============================
;; EXAMPLE 7: SIMPLE CSV PARSER
;; ============================

;; define a test-check form for making unit tests
(define-syntax test-check
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (begin
       (format #t "* Checking ~s\n" title)
       (let* ((expected expected-result)
              (produced tested-expression))
         (if (not (equal? expected produced))
             (begin (format #t "Expected: ~s\n" expected)
                    (format #t "Computed: ~s\n" produced))))))))

;; defining the unit tests / checks for the csv parser
(when (or (getenv "CHECK")
          (getenv "CHECK_MARKDOWN"))
  (test-check "single line"
              (csv "a;b;c;")
              (list (list "a" "b" "c")))
  (test-check "multi line"
              (csv (string-append "a;b;c;\n"
                                  "d;e;f;"))
              (list (list "a" "b" "c") (list "d" "e" "f"))))

;; interlude
;; (parse-unless predicate parser)
(define (parse-unless predicate parser)
  (lambda (stream)
    (match (predicate stream)
      ((? parse-failure?) (parser stream))
      (_ %parse-failure))))

;; defining the csv parser
