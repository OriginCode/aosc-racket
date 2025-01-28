#lang racket/base

(require racket/contract
         racket/format
         racket/string
         "arch.rkt"
         "utils.rkt"
         (for-syntax racket/base
                     racket/syntax))

(provide (all-defined-out))

(define-syntax (define-vars stx)
  (syntax-case stx ()
    [(_ array/quote contract a)
     (with-syntax*
       ([a (if (equal? (syntax->datum #'contract) 'boolean?)
               (format-id #'a "~a?" #'a)
               #'a)]
        [a-val (format-id #'a "~a-val" #'a)]
        [a? (format-id #'a "~a?" #'a)]
        [a->string (format-id #'a "~a->string" #'a)] )
       #`(begin
           (struct a (val) #:transparent)
           (define/contract (a->string val [arch 'any])
             (->* (a?) (arch?) string?)
             (format "~a=~a"
                     (string-upcase
                      (format "~a~a"
                              (string-replace
                               (if (equal? 'contract 'boolean?)
                                  (string-trim (symbol->string 'a) "?" #:left? #f)
                                  (symbol->string 'a))
                               "-" "_")
                              (if (equal? arch 'any)
                                  ""   
                                  (string-append "__" (symbol->string arch)))))
                     (cond
                       [(equal? 'contract 'string?)
                        (if 'array/quote
                            (~v (a-val val))
                            (a-val val))]
                       [(equal? 'contract 'boolean?)
                        (boolean->exact-nonnegative-integer (a-val val))]
                       [(equal? 'contract 'exact-nonnegative-integer?)
                        (a-val val)]
                       [(equal? 'contract '(listof string?))
                        (if 'array/quote
                           (format "(~a)" (string-join (map ~v (a-val val))))
                           (format "\"~a\"" (string-join (a-val val))))])))
           ))]
    [(_ array/quote contract a b ...)
     #'(begin
         (define-vars array/quote contract a)
         (define-vars array/quote contract b ...))]
    ))
