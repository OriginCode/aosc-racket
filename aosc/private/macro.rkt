#lang racket/base

(require racket/contract
         racket/format
         racket/string
         (for-syntax racket/base
                     racket/syntax))

(define-syntax (define-vars stx)
  (syntax-case stx ()
    [(_ array contract a)
     (with-syntax
       ([a-val (format-id #'a "~a-val" #'a)]
        [a? (format-id #'a "~a?" #'a)]
        [a->string (format-id #'a "~a->string" #'a)] )
       #`(begin
           (struct a (val) #:transparent)
           (define/contract (a->string val)
             (-> a? string?)
             (format "~a=~a"
                     'a
                     (if (equal? 'contract string?)
                         (a-val val)
                         (if 'array 
                             (format "(~a)" (string-join (map ~v (a-val val))))
                             (format "\"~a\"" (string-join (a-val val)))))))
           ))]
    [(_ array contract a b ...)
     #'(begin
         (define-vars contract a)
         (define-vars contract b ...))]
    ))
