#lang racket/base

(require racket/contract
         (for-syntax racket/base
                     racket/syntax))

(define-syntax (define-vars stx)
  (syntax-case stx ()
    [(_ contract a)
     (with-syntax
         ([a-type (format-id #'a "~a-type" #'a)]
          [a-vals (format-id #'a "~a-type-vals" #'a)]
          [a? (format-id #'a "~a-type?" #'a)])
       #`(begin
           (struct a-type (vals) #:transparent)
           (define/contract (a val)
             (-> contract a?)
             (a-type val))
           ))]
    [(_ contract a b ...)
     #'(begin
         (define-vars contract a)
         (define-vars contract b ...))]
    ))
