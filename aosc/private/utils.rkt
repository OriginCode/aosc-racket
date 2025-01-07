#lang racket/base

(require racket/contract
         racket/string
         "arch.rkt")

(provide (all-defined-out))

(define/contract (boolean->symbol b)
  (-> boolean? symbol?)
  (if b 'true 'false))

(define/contract (boolean->exact-nonnegative-integer b)
  (-> boolean? exact-nonnegative-integer?)
  (if b 1 0))

(define/contract (spec-entry->string entry-name value [quote? #f] [arch 'any])
  (->* (string? string?) (boolean? arch?) string?)
  (string-append entry-name
                 (if (equal? arch 'any)
                     ""
                     (string-append "__" (string-upcase (symbol->string arch))))
                 (if quote? "=\"" "=")
                 value
                 (if quote? "\"" "")))
