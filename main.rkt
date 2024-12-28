#lang racket/base

(require racket/contract
         net/url
         "private/spec.rkt")

(module+ test
  (require rackunit
           rackunit/text-ui))

(module+ test
  (run-tests spec-tests))
