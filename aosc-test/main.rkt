#lang racket/base

(require racket/contract
         net/url
         "spec-test.rkt")

(module+ test
  (require rackunit
           rackunit/text-ui))

(module+ test
  (run-tests spec-tests))
