#lang info
(define collection 'multi)
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/aosc.scrbl" ())))
(define pkg-desc "AOSC Racket Packaging Tools")
(define version "0.0")
(define pkg-authors '(origincode))
(define license '(Apache-2.0 OR MIT))
