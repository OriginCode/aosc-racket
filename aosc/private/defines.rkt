#lang racket/base

(require racket/contract
         "macro.rkt")

; TODO: limit abtype and abhost to a set of valid values with contract
(define-vars #f string? pkgname pkgsec abhost abtype)
(define-vars #t string? pkgdes)
(define-vars #f (listof string?)
             pkgdep builddep pkgrecom pkgbreak pkgsug pkgconfl pkgrep)
(define-vars #f (listof string?)
             autotools-def cmake-def waf-def qtproj-def)
(define-vars #t (listof string?)
             autotools-after cmake-after waf-after qtproj-after go-build-after
             cargo-after)
(define-vars #f boolean?
             ab-flags-o3 ab-flags-specs ab-flags-ssp ab-flags-ftf ab-flags-rro
             nolto useclang ab-flags-pie ab-flags-pic abshadow abconfighack
             abclean noparallel abstrip absplitdbg)
(define-vars #f boolean? reconf nopython2 nopython3)
(define-vars #f exact-nonnegative-integer?
             abthreads)

(struct defines-type (opts) #:transparent)

(define/contract (defines opts)
  (-> (listof pair?) defines-type?)
  ;; TODO: checks here
  (defines-type opts))

(define/contract (defines->string defines)
  (-> defines-type? string?)
  ;; TODO: ->string here, apply ->string to each opt
  "")
