#lang racket/base

(require racket/contract
         net/url
         "private/spec.rkt")

;; Probably like this?
;(define sample-package
;  (define version "1.0.0")
;  (package
;   (section "app-utils")
;   (spec (ver version)
;         #:srcs (srcs (hash 'any (list
;                                    (git "https://github.com/sample/sample"
;                                         #:options
;                                         (list (copy-repo? #t)
;                                               (commit (string-append "tags/v" ver)))))
;                            'arm64 (list
;                                    (git "https://github.com/sample/sample-arm64"
;                                         #:options
;                                         (list (copy-repo? #t)
;                                               (commit (string-append "tags/v" ver))))))
;         #:chksums (chksums (list (skip)))
;         #:chkupdate (chkupdate (anitya 114514)))
;   (defines (pkgname "sample")
;            (pkgdes "A sample package")
;            (pkgdep (list "glibc"))
;            (builddep (list "meson" "ninja"))
;            (pkgsec "utils")))
;            ))

(struct package (section spec defines))

;; TODO: deploy package tree from a package variable
(define/contract (deploy-package package)
  (-> package? void?)
  void)

(provide (except-out (all-from-out "private/spec.rkt")
                     spec-type)
         (contract-out [struct package ((section string?)
                                        (spec spec-type?)
                                        (defines void?))]))
