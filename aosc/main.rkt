#lang racket/base

(require racket/contract
         net/url
         "private/spec.rkt")

;; Probably like this?
;(define sample-package
;  (define ver "1.0.0")
;  (package
;   (section "app-utils")
;   (spec #:ver ver
;         #:srcs (hash 'any (list
;                              (git "https://github.com/sample/sample"
;                                   #:options
;                                   (list (copy-repo? #t)
;                                         (commit (string-append "tags/v" ver)))))
;                      'arm64 (list
;                              (git "https://github.com/sample/sample-arm64"
;                                   #:options
;                                   (list (copy-repo? #t)
;                                         (commit (string-append "tags/v" ver))))))
;         #:chksums (list (skip))
;         #:chkupdate (anitya 114514))
;   (defines #:pkgname "sample"
;            #:pkgdes "A sample package"
;            #:pkgdep (list "glibc")
;            #:builddep (list "meson" "ninja")
;            #:pkgsec "utils")))

(struct package (section spec defines))

;; TODO: deploy package tree from a package variable
(define/contract (deploy-package package)
  (-> package? void?)
  void)

(provide (all-from-out "private/spec.rkt"))
