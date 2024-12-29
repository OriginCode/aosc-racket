#lang racket/base

(require racket/contract
         racket/string
         net/url)

(provide (all-defined-out))

(define/contract (boolean->symbol b)
  (-> boolean? symbol?)
  (if b 'true 'false))

;; SRCS options
;;==============
(struct src-option (field value) #:transparent)

(define/contract (branch name)
  (-> string? src-option?)
  (src-option 'branch name))

(define/contract (commit hash)
  (-> string? src-option?)
  (src-option 'commit hash))

(define/contract (rename name)
  (-> string? src-option?)
  (src-option 'rename name))

(define/contract (submodule method)
  (-> (or/c boolean? symbol?) src-option?)
  (src-option
   'submodule
   (cond
     [(boolean? method) (boolean->symbol method)]
     [(symbol? method)
      (if (not (equal? method 'recursive))
          (raise-argument-error 'submodule "boolean? or 'recursive" method)
          method)])))

(define/contract (copy-repo? val)
  (-> boolean? src-option?)
  (src-option 'copy-repo (boolean->symbol val)))

;; SRCS= field
;;=============
(struct src (type options value) #:transparent)

(define/contract (git url #:options [options null])
  (->* (string?) (#:options (listof src-option?)) src?)
  (src 'git options url))

(define/contract (tbl url #:options [options null])
  (->* (string?) (#:options (listof src-option?)) src?)
  (define allowed-options '(rename))
  (for-each (λ (opt)
              (when (not (member (src-option-field opt) allowed-options))
                (raise-user-error 'tbl
                                  "~a option is not allowed in tbl source"
                                  (src-option-field opt))))
            options)
  (src 'tbl options url))

(define/contract (src->string src)
  (-> src? string?)
  (string-join `(,(symbol->string (src-type src))
                 ,@(if (null? (src-options src))
                       null
                       (list (string-join (for/list ([opt (src-options src)])
                                            (format "~a=~a"
                                                    (src-option-field opt)
                                                    (src-option-value opt)))
                                          ";")))
                 ,(src-value src))
               "::"))

;; TODO: __$ARCH support
(define/contract (srcs->string srcs)
  (-> (listof src?) string?)
  (string-append "SRCS=\"" (string-join (map src->string srcs)) "\""))

;; CHKSUMS= field
;;================
(struct chksum (type value) #:transparent)

(define/contract (checksum algorithm value)
  (-> symbol? string? chksum?)
  (chksum algorithm value))

(define/contract (skip)
  (-> chksum?)
  (chksum 'skip #f))

(define/contract (chksum->string chksum)
  (-> chksum? string?)
  (if (equal? (chksum-type chksum) 'skip)
      "SKIP"
      (format "~a::~a" (chksum-type chksum) (chksum-value chksum))))

;; TODO: __$ARCH support
(define/contract (chksums->string chksums)
  (-> (listof chksum?) string?)
  (string-append "CHKSUMS=\"" (string-join (map chksum->string chksums)) "\""))

;; TODO: CHKUPDATE= field
;;==================
(struct chkupdate (type value) #:transparent)

(define/contract (anitya id)
  (-> exact-nonnegative-integer? chkupdate?)
  (chkupdate 'anitya (format "id=~a" id)))

;; Whole `spec` file struct
(struct spec-type (ver rel srcs chksums chkupdate dummysrc?) #:transparent)

;; `spec` constructor
;; TODO: __$ARCH support, probably (or/c (hash/c symbol? (listof obj?)) (listof obj?))
(define/contract (spec #:ver ver
                       #:rel [rel #f]
                       #:srcs [srcs null]
                       #:subdir [subdir #f]
                       #:chksums [chksums null]
                       #:chkupdate [chkupdate #f]
                       #:dummysrc? [dummysrc? #f])
  (->* (#:ver string?)
       (#:rel exact-nonnegative-integer?
              #:srcs (listof src?)
              #:subdir string?
              #:chksums (listof chksum?)
              #:chkupdate chkupdate?
              #:dummysrc? boolean?)
       spec-type?)
  (spec-type ver rel srcs chksums chkupdate dummysrc?))
