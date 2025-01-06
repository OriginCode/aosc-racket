#lang racket/base

(require racket/contract
         racket/string
         net/url)

(provide (all-defined-out))

(define/contract (boolean->symbol b)
  (-> boolean? symbol?)
  (if b 'true 'false))

(define/contract (spec-entries->string
                  entry-name entries entry->string [arch 'any])
  (->* (string? (listof any/c) (-> any/c string?))
       (symbol?)
       string?)
  (string-append entry-name
                 (if (equal? arch 'any)
                     ""
                     (string-append "__" (string-upcase (symbol->string arch))))
                 "=\""
                 (string-join (map entry->string entries))
                 "\""))

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

(define/contract (srcs->string srcs [arch 'any])
  (->* ((listof src?)) (symbol?) string?)
  (spec-entries->string "SRCS" srcs src->string arch))

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

(define/contract (chksums->string chksums [arch 'any])
  (->* ((listof chksum?)) (symbol?) string?)
  (spec-entries->string "CHKSUMS" chksums chksum->string arch))

;; CHKUPDATE= field (https://wiki.aosc.io/developer/packaging/aosc-findupdate/)
;;==================
(struct chkupdate (type value) #:transparent)

(define/contract (anitya id)
  (-> exact-nonnegative-integer? chkupdate?)
  (chkupdate 'anitya (format "id=~a" id)))

(define/contract (github repo [pattern #f] [sort-version #f])
  (->* (string?)
       ((or/c boolean? string?)
        boolean?)
       chkupdate?)
  (chkupdate 'github
             (string-append
                     (string-append "repo="repo)
                     (if pattern
                         (string-append ";pattern=" pattern)
                         "")
                     (if sort-version
                         (string-append ";sort_version=" (symbol->string (boolean->symbol
                                                           sort-version)))
                         ""))))

(define/contract (gitlab repo [instance #f] [pattern #f] [sort-version #f])
  (->* (string?)
       ((or/c boolean? string?)
        (or/c boolean? string?)
        boolean?)
       chkupdate?)
  (chkupdate 'gitlab
             (string-append
                     (string-append "repo="repo)
                     (if instance
                         (string-append ";instance=" instance)
                         "")
                     (if pattern
                         (string-append ";pattern=" pattern)
                         "")
                     (if sort-version
                         (string-append ";sort_version=" (symbol->string (boolean->symbol
                                                           sort-version)))
                         ""))))

(define/contract (gitweb url [pattern #f])
  (->* (string?)
       ((or/c boolean? string?))
       chkupdate?)
  (chkupdate 'gitweb
             (string-append
                     (string-append "url=" url)
                     (if pattern
                         (string-append ";pattern=" pattern)
                         "")
                     )))

(define/contract (git-generic url [pattern #f])
  (->* (string?)
       ((or/c boolean? string?))
       chkupdate?)
  (chkupdate 'git
             (string-append
                     (string-append "url=" url)
                     (if pattern
                         (string-append ";pattern=" pattern)
                         "")
                     )))

(define/contract (html url pattern)
  (-> string? string?
       chkupdate?)
  (chkupdate 'gitweb
             (string-append
                     "url=" url
                     ";pattern=" pattern)))

(define/contract (chkupdate->string chkupdate)
  (-> chkupdate? string?)
  (string-append "CHKUPDATE="
                 (symbol->string (chkupdate-type chkupdate))
                 "::"
                 (chkupdate-value chkupdate)))

;; Whole `spec` file struct
(struct spec-type (ver rel srcs chksums chkupdate dummysrc?) #:transparent)

;; `spec` constructor
(define/contract (spec #:ver ver
                       #:rel [rel #f]
                       #:srcs [srcs null]
                       #:subdir [subdir #f]
                       #:chksums [chksums null]
                       #:chkupdate [chkupdate #f]
                       #:dummysrc? [dummysrc? #f])
  (->* (#:ver string?)
       (#:rel exact-nonnegative-integer?
              #:srcs (or/c (hash/c symbol? (listof src?)) (listof src?))
              #:subdir string?
              #:chksums (or/c (hash/c symbol? (listof chksum?)) (listof chksum?))
              #:chkupdate chkupdate?
              #:dummysrc? boolean?)
       spec-type?)
  (spec-type ver rel srcs chksums chkupdate dummysrc?))
