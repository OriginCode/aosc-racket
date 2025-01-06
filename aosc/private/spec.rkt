#lang racket/base

(require racket/bool
         racket/contract
         racket/string
         net/url
         "arch.rkt")

(provide branch
          commit
          rename
          submodule
          copy-repo?
          git
          tbl
          skip
          checksum
          anitya
          github
          gitlab
          gitweb
          git-generic
          html
          spec
          write-spec)

(define/contract (boolean->symbol b)
  (-> boolean? symbol?)
  (if b 'true 'false))

(define/contract (boolean->exact-nonnegative-integer b)
  (-> boolean? exact-nonnegative-integer?)
  (if b 1 0))

(define/contract (spec-entry->string entry-name value [arch 'any] [quote? #f])
  (->* (string? string?) (arch? boolean?) string?)
  (string-append entry-name
                 (if (equal? arch 'any)
                     ""
                     (string-append "__" (string-upcase (symbol->string arch))))
                 (if quote? "=\"" "=")
                 value
                 (if quote? "\"" "")))

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
  (->* ((listof src?)) (arch?) string?)
  (spec-entry->string "SRCS" (string-join (map src->string srcs)) arch #t))

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
  (->* ((listof chksum?)) (arch?) string?)
  (spec-entry->string "CHKSUMS"
                      (string-join (map chksum->string chksums))
                      arch
                      #t))

;; CHKUPDATE= field (https://wiki.aosc.io/developer/packaging/aosc-findupdate/)
;;==================
(struct chkupdate (type value) #:transparent)

(define/contract (anitya id)
  (-> exact-nonnegative-integer? chkupdate?)
  (chkupdate 'anitya (format "id=~a" id)))

(define/contract (github repo [pattern #f] [sort-version #f])
  (->* (string?) (string? boolean?) chkupdate?)
  (chkupdate 'github
             (string-append (string-append "repo=" repo)
                            (if pattern
                                (string-append ";pattern=" pattern)
                                "")
                            (if sort-version
                                (string-append ";sort_version="
                                               (symbol->string (boolean->symbol
                                                                sort-version)))
                                ""))))

(define/contract (gitlab repo [instance #f] [pattern #f] [sort-version #f])
  (->* (string?)
       (string? string? boolean?)
       chkupdate?)
  (chkupdate 'gitlab
             (string-append (string-append "repo=" repo)
                            (if instance
                                (string-append ";instance=" instance)
                                "")
                            (if pattern
                                (string-append ";pattern=" pattern)
                                "")
                            (if sort-version
                                (string-append ";sort_version="
                                               (symbol->string (boolean->symbol
                                                                sort-version)))
                                ""))))

(define/contract (gitweb url [pattern #f])
  (->* (string?) (string?) chkupdate?)
  (chkupdate 'gitweb
             (string-append (string-append "url=" url)
                            (if pattern
                                (string-append ";pattern=" pattern)
                                ""))))

(define/contract (git-generic url [pattern #f])
  (->* (string?) (string?) chkupdate?)
  (chkupdate 'git
             (string-append (string-append "url=" url)
                            (if pattern
                                (string-append ";pattern=" pattern)
                                ""))))

(define/contract (html url pattern)
  (-> string? string? chkupdate?)
  (chkupdate 'gitweb (string-append "url=" url ";pattern=" pattern)))

(define/contract (chkupdate->string chkupdate)
  (-> chkupdate? string?)
  (spec-entry->string
   "CHKUPDATE"
   (format "~a::~a" (chkupdate-type chkupdate) (chkupdate-value chkupdate))))

;; Whole `spec` file struct
(struct spec-type (ver rel srcs subdir chksums chkupdate dummysrc?) #:transparent)

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
        #:srcs (or/c (hash/c arch? (listof src?)) (listof src?))
        #:subdir string?
        #:chksums (or/c (hash/c arch? (listof chksum?)) (listof chksum?))
        #:chkupdate chkupdate?
        #:dummysrc? boolean?)
       spec-type?)
  ;; Checks
  (when (xor (list? srcs)
             (list? chksums))
    (raise-user-error 'spec
                      "SRCS and CHKSUMS should have the same contract"))
  (when (and (or (null? srcs)
                 (null? chksums)
                 (hash-empty? srcs)
                 (hash-empty? chksums))
             (not dummysrc?))
    (raise-user-error 'spec "either add SRCS and CHKSUMS or specify DUMMYSRC"))
  (when (and (hash? srcs)
             (not (equal? (hash-keys srcs #t)
                          (hash-keys chksums #t))))
    (raise-user-error 'spec "SRCS and CHKSUMS have mismatching ARCH args"))
  (when (not (if (list? srcs)
                 (equal? (length srcs)
                         (length chksums))
                 (andmap (λ (s c) (equal? (length s)
                                          (length c)))
                         (hash->list srcs #t)
                         (hash->list chksums #t))))
    (raise-user-error 'spec "SRCS and CHKSUMS length mismatch"))

  ;; Final struct
  (spec-type ver rel srcs subdir chksums chkupdate dummysrc?))

(define/contract (write-spec spec out)
  (-> spec-type? output-port? void?)
  (displayln (spec-entry->string "VER" (spec-type-ver spec)) out)
  (when (spec-type-rel spec)
      (displayln (spec-entry->string "REL" (spec-type-rel spec)) out))
  (when (not (spec-type-dummysrc? spec))
    (if (list? (spec-type-srcs spec))
      (displayln (srcs->string (spec-type-srcs spec)) out)
      (for ([arch-srcs (hash->list (spec-type-srcs spec))])
        (displayln (srcs->string (cdr arch-srcs)
                               (car arch-srcs)) out))))
  (when (spec-type-subdir spec)
    (displayln (spec-entry->string "SUBDIR" (spec-type-subdir spec)) out))
  (when (not (spec-type-dummysrc? spec))
    (if (list? (spec-type-chksums spec))
      (displayln (chksums->string (spec-type-chksums spec)) out)
      (for ([arch-chksums (hash->list (spec-type-chksums spec))])
        (displayln (chksums->string (cdr arch-chksums)
                               (car arch-chksums)) out))))
  (when (spec-type-chkupdate spec)
    (displayln (chkupdate->string (spec-type-chkupdate spec)) out))
  (when (spec-type-dummysrc? spec)
    (displayln (spec-entry->string "DUMMYSRC"
                        (number->string (boolean->exact-nonnegative-integer
                          (spec-type-dummysrc? spec)))) out))
  )
