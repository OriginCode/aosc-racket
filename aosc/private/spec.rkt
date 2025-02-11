#lang racket/base

(require racket/bool
         racket/contract
         racket/string
         net/url
         "hashing-algorithm.rkt"
         "arch.rkt"
         "utils.rkt")

(provide src-option?
         branch
         commit
         rename
         submodule
         copy-repo?
         src-type?
         git
         tbl
         chksum-type?
         skip
         checksum
         anitya
         github
         gitlab
         gitweb
         git-generic
         html
         (contract-out
          [struct ver ((val string?))]
          [struct rel ((val exact-nonnegative-integer?))]
          [struct srcs ((val (or/c (hash/c arch? (listof src-type?)) (listof src-type?))))]
          [struct subdir ((val string?))]
          [struct chksums ((val (or/c (hash/c arch? (listof chksum-type?)) (listof chksum-type?))))]
          [struct chkupdate ((val chkupdate-type?))]
          [struct dummysrc? ((val boolean?))]
          [struct spec-type ((ver ver?)
                             (rel rel?)
                             (srcs srcs?)
                             (subdir subdir?)
                             (chksums chksums?)
                             (chkupdate chkupdate?)
                             (dummysrc? dummysrc??))])
         spec
         write-spec)

(module+ test
  (require rackunit))

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
      (unless (equal? method 'recursive)
        (raise-argument-error 'submodule "boolean? or 'recursive" method))
      method])))

(define/contract (copy-repo? val)
  (-> boolean? src-option?)
  (src-option 'copy-repo (boolean->symbol val)))

;; SRCS= field
;;=============
(struct src-type (type options value) #:transparent)
(struct srcs (val) #:transparent)

(define/contract (git url #:options [options null])
  (->* (string?) (#:options (listof src-option?)) src-type?)
  (src-type 'git options url))

(define/contract (tbl url #:options [options null])
  (->* (string?) (#:options (listof src-option?)) src-type?)
  (define allowed-options '(rename))
  (for ([opt (in-list options)]
        #:when (not (member (src-option-field opt) allowed-options)))
    (raise-user-error 'tbl "~a option is not allowed in tbl source" (src-option-field opt)))
  (src-type 'tbl options url))

(define/contract (src->string src)
  (-> src-type? string?)
  (string-join `(,(symbol->string (src-type-type src))
                 ,@(if (null? (src-type-options src))
                       null
                       (list (string-join (for/list ([opt (src-type-options src)])
                                            (format "~a=~a"
                                                    (src-option-field opt)
                                                    (src-option-value opt)))
                                          ";")))
                 ,(src-type-value src))
               "::"))

(define/contract (srcs->string srcs [arch 'any])
  (->* ((listof src-type?)) (arch?) string?)
  (spec-entry->string "SRCS" (string-join (map src->string srcs)) #t arch))

;; CHKSUMS= field
;;================
(struct chksum-type (type value) #:transparent)
(struct chksums (val) #:transparent)

(define/contract (checksum algorithm value)
  (-> algorithm? string? chksum-type?)
  (chksum-type algorithm value))

(define/contract (skip)
  (-> chksum-type?)
  (chksum-type 'skip #f))

(define/contract (chksum->string chksum)
  (-> chksum-type? string?)
  (if (equal? (chksum-type-type chksum) 'skip)
      "SKIP"
      (format "~a::~a" (chksum-type-type chksum) (chksum-type-value chksum))))

(define/contract (chksums->string chksums [arch 'any])
  (->* ((listof chksum-type?)) (arch?) string?)
  (spec-entry->string "CHKSUMS"
                      (string-join (map chksum->string chksums))
                      #t
                      arch))

;; CHKUPDATE= field (https://wiki.aosc.io/developer/packaging/aosc-findupdate/)
;;==================
(struct chkupdate-type (type value) #:transparent)
(struct chkupdate (val) #:transparent)

(define/contract (anitya id)
  (-> exact-nonnegative-integer? chkupdate-type?)
  (chkupdate-type 'anitya (format "id=~a" id)))

(define/contract (github repo (pattern #f) [sort-version #f])
  (->* (string?) (string? boolean?) chkupdate-type?)
  (chkupdate-type 'github
             (string-append (string-append "repo=" repo)
                            (if pattern
                                (string-append ";pattern=" pattern)
                                "")
                            (if sort-version
                                (string-append ";sort_version="
                                               (symbol->string (boolean->symbol
                                                                sort-version)))
                                ""))))

(define/contract (gitlab repo [instance #f] (pattern #f) [sort-version #f])
  (->* (string?) (string? string? boolean?) chkupdate-type?)
  (chkupdate-type 'gitlab
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

(define/contract (gitweb url (pattern #f))
  (->* (string?) (string?) chkupdate-type?)
  (chkupdate-type 'gitweb
             (string-append (string-append "url=" url)
                            (if pattern
                                (string-append ";pattern=" pattern)
                                ""))))

(define/contract (git-generic url (pattern #f))
  (->* (string?) (string?) chkupdate-type?)
  (chkupdate-type 'git
             (string-append (string-append "url=" url)
                            (if pattern
                                (string-append ";pattern=" pattern)
                                ""))))

(define/contract (html url pattern)
  (-> string? string? chkupdate-type?)
  (chkupdate-type 'gitweb (string-append "url=" url ";pattern=" pattern)))

(define/contract (chkupdate->string chkupdate)
  (-> chkupdate-type? string?)
  (spec-entry->string
   "CHKUPDATE"
   (format "~a::~a" (chkupdate-type-type chkupdate) (chkupdate-type-value chkupdate))
   #t))

(struct ver (val) #:transparent)
(struct rel (val) #:transparent)
(struct subdir (val) #:transparent)
(struct dummysrc? (val) #:transparent)

;; Whole `spec` file struct
(struct spec-type (ver rel srcs subdir chksums chkupdate dummysrc?)
  #:transparent)

;; `spec` constructor
(define/contract (spec ver
                       #:rel [rel #f]
                       #:srcs [srcs #f]
                       #:subdir [subdir #f]
                       #:chksums [chksums #f]
                       #:chkupdate [chkupdate #f]
                       #:dummysrc? [dummysrc? #f])
  (->* (ver?)
       (#:rel rel?
        #:srcs srcs?
        #:subdir subdir?
        #:chksums chksums?
        #:chkupdate chkupdate?
        #:dummysrc? dummysrc??)
       spec-type?)
  ;; Checks
  (when (and (or (not srcs?)
                 (not chksums?)
                 (null? (srcs-val srcs))
                 (null? (chksums-val chksums))
                 (hash-empty? (srcs-val srcs))
                 (hash-empty? (chksums-val chksums)))
             (not (or dummysrc? (dummysrc?-val dummysrc?))))
    (raise-user-error 'spec "either add SRCS and CHKSUMS or specify DUMMYSRC"))
  (when (xor (list? (srcs-val srcs)) (list? (chksums-val chksums)))
    (raise-user-error 'spec "SRCS and CHKSUMS should have the same contract"))
  (when (and (hash? (srcs-val srcs))
             (not (equal? (hash-keys (srcs-val srcs) #t) (hash-keys (chksums-val chksums) #t))))
    (raise-user-error 'spec "SRCS and CHKSUMS have mismatching ARCH args"))
  (unless (if (list? (srcs-val srcs))
              (equal? (length (srcs-val srcs)) (length (chksums-val chksums)))
              (andmap (λ (s c) (equal? (length s) (length c)))
                      (hash->list (srcs-val srcs) #t)
                      (hash->list (chksums-val chksums) #t)))
    (raise-user-error 'spec "SRCS and CHKSUMS length mismatch"))

  ;; Final struct
  (spec-type ver rel srcs subdir chksums chkupdate dummysrc?))

(define/contract (write-spec spec out)
  (-> spec-type? output-port? void?)
  (displayln (spec-entry->string "VER" (ver-val (spec-type-ver spec))) out)
  (when (spec-type-rel spec)
    (displayln (spec-entry->string "REL" (rel-val (spec-type-rel spec))) out))
  (unless (spec-type-dummysrc? spec)
    (if (list? (srcs-val (spec-type-srcs spec)))
        (displayln (srcs->string (srcs-val (spec-type-srcs spec))) out)
        (for ([arch-srcs (hash->list (srcs-val (spec-type-srcs spec)))])
          (displayln (srcs->string (cdr arch-srcs) (car arch-srcs)) out))))
  (when (spec-type-subdir spec)
    (displayln (spec-entry->string "SUBDIR" (subdir-val (spec-type-subdir spec)) #t) out))
  (unless (spec-type-dummysrc? spec)
    (if (list? (chksums-val (spec-type-chksums spec)))
        (displayln (chksums->string (chksums-val (spec-type-chksums spec))) out)
        (for ([arch-chksums (hash->list (chksums-val (spec-type-chksums spec)))])
          (displayln (chksums->string (cdr arch-chksums) (car arch-chksums)) out))))
  (when (spec-type-chkupdate spec)
    (displayln (chkupdate->string (chkupdate-val (spec-type-chkupdate spec))) out))
  (when (spec-type-dummysrc? spec)
    (displayln
     (spec-entry->string "DUMMYSRC"
                         (number->string (boolean->exact-nonnegative-integer
                                          (dummysrc?-val (spec-type-dummysrc? spec)))))
     out)))

(module+ test
  (require rackunit/text-ui)
  (define spec-tests
    (test-suite "SPEC tests"
      (test-suite "SRCS options tests"
        (check-equal? (branch "next")
                      (src-option 'branch "next")
                      "branch option")

        (check-equal? (commit "4ad8776a633")
                      (src-option 'commit "4ad8776a633")
                      "commit option")

        (check-equal? (rename "out") (src-option 'rename "out") "rename option")

        (check-equal? (submodule #t)
                      (src-option 'submodule 'true)
                      "submodule option: true")
        (check-equal? (submodule #f)
                      (src-option 'submodule 'false)
                      "submodule option: false")
        (check-equal? (submodule 'recursive)
                      (src-option 'submodule 'recursive)
                      "submodule option: recursive")
        (check-exn exn:fail:contract?
                   (λ () (submodule 'huh))
                   "MUST FAIL submodule option: unknown symbol")

        (check-equal? (copy-repo? #t)
                      (src-option 'copy-repo 'true)
                      "copy-repo option"))

      (test-suite "SRCS Git SRC tests"
        (check-equal?
         (git "https://github.com/NVIDIA/nvidia-settings")
         (src-type 'git null "https://github.com/NVIDIA/nvidia-settings")
         "SRCS git no options")
        (check-equal? (git "https://github.com/NVIDIA/nvidia-settings"
                           #:options
                           (list (copy-repo? #t)
                                 (rename "nvidia")
                                 (commit (string-append "tags/v" "0.0.1"))
                                 (submodule 'recursive)))
                      (src-type 'git
                           (list (copy-repo? #t)
                                 (rename "nvidia")
                                 (commit (string-append "tags/v" "0.0.1"))
                                 (submodule 'recursive))
                           "https://github.com/NVIDIA/nvidia-settings")
                      "SRCS git with options")

        (check-equal?
         (src->string (git "https://github.com/NVIDIA/nvidia-settings"))
         "git::https://github.com/NVIDIA/nvidia-settings"
         "SRCS ->string git no options")
        (check-equal?
         (src->string (git "https://github.com/NVIDIA/nvidia-settings"
                           #:options
                           (list (copy-repo? #t)
                                 (rename "nvidia")
                                 (commit (string-append "tags/v" "0.0.1"))
                                 (submodule 'recursive))))
         "git::copy-repo=true;rename=nvidia;commit=tags/v0.0.1;submodule=recursive::https://github.com/NVIDIA/nvidia-settings"
         "SRCS ->string git with options"))

      (test-suite "SRCS Tarball SRC tests"
        (check-exn
         exn:fail:user?
         (λ ()
           (tbl
            "https://download.racket-lang.org/releases/8.15/installers/racket-8.15-src.tgz"
            #:options (list (copy-repo? #t)
                            (rename "racket")
                            (commit (string-append "tags/v" "0.0.1"))
                            (submodule 'recursive))))
         "MUST FAIL SRCS tbl with illegal options")
        (check-equal?
         (tbl
          "https://download.racket-lang.org/releases/8.15/installers/racket-8.15-src.tgz"
          #:options (list (rename "racket")))
         (src-type
          'tbl
          (list (rename "racket"))
          "https://download.racket-lang.org/releases/8.15/installers/racket-8.15-src.tgz")
         "SRCS tbl with options")
        (check-equal?
         (tbl
          "https://download.racket-lang.org/releases/8.15/installers/racket-8.15-src.tgz")
         (src-type
          'tbl
          null
          "https://download.racket-lang.org/releases/8.15/installers/racket-8.15-src.tgz")
         "SRCS tbl no options")

        (check-equal?
         (src->string
          (tbl
           "https://download.racket-lang.org/releases/8.15/installers/racket-8.15-src.tgz"))
         "tbl::https://download.racket-lang.org/releases/8.15/installers/racket-8.15-src.tgz"
         "SRCS ->string tbl no options")
        (check-equal?
         (src->string
          (tbl
           "https://download.racket-lang.org/releases/8.15/installers/racket-8.15-src.tgz"
           #:options (list (rename "racket"))))
         "tbl::rename=racket::https://download.racket-lang.org/releases/8.15/installers/racket-8.15-src.tgz"
         "SRCS ->string tbl with options"))

      (check-equal?
       (srcs->string
        (list
         (tbl
          "https://download.racket-lang.org/releases/8.15/installers/racket-8.15-src.tgz"
          #:options (list (rename "racket")))
         (git "https://github.com/NVIDIA/nvidia-settings"
              #:options (list (copy-repo? #t)
                              (rename "nvidia")
                              (commit (string-append "tags/v" "0.0.1"))
                              (submodule 'recursive)))))
       "SRCS=\"tbl::rename=racket::https://download.racket-lang.org/releases/8.15/installers/racket-8.15-src.tgz git::copy-repo=true;rename=nvidia;commit=tags/v0.0.1;submodule=recursive::https://github.com/NVIDIA/nvidia-settings\""
       "SRCS ->string")

      (check-equal?
       (srcs->string
        (list
         (tbl
          "https://download.racket-lang.org/releases/8.15/installers/racket-8.15-src.tgz"
          #:options (list (rename "racket")))
         (git "https://github.com/NVIDIA/nvidia-settings"
              #:options (list (copy-repo? #t)
                              (rename "nvidia")
                              (commit (string-append "tags/v" "0.0.1"))
                              (submodule 'recursive))))
        'amd64)
       "SRCS__AMD64=\"tbl::rename=racket::https://download.racket-lang.org/releases/8.15/installers/racket-8.15-src.tgz git::copy-repo=true;rename=nvidia;commit=tags/v0.0.1;submodule=recursive::https://github.com/NVIDIA/nvidia-settings\""
       "SRCS ->string with ARCH")

      (test-suite "CHKSUMS tests"
        (check-equal?
         (chksums->string
          (list
           (skip)
           (checksum
            'sha256
            "4ccdbbd95d4aef058502c8ee07b1abb490f5ef4a4d6ff711440facd0b8eded33")))
         "CHKSUMS=\"SKIP sha256::4ccdbbd95d4aef058502c8ee07b1abb490f5ef4a4d6ff711440facd0b8eded33\""
         "CHKSUMS ->string")

        (check-equal?
         (chksums->string
          (list
           (skip)
           (checksum
            'sha256
            "4ccdbbd95d4aef058502c8ee07b1abb490f5ef4a4d6ff711440facd0b8eded33"))
          'amd64)
         "CHKSUMS__AMD64=\"SKIP sha256::4ccdbbd95d4aef058502c8ee07b1abb490f5ef4a4d6ff711440facd0b8eded33\""
         "CHKSUMS ->string with ARCH"))

      (test-suite "CHKUPDATE tests"
        (check-equal?
         (chkupdate->string (gitlab "OriginCode/aosc-racket"
                                    "https://factoria.origincode.me"
                                    "\\d+\\.\\d+\\.\\d+"
                                    #t))
         "CHKUPDATE=\"gitlab::repo=OriginCode/aosc-racket;instance=https://factoria.origincode.me;pattern=\\d+\\.\\d+\\.\\d+;sort_version=true\""
         "CHKUPDATE ->string GitLab"))
         ))

  (run-tests spec-tests))
