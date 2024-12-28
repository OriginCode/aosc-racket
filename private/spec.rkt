#lang racket/base

(require racket/contract
         racket/string
         rackunit
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

(define spec-tests
  (test-suite "SPEC tests"
    (test-suite "SRCS options tests"
      (check-equal? (branch "next") (src-option 'branch "next") "branch option")

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
      (check-equal? (git "https://github.com/NVIDIA/nvidia-settings")
                    (src 'git null "https://github.com/NVIDIA/nvidia-settings")
                    "SRCS git no options")
      (check-equal? (git "https://github.com/NVIDIA/nvidia-settings"
                         #:options
                         (list (copy-repo? #t)
                               (rename "nvidia")
                               (commit (string-append "tags/v" "0.0.1"))
                               (submodule 'recursive)))
                    (src 'git
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
       (src
        'tbl
        (list (rename "racket"))
        "https://download.racket-lang.org/releases/8.15/installers/racket-8.15-src.tgz")
       "SRCS tbl with options")
      (check-equal?
       (tbl
        "https://download.racket-lang.org/releases/8.15/installers/racket-8.15-src.tgz")
       (src
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

    (test-suite "CHKSUMS tests"
      (check-equal?
       (chksums->string
        (list
         (skip)
         (checksum
          'sha256
          "4ccdbbd95d4aef058502c8ee07b1abb490f5ef4a4d6ff711440facd0b8eded33")))
       "CHKSUMS=\"SKIP sha256::4ccdbbd95d4aef058502c8ee07b1abb490f5ef4a4d6ff711440facd0b8eded33\""
       "CHKSUMS ->string"))))
