#lang racket/base

(require aosc
         rackunit)

(provide (all-defined-out))

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
