#lang racket/base

(require racket/contract
         net/url)

(module+ test
  (require rackunit))

(struct src (type value) #:transparent)

(define/contract (git commit url)
  (-> string? url? src?)
  (src 'git (format "commit=~a::~a" commit url)))

(struct chksum (type value) #:transparent)

(define/contract (sha256 value)
  (-> string? chksum?)
  (chksum 'sha256 value))

(define/contract (skip)
  (-> chksum?)
  (chksum 'skip #f))

(struct chkupdate (type value) #:transparent)

(define/contract (anitya id)
  (-> exact-nonnegative-integer? chkupdate?)
  (chkupdate 'anitya (format "id=~a" id)))

(struct spec-type (ver rel srcs chksums chkupdate dummysrc?) #:transparent)

(define/contract (spec #:ver ver
                       #:rel [rel #f]
                       #:srcs [srcs #f]
                       #:chksums [chksums #f]
                       #:chkupdate [chkupdate #f]
                       #:dummysrc? [dummysrc? #f])
  (->* (#:ver string?)
       (#:rel exact-nonnegative-integer?
        #:srcs (listof src?)
        #:chksums (listof chksum?)
        #:chkupdate chkupdate?
        #:dummysrc? boolean?)
       spec-type?)
  (spec-type ver rel srcs chksums chkupdate dummysrc?))

(module+ test
  (define test-spec
    (spec #:ver "1.1.4"
          #:rel 514
          #:srcs (list (git "tags/$VER" (string->url
                                         "https://github.com/AOSC-Dev/systemd-boot-friend-rs")))
          #:chksums (list (skip))
          #:chkupdate (anitya 226819)
          #:dummysrc? #f))
  (displayln test-spec))
