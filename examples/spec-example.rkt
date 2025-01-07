#lang racket/base

(require aosc)

(define (obs-studio)
  (define version "31.0.0")
  (define obs-cef-ver "127.3.5+g114ea2a+chromium-127.0.6533.120")
  (define common-src
    (git "https://github.com/obsproject/obs-studio"
         #:options (list (commit (string-append "tags/" version)) (rename "obs-studio"))))
  (spec
   #:ver version
   #:rel 1
   #:srcs
   (hash
    'any
    (list common-src)
    'amd64
    (list common-src
          (tbl (format "https://cef-builds.spotifycdn.com/cef_binary_~a_linux64_minimal.tar.bz2"
                       obs-cef-ver)
               #:options (list (rename "cef.tar.bz2"))))
    'arm64
    (list common-src
          (tbl (format "https://cef-builds.spotifycdn.com/cef_binary_~a_linuxarm64_minimal.tar.bz2"
                       obs-cef-ver)
               #:options (list (rename "cef.tar.bz2")))))
   #:chksums
   (hash 'any
         (list (skip))
         'amd64
         (list (skip)
               (checksum 'sha256 "e7b13b6d92a95625a7a3d5b801a6928cbf41e2ec6afab0d7028bb5b6c88e0d48"))
         'arm64
         (list (skip)
               (checksum 'sha256 "b3c751f7bac03b49825306e96273d6c98dedf26a2fab4e4785964a965859741b")))
   #:subdir "obs-studio"
   #:chkupdate (anitya 7239)))
