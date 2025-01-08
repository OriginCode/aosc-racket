#lang scribble/manual
@require[@for-label[aosc
                    racket/base]
          scribble/example]

@title{AOSC Racket Packaging Tools}
@author[(author+email "Kaiyang Wu" "origincode@aosc.io")]

@defmodule[aosc]

AOSC Racket Packaging Tools provides tools to write an AOSC package definition
in Racket, and to convert to the good old @tt{spec} and @tt{defines} files.

@section{Specification File}

@subsection{Example}

An example @tt{spec} written in Racket is shown below. Let's get started by requiring
the @tt{aosc} module:

@racket[(require aosc)]

A minimal @racket[spec] requires a @racket[ver] and a @racket[dummysrc?] set to
@racket[#t].

@racketblock[
(spec #:ver "0.0.1"
      #:dummysrc? #t)
]

Voilà! We have a small @tt{spec}.

The following is a real world example with the sample package @tt{obs-studio}:

@examples[
(require aosc)

(define (obs-studio)
  (define version "31.0.0")
  (define obs-cef-ver "127.3.5+g114ea2a+chromium-127.0.6533.120")
  (define common-src
    (git "https://github.com/obsproject/obs-studio"
         #:options (list (commit (string-append "tags/" version))
                         (rename "obs-studio"))))
  (spec
   #:ver version
   #:rel 1
   #:srcs
   (hash 'any   (list common-src)
         'amd64 (list common-src
                      (tbl (format
                             "https://cef-builds.spotifycdn.com/cef_binary_~a_linux64_minimal.tar.bz2"
                             obs-cef-ver)
                           #:options (list (rename "cef.tar.bz2"))))
         'arm64 (list common-src
                      (tbl (format
                             "https://cef-builds.spotifycdn.com/cef_binary_~a_linuxarm64_minimal.tar.bz2"
                             obs-cef-ver)
                           #:options (list (rename "cef.tar.bz2")))))
   #:chksums
   (hash 'any   (list (skip))
         'amd64 (list (skip)
                      (checksum 'sha256 "e7b13b6d92a95625a7a3d5b801a6928cbf41e2ec6afab0d7028bb5b6c88e0d48"))
         'arm64 (list (skip)
                      (checksum 'sha256 "b3c751f7bac03b49825306e96273d6c98dedf26a2fab4e4785964a965859741b")))
   #:subdir "obs-studio"
   #:chkupdate (anitya 7239)))

(write-spec (obs-studio) (current-output-port))
]

@subsection{Reference}

@defproc[(spec-type? [obj any/c]) boolean?]{
Returns @racket[#t] if @racket[obj] is a specification struct, @racket[#f]
otherwise.
}

@defproc[(spec [#:ver ver string?]
               [#:rel rel exact-nonnegative-integer? #f]
               [#:srcs srcs (or/c (hash/c arch? (listof src?)) (listof src?))
                null]
               [#:subdir subdir string? #f]
               [#:chksums chksums (or/c (hash/c arch? (listof chksum?)) (listof
                                                                          chksum?))
                null]
               [#:chkupdate chkupdate chkupdate? #f]
               [#:dummysrc? dummysrc? boolean? #f])
         spec-type?]{
Generates a @racket[spec-type?] to be used or converted to a plain text @tt{spec}
file. Check @url{https://wiki.aosc.io/developer/packaging/acbs/spec-format/} and
@url{https://wiki.aosc.io/developer/packaging/aosc-findupdate/} for more information.

@racket[srcs] and @racket[chksums] can either be lists of sources and checksums
or a @racket[hash] of architecture symbol and list of sources or checksums pair.

Currently supported architectures: @racket['amd64], @racket['arm64],
@racket['loongarch64], @racket['ppc64el], @racket['loongson3],
@racket['riscv64].
}

@defproc[(write-spec [spec spec-type?]
                     [out output-port?])
         void?]{
Writes @racket[spec] to @racket[out] in the form of a subset of
@hyperlink["https://wiki.aosc.io/developer/automation/apml/" "APML"].
}

@subsubsection{@racket[SRCS] Options and Types}

@defproc[(src-option? [obj any/c]) boolean?]{
Returns @racket[#t] if @racket[obj] is a @tt{src} option, @racket[#f]
otherwise.
}

@defproc*[([(branch [name string?]) src-option?]
           [(commit [hash string?]) src-option?]
           [(rename [name string?]) src-option?]
           [(submodule [method (or/c boolean? symbol?)]) src-option?]
           [(copy-repo? [val boolean?]) src-option?])]{
@tt{SRCS} options, @racket[submodule] accepts either @racket['recursive] or a
@racket[boolean?] value.
}

@defproc[(src? [obj any/c]) boolean?]{
Returns @racket[#t] if @racket[obj] is a @tt{src}, @racket[#f] otherwise.
}

@defproc*[([(git [url string?]
                 [#:options options (listof src-option?) null])
            src?]
           [(tbl [url string?]
                 [#:options options (listof src-option?) null])
            src?])]{
@tt{SRCS} types, @racket[tbl] only accepts @racket[rename] option.
}

@subsubsection{@racket[CHKSUMS] Types}

@defproc[(chksum? [obj any/c]) boolean?]{
Returns @racket[#t] if @racket[obj] is a @tt{chksum}, @racket[#f] otherwise.
}

@defproc*[([(checksum [algorithm algorithm?]
                      [value string?])
            chksum?]
           [(skip) chksum?])]{
@tt{CHKSUMS} types, for supported hashing algorithm symbols, check
@url{https://wiki.aosc.io/developer/packaging/acbs/appendix/#supported-checksum-hashing-algorithm}
for more information.

@racket[skip] is an empty placeholder for skipping checksum verification.
}

@subsubsection{@racket[CHKUPDATE] Types}

@defproc[(chkupdate? [obj any/c]) boolean?]{
Returns @racket[#t] if @racket[obj] is a @tt{chkupdate}, @racket[#f] otherwise.
}

@defproc*[([(anitya [id exact-nonnegative-integer?]) chkupdate?]
           [(github [repo string?]
                    [pattern string? #f]
                    [sort-version boolean? #f])
            chkupdate?]
           [(gitlab [repo string?]
                    [instance string? #f]
                    [pattern string? #f]
                    [sort-version boolean? #f])
            chkupdate?]
           [(gitweb [url string?]
                    [pattern string? #f])
            chkupdate?]
           [(git-generic [url string?]
                         [pattern string? #f])
            chkupdate?]
           [(html [url string?]
                  [pattern string?])
            chkupdate?])]{
@tt{CHKUPDATE} types, check
@hyperlink["https://wiki.aosc.io/developer/packaging/aosc-findupdate"
           "aosc-findupdate"]
for more information.
}
