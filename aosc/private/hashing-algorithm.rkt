#lang racket/base

(require racket/contract)

(provide (all-defined-out))

(define/contract (algorithm? sym)
  (-> symbol? boolean?)
  (and
   (member
    sym
    '(md2 md5 sha1 sha256 sha224 sha384 sha512 blake2b blake2s sha3_224 sha3_256 sha3_384 sha3_512))
   #t))
