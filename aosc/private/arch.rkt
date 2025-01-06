#lang racket/base

(require racket/contract)

(provide (all-defined-out))

(define/contract (arch? sym)
  (-> symbol? arch?)
  (or 'any 'amd64 'arm64 'loongarch64 'ppc64el 'loongson3 'riscv64))
