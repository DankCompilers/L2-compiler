#lang racket




;; string? -> string?
(define (L2-to-L1-compiler raw-L2)
  (cond
    [(string? raw-L2)                                (ast-to-string (L2-to-L1 (parse-program raw-L2)))]
    [(and (AST? raw-L2) (is-program-node? raw-L2))   (ast-to-string (L2-to-L1 raw-L2))]
    [else (lambda () (error "Compiler given invalid L2 source"))]))
