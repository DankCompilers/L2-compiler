#lang racket

(require "AST.rkt")
(provide to-string calculate-spillage)

;; symbol|string -> string
(define (to-string token)
  (format "~a" token))



;; AST -> NAT
(define (calculate-spillage func-ast)
  (let ([arg-places (if (> (get-second-data func-ast) 6)
                        (- (get-second-data func-ast) 6)
                        0)])
    (* 8 (+ arg-places (get-third-data func-ast)))))
