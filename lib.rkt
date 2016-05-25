#lang racket

(require "AST.rkt")
(provide (all-defined-out))

;; symbol|string -> string
(define (to-string token)
  (format "~a" token))



;; AST -> NAT
(define (calculate-spillage func-ast)
  (let* ([num-locals  (get-third-data  func-ast)])
    (* 8  num-locals)))


(define (print-hash a-hash)
  (for ([a-pair (hash->list a-hash)])
       (printf  "~a: ~a\n" (car a-pair) (cdr a-pair)))
  (printf "\n\n"))

(define (print-adj a-adj-graph)
  (for ([a-list a-adj-graph])
       (printf "~a\n" a-list))
  (printf "\n\n"))


#|
(define (make-debug-print)
  (let ([debug  #f])
    (define (debug-print str . args)
      (when print-debug (apply printf (append (list str) args))))
    (define 
    (list (lambda ()|# 
