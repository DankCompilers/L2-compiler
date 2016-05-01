#lang racket

(require "lib.rkt" "liveness.rkt" "AST.rkt")

(define all-registers (set r8 r9 r10 r11 r12 r13 r14 r15 rbp rbx rdi rsi rdx rcx rax))

;; a graph is a hashset of variables and registers that maintain a set of variables and registers that it interferes with
(define initial-map
  (make-hash (list 



(define (color-graph-program graph)
  (empty))

(define (generate-graph-program



#| Build interference graph from the liveness information
- For each instruction:
--- Two variables live at the same time interfere with each other
--- Killed variables interfere with variables in the out set
--- Except that the variables x and y do not interfere if the instruction was (x <- y)
- All real registers interfere with each other
- Handle constrained arithmetic via extra edges |#

(define (generate-graph-function ins outs kills f-ast)
  (let ([graph ()]
        [instructions (AST-children f-ast)])
    (for ([i (range (length ins))])
         (let ([in  (list-ref ins i)]
               [out (list-ref outs i)]
               [kill (list-ref kills i)]
               [i-ast (list-ref instructions i)])
           ()))))