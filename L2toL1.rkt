#lang racket


(require "parser.rkt" "AST.rkt" "liveness.rkt" "backend.rkt" "graph-color.rkt" "spill.rkt")
(provide L2-to-L1-compiler L2-to-L1)

#| This module provides L2-to-L1 which consumes a valid L2 program in abstract syntax tree form
   and returns a valid L1 abstract syntax tree. It accomplishes this by doing the following:
     - Allocating a register for every variable and spilling those that won't.
     - Processing the syntax tree and replacing variable uses with registers
     - Processing the syntax tree and replacing stack to w assignments with the corresponsing mem to w

   The provided compiler consumes a L2 program string, parses it into L2 AST, changes it to a valid L1 AST
   then uses the backend to change the program AST into a string
|#

(define spill-prefix "L2_")

;; Consumes an L2 program string and returns the related L1 program string
;; string? -> string?
(define/contract (L2-to-L1-compiler raw-L2)
  ;;(-> (or/c string? AST?) string?)
  (cond
    [(string? raw-L2)                                (ast-to-string (L2-to-L1 (parse-program raw-L2)))]
    [(and (AST? raw-L2) (is-program-node? raw-L2))   (ast-to-string (L2-to-L1 raw-L2))]
    [else (lambda () (error "Compiler given invalid L2 source"))]))


;; Consumes an L2 AST and converts it to its equivalent L1 AST
;; l2-program-ast -> l1-program-ast
(define (L2-to-L1 program-ast)
  ;; replace the children (functions) with the transformed code
  (set-AST-children program-ast
                    (for/list ([func-ast (AST-children  program-ast)])
                              (let* ([analysis       (analyze-func-ast func-ast)]
                                     [new-func-ast   (first analysis)]
                                     [colored-graph  (second analysis)])
                                (process-func-ast new-func-ast colored-graph)))))


;; Performs liveness analysis, register allocation, spilling, and then returns
;; a func ast that has the variable ID's replaced with their register
;; func-AST ->listof(func-ast colored-graph)/error
(define (analyze-func-ast func-ast)
  (let* ([liveness-data  (liveness-analysis func-ast)]
         [gens           (first liveness-data)]
         [kills          (second liveness-data)]
         [successors     (third liveness-data)]
         [ins            (fourth liveness-data)]
         [outs           (fifth liveness-data)]
         [colored-graph  (allocate-function func-ast ins outs kills)])
    (match colored-graph
      ;; graph is a variable id to spill, so spill it and reanalyze
      [symbol?     (analyze-func-ast (spill-function colored-graph spill-prefix func-ast gens kills))]
      ;; successfully colored graph
      [hash?       (list func-ast colored-graph)]
      ;; impossible to allocate function
      [boolean?    (lambda () (error "Impossible to allocate function"))])))



;; Iterate through intructions and replace variable uses with their allocated register
;; and also change stack nodes to mem nodes
;; func-AST colored-graph -> func-AST
(define (process-func-ast func-ast colored-graph)
  ;; Replaces variable nodes with register nodes and stack nodes with mem nodes
  ;; AST -> AST
  (define (child-replace child-ast)
    (cond
      [(is-var-node? child-ast)      (parse-token (hash-ref colored-graph (get-first-data child-ast)))]
      [(is-stack-node? child-ast)    (stack-to-mem-node child-ast)] 
      [else child-ast]))
  
  (set-AST-children func-ast
                    ;; iterates through instruction AST's and replaces the ID's, and changes memstack2w to memmem2w
                    (for/list ([i-ast (AST-children func-ast)])
                              (let ([replaced-ast   (set-AST-children i-ast (map child-replace (AST-children i-ast)))])
                                (if (is-memstack-node? replaced-ast)
                                    (replace-type replaced-ast 'memmem2w)
                                    replaced-ast)))))


;; Converts a stack-ast to it's equivalent mem-ast
;; stack-ast -> mem-ast
(define (stack-to-mem-node stack-ast)
  (define base-x (parse-token 'rsp))
  (AST 'mem '() (list base-x (get-first-child stack-ast))))