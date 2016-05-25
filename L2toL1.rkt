#lang racket


(require "parser.rkt"
         "AST.rkt"
         "liveness.rkt"
         "backend.rkt"
         "graph-color.rkt"
         "spill.rkt")

(provide L2->L1-compile L2->L1)

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
(define (L2->L1-compile quoted-raw-L2)
  (let ([new-ast       (L2->L1 (parse-program quoted-raw-L2))])
    (if (AST? new-ast)
        (ast-to-string new-ast)
        new-ast)))
  
  ;(cond
  ;  [(string? raw-L2)                                (ast-to-string (L2-to-L1 (parse-program raw-L2)))]
  ;  [(and (AST? raw-L2) (is-program-node? raw-L2))   (ast-to-string (L2-to-L1 raw-L2))]
  ;  [else                                            (error "Compiler given invalid L2 source")]))


;; Consumes an L2 AST and converts it to its equivalent L1 AST
;; l2-program-ast -> l1-program-ast
(define (L2->L1 program-ast)
  (let ([new-children   (for/list ([func-ast (AST-children  program-ast)])
                              (let* ([analysis       (analyze-func-ast func-ast)])
                                (if (not (list? analysis))
                                    ;; return could not register allocate
                                    analysis
                                    ;; else replace the variables with their registers/stack-args with mem node
                                    (let ([new-func-ast   (first analysis)]
                                          [colored-graph  (second analysis)])
                                      (process-func-ast new-func-ast colored-graph)))))])
    (cond
      [(not (empty? (filter (negate AST?) new-children))) (println "could not register allocate") "could not register allocate"]
        ;; replace the children (functions) with the transformed code
       [else (set-AST-children program-ast new-children)])))


;; Performs liveness analysis, register allocation, spilling, and then returns
;; a func ast that has the variable ID's replaced with their register
;; func-AST ->listof(func-ast colored-graph)/string
(define (analyze-func-ast func-ast)
  (println "Analyzing function")
  (let* ([liveness-data  (generate-liveness-info func-ast)]
         [gens           (hash-ref liveness-data 'gens)]
         [kills          (hash-ref liveness-data 'kills)]
         [successors     (hash-ref liveness-data 'successors)]
         [ins            (hash-ref liveness-data 'ins)]
         [outs           (hash-ref liveness-data 'outs)]
         [colored-graph  (allocate-function func-ast ins outs kills)]
         )
    (println colored-graph)
    (match colored-graph
      ;; graph is a variable id to spill, so spill it and reanalyze
      [(? symbol?)     (analyze-func-ast (spill-function colored-graph spill-prefix func-ast gens kills))]
      ;; successfully colored graph
      [(? hash?)       (list func-ast colored-graph)]
      ;; impossible to allocate function
      [(? boolean?)    "could not register allocate"])
    )
  )



;; Iterate through intructions and replace variable uses with their allocated register
;; and also change stack nodes to mem nodes
;; func-AST colored-graph -> func-AST
(define (process-func-ast func-ast colored-graph)
  ;; AST -> AST
  (define (replace-variables a-ast)
    (cond
      [(is-var-node? a-ast)          (parse-token (hash-ref colored-graph (get-first-data a-ast)))]
      [else                          (set-AST-children a-ast (map replace-variables (AST-children a-ast)))]))
  (set-AST-children func-ast
                    ;; iterates through instruction AST's and replaces the ID's, and changes memstack2w to memmem2w
                    (for/list ([i-ast (AST-children func-ast)])
                              (let ([replaced-ast   (replace-variables i-ast)])
                                (if (is-memstack-node? replaced-ast)
                                    (stack-to-mem-node replaced-ast)
                                    replaced-ast)))))


;; Converts a stack-ast to it's equivalent mem-ast
;; stack-ast -> mem-ast
(define (stack-to-mem-node stack-ast)
  (define base-x (parse-token 'rsp))
  (AST 'mem '() (list base-x (get-first-child stack-ast))))