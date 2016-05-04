#lang racket


(require "parser.rkt" "AST.rkt" "liveness.rkt" "backend.rkt")
(provide L2-to-L1-driver)


(define spill-prefix "L2")

;; string? -> string?
(define (L2-to-L1-driver raw-L2)
  (cond
    [(string? raw-L2)                                (L2-to-L1 (parse-program raw-L2))]
    [(and (AST? raw-L2) (is-program-node? raw-L2))   (L2-to-L1 raw-L2)]
    [else (lambda () (error "Compiler given invalid L2 source"))]))


;; program-ast -> string?
(define (L2-to-L1 program-ast)
  (ast-to-string (set-AST-children program-ast
                                   (for/list ([func-ast (AST-children  program-ast)])
                                             (analyze-func-ast func-ast))))
  
  ;; Performs liveness analysis, register allocation, spilling, and then returns
  ;; a func ast that has the variable ID's replaced with their register
  ;; func-AST -> func-AST/error
  (define (analyze-func-ast func-ast)
    (let* ([gen-kills      (generate-gen-kills func-ast)]
           [successors     (generate-successors func-ast)]
           [gens           (first gen-kills)]
           [kills          (second gen-kills)]
           [ins-outs       (generate-in-outs gens kills successors)]
           [ins            (first ins-outs)]
           [outs           (second ins-outs)]
           [colored-graph  (allocate-function ins outs kills func-ast)]))
    (match colored-graph
      ;; graph is a variable id to spill, so spill it and reanalyze
      [symbol?     (analyze-func-ast (spill-function colored-graph spill-prefix func-ast gens kills))]
      ;; successfully colored graph
      [hash?       (process-func-coloring func-ast colored-graph)]
      ;; impossible to allocate function
      [boolean?    (lambda () (error "Impossible to allocate function"))]))

  ;; Iterate through intructions and replace variable uses with their allocated register
  ;; func-AST colored-graph -> func-AST
  (define (process-func-coloring func-ast colored-graph)
    (let ([var-replace (lambda (child-ast)
                         (if (is-var-node? child-ast)
                             (set-AST-data child-ast (hash-ref colored-graph (get-first-data child-ast)))
                             child-ast))])
      (set-AST-children func-ast
       ;; iterates through instruction AST's and replaces the ID's
       (for/list ([i-ast (AST-children func-ast)])
                 (set-AST-children i-ast (map var-replace (AST-children i-ast))))))))
   
  
  