#lang racket

(require "lib.rkt" "liveness.rkt" "AST.rkt")

;; Overview:
;; The process is that we generate a graph first, then color it by trying to assign each variable a register
;;
;; Vocab:
;; - Conflict set: all the variables/registers a variable interferes with
;; - Conflicters:  A set of variables/registers that conflict with each other
;; - Graph: A hash table of identifiers to a conflict set
;; - id: refers to a variable or register identifier
;; - Colors: The colors are simply the registers
;; - Initial graph: The initial graph is empty except for registers, all of which interfere with each other
;; - interference graph: A graph that goes through a program and builds the conflict sets for all registers/variables
;; - Colored graph: A graph that maps from IDs to register IDs


;; These register names also serve as the color names
(define all-registers (set 'r8 'r9 'r10 'r11 'r12 'r13 'r14 'r15 'rbp 'rbx 'rdi 'rsi 'rdx 'rcx 'rax))


;; interference-graph -> colored-graph/id to spill
(define (color-graph-function interference-graph)
  ;; gets a list of "colors from neighbors
  ;; colored-graph id -> id
  (define (get-neighbors-colors colored-graph id)
    (filter (negate boolean?)
            (for ([neighbor (hash-ref interference-graph id)])
                 (hash-ref colored-graph neighbor))))

  ;; chooses a register as color
  ;; colored-graph id -> id
  (define (choose-color colored-graph id)
    (let ([choices (set-subtract all-registers (get-neighbors-colors colored-graph id))])
      (set-first choices)))

  ;; for each node, if it's a register, color it as itself. Else, put false as uncolored.
  (define (get-initial-colored-graph)
    (make-hash (map (lambda (key) (list key (if (set-contains? all-registers key) key #f))))))
  
  (define (generate-evaluation-order)
    (sort (hash-map (lambda (key value) (list key value)) interference-map)
          #:key second number<?))
  
  ;; gets a list of variables ordered by number of edges
  (let ([evaluation-order (generate-evaluation-order)]
        [colored-graph  (get-inital-colored-graph)])
    (for ([id-edges evaluation-order]
          #:break (not (hash? colored-graph)))
         (let ([id         (first id-edges)]
               [num-edges (second id-edges)])
           (cond
             [(> num-edges (length all-registers)) (set! colored-graph id)] ;; set it to the id to spill it
             ;; when a variable does not have a color assigned, assign a register that is not in its interference set
             [else (when (not (symbol? (hash-ref colored-graph variable)))
                     (let ([chosen-color (choose-color colored-graph id)])
                       (if (symbol? chosen-color)
                           (set! colored-graph (hash-set colored-graph id chosen-color))
                           (set! colored-graph id))))])
    ))))



#| Build interference graph from the liveness information
- For each instruction:
--- Two variables live at the same time interfere with each other
--- Killed variables interfere with variables in the out set
--- Except that the variables x and y do not interfere if the instruction was (x <- y)
- All real registers interfere with each other
- Handle constrained arithmetic via extra edges

Live at the same time means appearing together in an ‘out’ set, or appearing together
in the first instruction’s ‘in’ set |#

;; listof(in-sets) listof(out-sets) listof(kill-sets) AST -> inteference Graph
(define (generate-graph-function ins outs kills f-ast)
  ;; As stated above: a graph is a hashtable of variables/registers identifiers that map to its conflict set
  (define (get-initial-graph)
    (make-hash (set-map all-registers
                        (lambda (curr-register) (list curr-register (set-remove all-registers curr-register)))))) 
  ;; Takes each element in conflicters and adds all the other conflicters to their conflict set
  (define (add-interference graph conflicters)
    (let ([new-graph graph])
      ;; for each conflicter, union it's current conflict set with the passed in conflicters - itself
      (for ([c conflicters])
           (set! new-graph (hash-set new-graph c (set-union (set-remove conflicters c)
                                                            ;; if one of the conflicters doesn't not exist yet
                                                            ;; add an empty set
                                                            (if (set-member? new-graph c) (hash-ref graph c) (set))))))
      new-graph))
  
  ;; iterate through isntructions to generate graph
  (let ([graph (get-initial-graph)]
        [instructions (AST-children f-ast)])
    (for ([i (range (length instructions))])
         (let ([in  (list-ref ins i)]
               [out (list-ref outs i)]
               [kill (list-ref kills i)]
               [i-ast (list-ref instructions i)])
           (match (AST-type i-ast)
             ['mems2w empty] ;; TODO: handle the x <- y case
             ;; handles that only rcx can be used
             ['sopsx (let ([shifter (second (get-token-children-data i-ast))])
                       (when (not (symbol=? shifter 'rcx))
                         (add-interference graph (set))))]
           ;; Handle live at the same time
           (if (= i 0)
               (set! graph (add-interference graph in))
               (set! graph (add-interference graph out)))
           ;; Handle the interference between kill and out
           (add-interference (set-union kill out)))))
    graph))