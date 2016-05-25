#lang racket

(require "lib.rkt" "liveness.rkt" "AST.rkt")
(provide allocate-function generate-uncolored-graph color-graph-function convert-to-adjacency variables-only-coloring)

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
;; - Colored graph: A hash that maps from IDs to register IDs


;; These register names also serve as the color names
(define all-registers (set 'r8 'r9 'r10 'r11 'r12 'r13 'r14 'r15 'rbp 'rbx 'rdi 'rsi 'rdx 'rcx 'rax))


;; func-ast insets outsets killsets -> colored-graph/id
(define (allocate-function func-ast ins outs kills)
  (color-graph-function (generate-uncolored-graph ins outs kills func-ast)))

;; interference-graph -> colored-graph/id to spill
(define (color-graph-function interference-graph)
  ;; gets a list of "colors from neighbors
  ;; colored-graph id -> id
  (define (get-neighbors-colors colored-graph id)
    (let ([neighbor-set (hash-ref interference-graph id)])
      (set-remove  (for/set ([neighbor neighbor-set])
                            (hash-ref colored-graph neighbor))
                   #f)))
  
  ;; chooses a register as color
  ;; colored-graph id -> id
  (define (choose-color colored-graph id)
    (let ([choices (set-subtract all-registers (get-neighbors-colors colored-graph id))])
      ;(printf "choices:\n~a\n" choices)
      (if (set-empty? choices)
          #f
          (set-first choices))))
  
  ;; for each node, if it's a register, color it as itself. Else, put false as uncolored.
  (define (get-initial-colored-graph to-allocate)
    (make-immutable-hash (map  (lambda (key)
                                 (cons key
                                       (if (set-member? all-registers key)
                                           key
                                           #f)))
                               to-allocate)))
  
  ;; interference-graph -> list
  (define (generate-complete-evaluation-order an-interference-graph)
    (sort (hash-map an-interference-graph (lambda (key value) (list key (set-count value))) #f)
          >
          #:key second))
  
  (define (generate-evaluation-order an-interference-graph)
    (filter-not (lambda (a-pair) (set-member? all-registers (car a-pair)))
                (generate-complete-evaluation-order an-interference-graph)))
  
  (define (generate-register-order an-interference-graph)
    (map first (sort (filter (lambda (a-pair) (set-member? all-registers (car a-pair)))
                             (generate-complete-evaluation-order an-interference-graph))
                     >
                     #:key second)))
  
  ;; gets a list of variables ordered by number of edges
  (let* ([evaluation-order  (generate-evaluation-order interference-graph)]
         [register-order    (generate-register-order   interference-graph)]
         [to-allocate       (hash-keys interference-graph)]
         [colored-graph     (get-initial-colored-graph to-allocate)]
         [num-registers     (set-count all-registers)])
    ;(printf  "evaluation order:\n~a\n" evaluation-order)
    ; (printf  "register order:\n~a\n" register-order)
    ; (printf "initial-graph:\n~a\n\n" colored-graph)
    (for ([id-edges evaluation-order]
          ;; breaks when we find something to spill or when deemed impossible
          #:break (not (hash? colored-graph)))
         (let* ([id          (first  id-edges)]
                [num-edges   (second id-edges)]
                [color       (hash-ref colored-graph id)])
           (cond
             ;; set it to the id to spill it
             [(> num-edges num-registers) (set! colored-graph id)]
             ;; when a variable does not have a color assigned, assign a register that is not in its interference set
             [else (when (not (symbol? color))
                     (let ([chosen-color (choose-color colored-graph id)])
                       (if (symbol? chosen-color)
                           (set! colored-graph (hash-set colored-graph id chosen-color))
                           ;; couldn't find a color
                           (set! colored-graph id))))])))
    colored-graph))



;; graph symbol set -> graph
(define (add-interference graph source targets)
  (let ([new-graph graph])
    ;(println "----------Start add-interference---------")
    ;(printf "source: ~a  targets: ~a\n" source targets)
    ;; adds targets to sources conflicts
    (set! new-graph (hash-set new-graph source (set-union targets (hash-ref new-graph source (set)))))
    ;(println "Added targets to source conflict set")
    ;(print-hash new-graph)
    ;(println "past source to targets")
    ;; adds source to each of targets conflicts
    (for ([target targets])
         (set! new-graph (hash-set new-graph target (set-add (hash-ref new-graph target (set)) source))))
    ;(println "----------End add-interference---------")
    new-graph))

;; Takes each element in conflicters and adds all the other conflicters to their conflict set
(define (add-interferences-within graph conflicters)
  ;(println "in add-interferences-within")
  (let ([new-graph graph])
    ;; for each conflicter, union it's current conflict set with the passed in conflicters - itself
    (for ([c conflicters])
         (set! new-graph (add-interference new-graph c (set-remove conflicters c))))
    new-graph))

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
(define (generate-uncolored-graph ins outs kills f-ast)
  ;; As stated above: a graph is a hashtable of variables/registers identifiers that map to its conflict set
  (define (get-initial-graph)
    (make-immutable-hash (set-map all-registers
                                  (lambda (curr-register) `(,curr-register . ,(set-remove all-registers curr-register))))))
  
  ;; iterate through instructions to generate graph
  (let ([graph (get-initial-graph)]
        [instructions (AST-children f-ast)])
    ;(println "Initial graph:")
    ;(print-hash graph)
    (for ([i (range (length instructions))])
         (let ([in     (list-ref ins i)]
               [out    (list-ref outs i)]
               [kill   (list-ref kills i)]
               [i-ast  (list-ref instructions i)])
           ;(println "***************************")
           ;(printf "in: ~a\nout: ~a\nkill: ~a\ni-ast ~a: ~a\n\n" in out kill i i-ast)
           ;(print-hash graph)
           (match (AST-type i-ast)
             ['mems2w    ;(cond
                           ;; handles that two variables don't interfere with each other
                         ;  [(and (is-var-node? (get-first-child i-ast)) (is-var-node? (get-second-child i-ast)))
              ;(println "Handling mems2w")
                            (let*   ([w    (get-first-data (get-first-child i-ast))]
                                     [s    (get-first-data (get-second-child i-ast))])
                              ;(printf "w: ~a  in-without-w: ~a\n" w (set-remove in w))
                              ;(printf "w: ~a  out-without-w: ~a\n" w (set-remove out w))
                              ;(printf "s: ~a  in-without-s: ~a\n" s (set-remove in s))
                              ;(printf "s: ~a  out-without-s: ~a\n" s (set-remove out s))
                              (when (= i 0)
                                (set! graph (add-interferences-within graph (set-remove in s)))
                                (set! graph (add-interferences-within graph (set-remove in w))))
                              ;; everything in out set conflicts with each other
                              (set! graph (add-interferences-within graph (set-remove out s)))
                              (set! graph (add-interferences-within graph (set-remove out w)))
                              (for ([a-kill kill])
                                   (set! graph (add-interference graph a-kill (set-remove out s)))))
                            ]
             ;; handles that only rcx can be used
             ['sopsx    (let* ([var-shifter?   (is-var-node? (ast-child i-ast 2))]
                               [c-data         (get-token-children-data i-ast)]
                               [shifter        (third c-data)]
                               [w              (first  c-data)])
                          ;(printf "var?: ~a w: ~a  shifter: ~a\n" var-shifter? w shifter)
                          (when var-shifter?
                            (set! graph (add-interference graph shifter (set-remove all-registers 'rcx)))))
                          (when (= i 0)  (set! graph (add-interferences-within graph in)))
                          ;; everything in out set conflicts with each other
                          (set! graph (add-interferences-within graph out))
                          (set! graph (add-interferences-within graph out))
                          (for ([a-kill kill])
                               (set! graph (add-interference graph a-kill (set-remove out 'rcx))))]
             [else    (when (= i 0) (set! graph (add-interferences-within graph in)))
                      ;; everything in out set conflicts with each other
                      (set! graph (add-interferences-within graph out))
                      ;; everything in the kill set conflicts with the out set
                      (for ([a-kill kill])
                           (set! graph (add-interference graph a-kill out)))])))
    (for/hash ([i    (range (hash-count graph))])
              (let*  ([a-key  (hash-iterate-key graph i)]
                      [a-set  (hash-iterate-value graph i)])
                (values a-key (set-remove a-set a-key))))))




(define (convert-to-adjacency uncolored-graph)
  (let      ([sorted-pairs (sort (hash->list uncolored-graph) symbol<? #:key car)])
    (for/list ([a-pair sorted-pairs])
              (let ([i-set (sort (set->list (cdr a-pair))
                                 symbol<?)]
                    [source (car a-pair)])
                (cons source i-set)))))

(define (variables-only-coloring a-coloring)
  (if (hash? a-coloring)
      (map flatten (sort (filter-not (lambda (a-pair) (set-member? all-registers (car a-pair)))
                        (hash->list a-coloring))
            symbol<?
            #:key car))
      ;; false if not a coloring
      #f))