#lang racket

(require "AST.rkt" "parser.rkt")
(provide liveness-analysis generate-in-out generate-gens-kills generate-gen-kill generate-successors search-for-label-instr generate-liveness-info)


;;;;;;;;;;;;;;;;;;; DISPLAY-HELPERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (print-set a-set)
  (for ([a-subset a-set])
       (println a-subset))
  (printf "\n\n"))

(define (print-sets a-sets b-sets)
  (for ([i (range (length a-sets))])
       (printf "~a) in: ~a  out: ~a\n" (+ i 1) (list-ref a-sets i)(list-ref b-sets i)))
  (printf "\n\n\n"))

;;;;;;;;;;;;;;;;;;; LIVENESS-ANALYSIS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (generate-liveness-info function-ast)
  (let* ([gen-kills    (map generate-gen-kill (AST-children function-ast))]
         [gens         (map (lambda (gen-kill) (first gen-kill)) gen-kills)]
         [kills        (map (lambda (gen-kill) (second gen-kill)) gen-kills)]
         [successors   (generate-successors function-ast)]
         [ins-outs     (generate-in-out gens kills successors)]
         [ins          (first ins-outs)]
         [outs         (second ins-outs)])
    (make-hash `((gens . ,gens) (kills . ,kills) (successors . ,successors) (ins . ,ins) (outs . ,outs)))))



;; string -> listof(gensets killsets successorslists insets outsets)
(define (liveness-analysis function-ast)
  (let* ([gen-kills    (map generate-gen-kill (AST-children function-ast))]
         [gens         (map (lambda (gen-kill) (first gen-kill)) gen-kills)]
         [kills        (map (lambda (gen-kill) (second gen-kill)) gen-kills)]
         [successors   (generate-successors function-ast)]
         [ins-outs     (generate-in-out gens kills successors)]
         [ins          (first ins-outs)]
         [outs         (second ins-outs)])
   `(      (in ,@(map (lambda (setInput) (sort (set->list setInput) symbol<?)) ins) )
           (out ,@(map (lambda (setInput) (sort (set->list setInput) symbol<?)) outs)))))


;;;;;;;;;;;;;;;;;;; IN OUT SETS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; listof instructions -> listof (insets outsets)
(define (generate-in-out gens kills successors)
  ;; helpers to build the sets
  (define (make-in gen out kill)
    (set-union gen (set-subtract out kill)))
  (define (make-out successor-ins)
    (apply set-union successor-ins))

  ;;keeps track of
  (let* ([num-insts    (length gens)]
         [empty-setter (lambda (x) (set))]
         [ins  (build-list num-insts empty-setter)]
         [outs (build-list num-insts empty-setter)]
         [changed #t])
    ;; loop until not changed
    (for  ([i (in-naturals)] ;; i in naturals basically means forever
           #:break (not changed))
          (let ([changed-this-round #f])
            ;; make ins by gen[i] union (out[i] - kill[i])
            (set! ins
                  (for/list ([inst-num (range num-insts)])
                            (let* ([in-length-before (set-count (list-ref ins inst-num))]
                                  [new-in  (make-in  (list-ref gens inst-num)
                                                     (list-ref outs inst-num)
                                                     (list-ref kills inst-num))])
                              (set! changed-this-round (not (= in-length-before (set-count new-in))))
                              new-in)))
            ;; make out set by Union in[m] where m is a successor of instruction[i]
            (set! outs
                  (for/list ([inst-num (range num-insts )])
                            (let* ([out-length-before (set-count (list-ref outs inst-num))]
                                   [successor-set (list-ref successors inst-num)]
                                   [new-out (if (list? successor-set) ;; checks to see if it's the last instruction
                                                (make-out (map (lambda (pos) (list-ref ins pos))
                                                               successor-set))
                                                (set))])
                                   (set! changed-this-round (or changed-this-round (not (= out-length-before (set-count new-out)))))
                                   new-out)))
            (set! changed changed-this-round))
          )
    (list ins outs)))


;;;;;;;;;;;;;;;;;;; SUCCESSORS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Creates a list for each instruction that lists the 
;; AST -> (list (list number?)|#f )
(define (generate-successors function-ast)
  (let ([num-instructions (num-children function-ast)])
    
    (for/list  ([inst-num (range num-instructions)])
             (let ([instr (ast-child function-ast inst-num)])
               (match (AST-type instr)
                 ['goto         (let  ([found (search-for-label-instr function-ast (get-first-data (get-first-child instr)))])
                                  (if (number? found)
                                      (list found)
                                      (lambda () (error (format "goto instruction with invalid label ~a" (get-first-data instr))))))]
                 ['cjump        (let  ([found1 (search-for-label-instr function-ast (get-first-data (ast-child instr  3)))]
                                       [found2 (search-for-label-instr function-ast (get-first-data (ast-child instr  4)))])
                                  ;(printf "~a ~a" found1 found2)
                                  ;(printf "~a ~a" (get-first-data instr) (get-second-data instr))
                                  (if (or (not (number? found1)) (not (number? found2)))
                                      (lambda () (error (format "cjump instruction with invalid labels ~a ~a" (get-first-data instr) (get-second-data instr))))
                                      (list found1 found2)))]
                 [(or 'return  'tail-call)      #f]
                
                 [_              (if (< (+ inst-num 1)  num-instructions)
                                     (list (+ inst-num 1))
                                     #f)] ;; should return #f only for last instruction, which should be a returnor a goto
               (AST-children function-ast))))))


;; AST symbol -> number?|bool
(define (search-for-label-instr func-ast label-name)
  (let ([found -1]
        [instructions (AST-children func-ast)])
    (for ([inst   instructions]
          [index  (range (length instructions))]
          #:break (>= found 0))
         (when (is-label-node? inst)
           (when (symbol=? label-name (get-first-data inst))
             (set! found index))))
    (if (>= found 0) found #f)))



;;;;;;;;;;;;;;;;;;; GEN KILL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (generate-gens-kills function-ast)
  (let* ([gen-kills    (map generate-gen-kill (AST-children function-ast))]
         [gens         (map (lambda (gen-kill) (first gen-kill)) gen-kills)]
         [kills        (map (lambda (gen-kill) (second gen-kill)) gen-kills)])
    (list gens kills)))

;; AST(instruction) -> listof (setof symbol? (gens)  setof symbol? (kills))
(define (generate-gen-kill i-ast)
  (let* ([c-data               (get-token-children-data i-ast)]
         [extract-all-tracked  (lambda (an-ast) (map get-first-data (filter is-tracked? (AST-children an-ast))))])
      (match (AST-type i-ast)
        ;; if s is tracked, put in gen. Kill w
        ['mems2w    (list (if (is-tracked? (ast-child i-ast 1))
                              (set (second c-data)) (set))
                          (set (first c-data)))]
        ;; kill w. gen x used in mem
        ['memmem2w  (let ([x  (get-first-child (get-second-child i-ast))])
                          (list (if (is-tracked? x)
                                     (set (get-first-data x))
                                     (set))
                          (set (first c-data))))]
        ;; if s is tracked, put in gen. gen mem x
        ['mems2mem       (let* ([x             (get-first-child (get-first-child i-ast))]
                                [x-track       (if (is-tracked? x)
                                                   (set (get-first-data (get-first-child (get-first-child i-ast))))
                                                   (set))]
                                [s             (ast-child i-ast 1)]
                                [s-track       (if (is-tracked? s)
                                                   (set (first c-data))
                                                   (set))])
                                                        
                           (list     (set-union x-track s-track)
                                     (set)))]
        ;; if t1, t2 tracked, put in gen. kill w.
        ['memcmp2w   (let  ([t1-tracked (is-tracked? (ast-child i-ast 1))]
                            [t2-tracked (is-tracked? (ast-child i-ast 3))]
                            [t1 (second c-data)]
                            [t2 (fourth c-data)])
                       
                            (list (cond ;; determines gen set
                                    [(and t1-tracked t2-tracked) (set t1 t2)]
                                    [t1-tracked (set t1)]
                                    [t2-tracked (set t2)]
                                    [else (set)])
                                  (set (first c-data))))]

        ;; w must be tracked. It is being killed
        ['memstack2w   (let  ([w-tracked? (is-tracked? (ast-child i-ast 0))]
                              [w          (first c-data)])

                         (list (set)
                               (set w)))]
                   
                              
                              
        ;; gen w (left). if t is tracked, gen. Kill w
        ['aop        (let  ([t2-tracked (is-tracked? (ast-child i-ast 2))]
                            [t1 (first c-data)]
                            [t2 (third c-data)])
                       
                       (list (cond ;; determines gen set
                                    [t2-tracked (set t1 t2)]
                                    [else (set t1)])
                                  (set (first c-data))))]
        ;; gen w, rcx. kill w. 
        ['sopsx     (list (set (first c-data) (third c-data))
                          (set (first c-data)))]
        ;; gen w. kill w.
        ['sopn      (list (set (first c-data))
                          (set (first c-data)))]
        ;; all empty
        [(or 'label  'goto)   (list (set) (set))]
        ;; if either t is tracked, gen. kill nothing
        ['cjump       (let ([t1-tracked (is-tracked? (ast-child i-ast 0))]
                            [t2-tracked (is-tracked? (ast-child i-ast 2))]
                            [t1 (first c-data)]
                            [t2 (third c-data)])
                       
                            (list (cond
                                    [(and t1-tracked t2-tracked) (set t1 t2)]
                                    [t1-tracked (set t1)]
                                    [t2-tracked (set t2)]
                                    [else (set)])
                                  (set)))]
        ;; do regular call. dummy u
        ['print                           (func-call-gen-kill #f #f (AST 'num '(0) '()) 1)]
        [(or  'allocate 'array-error)     (func-call-gen-kill #f #f (AST 'num '(0) '()) 2)]
        ;; do regular call.
        ['call        (func-call-gen-kill #f #f (ast-child i-ast 0) (last c-data))]
        ;; do tail-call
        ['tail-call   (func-call-gen-kill #t #f (ast-child i-ast 0) (last c-data))]
        ;; do return
        ['return      (func-call-gen-kill #f #t (AST 'num '(0) '()) 0)]
        [_            (error "generate-gen-kill: could not understand ~a" i-ast)])))


;; Handles the call instructions 
;; bool bool AST -> pair? (gen . kill)
(define (func-call-gen-kill tail? return? u num-args)
  (let ([u-data (first (AST-data u))]
        [tracked (is-tracked? u)]
        [used-args     (if (>= num-args 6)
                           args
                           (take args num-args))])
  (cond
    [(and tail? return?) (error "Improper call of func-call-gen-kill")]
    [tail?               (list (if tracked
                                   (list->set (append used-args callee-save (list u-data)))
                                   (list->set (append used-args callee-save)))
                               (set))]
    [return?             (list (list->set (append callee-save result))
                               (set))]
    [else                (list (if tracked
                                   (list->set (append used-args  (list u-data)))
                                   (list->set (append used-args)))
                               (list->set (append caller-save result)))])))

;; Returns true of it's a register or a variable node
;; AST -> bool
(define (is-tracked? an-ast)
  (or (and (is-reg-node? an-ast)
                ((negate eq?) (get-first-data an-ast) 'rsp))
                (is-var-node?  an-ast)))
