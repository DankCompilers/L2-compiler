#lang racket

(require "AST.rkt" "parser.rkt")

(define print-debug #f)
(provide generate-graph generate-in-out generate-gen-kill)

(define (debug-printer proc . args)
  (when print-debug (printf "given:    ~a\nexpected: ~a\n\n" (first args) (last args)))
  (apply proc args))

;; string -> listof (insets outsets)
(define (liveness raw-function)
  (let* ([function-ast (parse-function `raw-function)]
         [gen-kills    (map generate-gen-kill (AST-children function-ast))]
         [gens         (map (lambda (gen-kill) (first gen-kill)) gen-kills)]
         [kills        (map (lambda (gen-kill) (second gen-kill)) gen-kills)]
         [successors   (generate-successors function-ast)]
         [in-outs      (generate-in-out gens kills successors)])
    empty))




;; (listof sets) (listof sets) -> graph
(define (generate-graph ins outs)
  empty)

(define (print-set a-set)
  (for ([a-subset a-set])
       (println a-subset))
  (printf "\n\n"))

(define (print-sets a-sets b-sets)
  (for ([i (range (length a-sets))])
       (printf "in: ~a  out: ~a\n" (list-ref a-sets i)(list-ref b-sets i)))
  (printf "\n\n\n"))


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
            (set! ins
                  (for/list ([inst-num (range num-insts)])
                            (let* ([in-length-before (set-count (list-ref ins inst-num))]
                                  [new-in  (make-in  (list-ref gens inst-num)
                                                     (list-ref outs inst-num)
                                                     (list-ref kills inst-num))]) 
                                  (set! changed-this-round (= in-length-before (set-count new-in)))
                                  new-in)))
            (set! outs
                  (for/list ([inst-num (range num-insts )])
                            (let* ([out-length-before (set-count (list-ref outs inst-num))]
                                   [successor-set (list-ref successors inst-num)]
                                   [new-out (if (list? successor-set) ;; checks to see if it's the last instruction
                                                (make-out (map (lambda (pos) (list-ref ins pos))
                                                               successor-set))
                                                (set))])
                                   (set! changed-this-round (or changed-this-round (= out-length-before (set-count new-out))))
                                   new-out)))
            (set! changed changed-this-round))
         ;(print-sets ins outs)
          )
    (list ins outs)))


;; test in out sets
(module+ test
  (require rackunit)
  (define (test-in-outs function-ast expected)
    (let* ([gen-kills    (map generate-gen-kill (AST-children function-ast))]
          [gens         (map (lambda (gen-kill) (first gen-kill)) gen-kills)]
          [kills        (map (lambda (gen-kill) (second gen-kill)) gen-kills)]
          [successors   (generate-successors function-ast)]
          [in-outs      (generate-in-out gens kills successors)])
      (debug-printer check-equal? in-outs expected)))

  ;(test-in-outs TEST-FUNC `(((r12 r13 r14 r15 rbp rbx rdi)   (r12 r13 r14 r15 rbp rbx rdi x2)(r12 r13 r14 r15 rbp rbx rdi x2) (dx2 r12 r13 r14 r15 rbp rbx rdi) (dx2 r12 r13 r14 r15 rbp rbx rdi)
  ;(dx2 r12 r13 r14 r15 rbp rbx tx) (dx2 r12 r13 r14 r15 rbp rbx tx)  (r12 r13 r14 r15 rax rbp rbx tx) (r12 r13 r14 r15 rax rbp rbx) (r12 r13 r14 r15 rax rbp rbx))
  ;((r12 r13 r14 r15 rbp rbx rdi x2) (r12 r13 r14 r15 rbp rbx rdi x2) (dx2 r12 r13 r14 r15 rbp rbx rdi) (dx2 r12 r13 r14 r15 rbp rbx rdi) (dx2 r12 r13 r14 r15 rbp rbx tx)
  ;(dx2 r12 r13 r14 r15 rbp rbx tx) (r12 r13 r14 r15 rax rbp rbx tx) (r12 r13 r14 r15 rax rbp rbx) (r12 r13 r14 r15 rax rbp rbx) ())))
  )



;; Creates a list for each instruction that lists the 
;; AST -> (list (list number?)|#f )
(define (generate-successors function-ast)
  (let ([num-instructions (num-children function-ast)])
    
    (for/list  ([inst-num (range num-instructions)])
             (let ([instr (ast-child function-ast inst-num)])
               (match (AST-type instr)
                 ['goto         (let  ([found (search-for-label-instr function-ast (get-first-data instr))])
                                  (if (number? found)
                                      (list found)
                                      (lambda () (error (format "goto instruction with invalid label ~a" (get-first-data instr))))))]
                 ['cjump        (let  ([found1 (search-for-label-instr function-ast (get-first-data instr))]
                                       [found2 (search-for-label-instr function-ast (get-second-data instr))])
                                  ;(printf "~a ~a" found1 found2)
                                  ;(printf "~a ~a" (get-first-data instr) (get-second-data instr))
                                  (if (or (not (number? found1)) (not (number? found2)))
                                      (lambda () (error (format "cjump instruction with invalid labels ~a ~a" (get-first-data instr) (get-second-data instr))))
                                      (list found1 found2)))]
                 [_              (if (< (+ inst-num 1)  num-instructions)
                                     (list (+ inst-num 1))
                                     #f)] ;; should return #f only for last instruction
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


;; tests the search for label and successors functions
(module+ test
  (require rackunit)
  (define (test-search func-ast search-label expected)
    (debug-printer check-equal? (search-for-label-instr func-ast search-label) expected))
  (define (test-successors func-ast expected)
    (debug-printer check-equal? (generate-successors func-ast) expected))
  
  (test-search TEST-FUNC ':hello 5)
  (test-search TEST-FUNC ':again 9)
  (test-search TEST-FUNC ':bad   #f)
  (test-search TEST-FUNC-JUMPS ':hello 5)
  (test-search TEST-FUNC-JUMPS ':again 7)
  (test-search TEST-FUNC-JUMPS ':finish 13)

  (test-successors TEST-FUNC        (list '(1) '(2) '(3) '(4) '(5) '(6) '(7) '(8) '(9) '(10) '(11) #f))
  (test-successors TEST-FUNC-JUMPS  (list '(1) '(2) '(3) '(4) '(5) '(6) '(7 13) '(8) '(9) '(10) '(11) '(12) '(5) '(14) #f))
  (test-successors TEST-FUNC-SKIPS  (list '(1) '(2) '(3) '(4) '(5) '(6) '(10 13) '(8) '(9) '(10) '(11) '(12) '(5) '(14) #f))
   (test-successors TEST-FUNC-LOOP  (list '(1) '(2) '(3) '(4) '(5) '(6) '(7) '(8) '(5) '(10) '(11) '(12) '(10 13) '(14) #f))

  (printf "Finished search and successors test\n\n"))

  




;; AST(instruction) -> listof (setof symbol? (gens)  setof symbol? (kills))
(define (generate-gen-kill i-ast)
  (let* ([c-data               (get-token-children-data i-ast)]
         [extract-all-tracked  (lambda (an-ast) (map get-first-data (filter is-tracked? (AST-children an-ast))))])
      (match (AST-type i-ast)
        ;; if s is tracked, put in gen. Kill w
        ['mems2w    (list (if (is-tracked? (ast-child i-ast 1))
                              (set (second c-data)) (set))
                          (set (first c-data)))]
        ;; kill w
        ['memmem2w  (list (set)
                          (set (first c-data)))]
        ;; if s is tracked, put in gen
        ['mems2mem   (list (if (is-tracked? (ast-child i-ast 1))
                              (set (first c-data)) (set))
                           (set))]
        ;; if t1, t2 tracked, put in gen. kill w.
        ['memcmp2w   (let ([t1-tracked (is-tracked? (ast-child i-ast 2))]
                            [t2-tracked (is-tracked? (ast-child i-ast 3))]
                            [t1 (third c-data)]
                            [t2 (fourth c-data)])
                       
                            (list (cond
                                    [(and t1-tracked t2-tracked) (set t1 t2)]
                                    [t1-tracked (set t1)]
                                    [t2-tracked (set t2)]
                                    [else (set)])
                                  (set (first c-data))))]
        ;; gen w (left). if t is tracked, gen. Kill w
        ['aop       (list (if (is-tracked? (ast-child i-ast 1))
                              (set (first c-data) (second c-data))
                              (set (first c-data)))
                          (set (first c-data)))]
        ;; gen w, rcx. kill w. 
        ['sopsx     (list (set (first c-data) (second c-data))
                          (set (first c-data)))]
        ;; gen w. kill w.
        ['sopn      (list (set (first c-data))
                          (set (first c-data)))]
        ;; all empty
        [(or 'label  'goto)   (list (set) (set))]
        ;; if either t is tracked, gen. kill nothing
        ['cjump       (let ([t1-tracked (is-tracked? (ast-child i-ast 1))]
                            [t2-tracked (is-tracked? (ast-child i-ast 2))]
                            [t1 (second c-data)]
                            [t2 (third c-data)])
                       
                            (list (cond
                                    [(and t1-tracked t2-tracked) (set t1 t2)]
                                    [t1-tracked (set t1)]
                                    [t2-tracked (set t2)]
                                    [else (set)])
                                  (set)))]
        ;; do regular call. dummy u
        [(or 'print 'allocate 'array-error)     (func-call-gen-kill #f #f (AST 'num '(0) '())) ]
        ;; do regular call.
        ['call        (func-call-gen-kill #f #f (ast-child i-ast 0))]
        ;; do tail-call
        ['tail-call   (func-call-gen-kill #t #f (ast-child i-ast 0))]
        ;; do return
        ['return      (func-call-gen-kill #f #t (AST 'num '(0) '()))]
        [_            (lambda () (error "generate-gen-kill: could not understand ~a" i-ast))])))


;; bool bool AST -> pair? (gen . kill)
(define (func-call-gen-kill tail? return? u)
  (let ([u-data (first (AST-data u))]
        [tracked (is-tracked? u)])
  (cond
    [(and tail? return?) (error "Improper call of func-call-gen-kill")]
    [tail?               (list (if tracked
                                   (list->set (append args callee-save (list u-data)))
                                   (list->set (append args callee-save)))
                               (set))]
    [return?             (list (list->set (append callee-save result))
                               (set))]
    [else                (list (if tracked
                                   (list->set (append args  (list u-data)))
                                   (list->set (append args)))
                               (list->set (append caller-save result)))])))

;; Returns true of it's a register or a variable node
;; AST -> bool
(define (is-tracked? an-ast)
  (or (is-reg-node? an-ast) (is-var-node?  an-ast)))

;; test generate-gen-kill 
(module+ test
  (require rackunit)
  (define (test-gen-kill quoted-inst expected)
    (debug-printer check-equal? (generate-gen-kill (parse-instruction quoted-inst)) expected))

  ;; test mems

  ;;mems2w
  (test-gen-kill `(x2 <- rdi) (list (set 'rdi)      (set 'x2)))
  (test-gen-kill `(rdi <- x2) (list (set 'x2)       (set 'rdi)))
  (test-gen-kill `(tx <- rdi) (list (set 'rdi)      (set 'tx)))

  ;; memmem2w
  (test-gen-kill `(x2 <- (mem rsp 0)) (list (set)      (set 'x2)))
  (test-gen-kill `(rdi <- (mem rsp 64)) (list (set)    (set 'rdi)))

  ;;mems2mem
  (test-gen-kill `((mem rsp 24) <- rdi) (list (set 'rdi)     (set)))
  (test-gen-kill `((mem rsp -8) <- x2) (list (set 'x2)       (set)))
  
  ;;memcmp2w
  (test-gen-kill `(x2 <- x2 < x2) (list (set 'x2)         (set 'x2)))
  (test-gen-kill `(x2 <- rdi < 5) (list (set 'rdi)        (set 'x2)))
  (test-gen-kill `(x2 <- rdi < rax) (list (set 'rdi 'rax) (set 'x2)))
  (test-gen-kill `(x2 <- x1 < rsi) (list (set 'rsi 'x1)   (set 'x2)))
  (test-gen-kill `(rax <- 10 < x1) (list (set 'x1)        (set 'rax)))

  ;; test aops
  (test-gen-kill `(x2 *= x2)  (list (set 'x2)       (set 'x2)))
  (test-gen-kill `(dx2 *= 2)  (list (set 'dx2)      (set 'dx2)))
  (test-gen-kill `(tx *= rdi) (list (set 'rdi 'tx)  (set 'tx)))

  ;; test sops
  (test-gen-kill `(x2 <<= x2)   (list (set 'x2)        (set 'x2)))
  (test-gen-kill `(x2 >>= x1)   (list (set 'x2 'x1)    (set 'x2)))
  (test-gen-kill `(dx2 >>= 2)   (list (set 'dx2)       (set 'dx2)))
  (test-gen-kill `(rax <<= 10)  (list (set 'rax)       (set 'rax)))
  (test-gen-kill `(rax <<= rcx) (list (set 'rax 'rcx)  (set 'rax)))
  (test-gen-kill `(raxor >>= rcx) (list (set 'raxor 'rcx)  (set 'raxor)))

  ;; test easy ones
  (test-gen-kill `(goto :go) (list (set)   (set)))
  (test-gen-kill `(goto :anoter) (list (set)   (set)))
  (test-gen-kill `:go (list (set)   (set)))
  (test-gen-kill `:another (list (set)   (set)))

  ;; test cjump
  (test-gen-kill `(cjump x2 < x2 :hi :there)   (list (set 'x2)         (set)))
  (test-gen-kill `(cjump rdi < 5 :hi :friend)  (list (set 'rdi)        (set)))
  (test-gen-kill `(cjump rdi < rax :go :hello) (list (set 'rdi 'rax)   (set)))
  (test-gen-kill `(cjump x1 < rsi  :fib :crud) (list (set 'rsi 'x1)    (set)))
  (test-gen-kill `(cjump 10 < x1   :oh  :no)   (list (set 'x1)         (set)))

  ;;test system calls
  (test-gen-kill `(call print 1)         (list (set 'rdi 'rsi 'rdx 'rcx 'r8 'r9)
                                               (set'rax 'r8 'r9 'r10 'r11 'rcx 'rdi 'rdx 'rsi)))

  (test-gen-kill `(call allocate 2)      (list (set 'rdi 'rsi 'rdx 'rcx 'r8 'r9)
                                               (set'rax 'r8 'r9 'r10 'r11 'rcx 'rdi 'rdx 'rsi)))

  (test-gen-kill `(call array-error 2)   (list (set 'rdi 'rsi 'rdx 'rcx 'r8 'r9)
                                               (set'rax 'r8 'r9 'r10 'r11 'rcx 'rdi 'rdx 'rsi)))
  

  ;; test regular calls
  (test-gen-kill `(call r13 1)          (list (set 'rdi 'rsi 'rdx 'rcx 'r8 'r9 'r13)
                                              (set 'rax 'r8 'r9 'r10 'r11 'rcx 'rdi 'rdx 'rsi)))

  (test-gen-kill `(call dvar 1)          (list (set 'rdi 'rsi 'rdx 'rcx 'r8 'r9 'dvar)
                                              (set 'rax 'r8 'r9 'r10 'r11 'rcx 'rdi 'rdx 'rsi)))

  (test-gen-kill `(call :random 1)          (list (set 'rdi 'rsi 'rdx 'rcx 'r8 'r9)
                                              (set 'rax 'r8 'r9 'r10 'r11 'rcx 'rdi 'rdx 'rsi)))


  ;;test tail-call
  (test-gen-kill `(tail-call r13 1)          (list (set 'rdi 'rsi 'rdx 'rcx 'r8 'r9 'r13 'r12 'r13 'r14 'r15 'rbp 'rbx)
                                              (set)))

  (test-gen-kill `(tail-call dvar 1)         (list (set 'rdi 'rsi 'rdx 'rcx 'r8 'r9 'dvar 'r12 'r13 'r14 'r15 'rbp 'rbx)
                                               (set)))

  (test-gen-kill `(tail-call :random 1)      (list (set 'rdi 'rsi 'rdx 'rcx 'r8 'r9 'r12 'r13 'r14 'r15 'rbp 'rbx)
                                              (set)))


  ;;test return
  (test-gen-kill `(return)          (list (set 'rax 'r12 'r13 'r14 'r15 'rbp 'rbx)
                                          (set)))
  
  (printf "Finished gen-kill tests\n\n"))



(define TEST-FUNC  (parse-function `(:go 1 0
                                         (x2 <- rdi) 
                                         (x2 *= x2) 
                                         (dx2 <- x2) 
                                         (dx2 *= 2) 
                                         (tx <- rdi)
                                         :hello
                                         (tx *= 3) 
                                         (rax <- dx2) 
                                         (rax += tx)
                                         :again
                                         (rax += 4) 
                                         (return))))

(define TEST-FUNC-JUMPS  (parse-function `(:go 1 0
                                               (x2 <- rdi) 
                                               (x2 *= x2) 
                                               (dx2 <- x2) 
                                               (dx2 *= 2) 
                                               (tx <- rdi)
                                               :hello
                                               (cjump rax < 100 :again :finish)
                                               :again
                                               (tx *= 3) 
                                               (rax <- dx2)
                                               (rax += tx)
                                               (rax += 4)
                                               (goto :hello)
                                               :finish
                                               (return))))

(define TEST-FUNC-SKIPS  (parse-function `(:go 1 0
                                               (x2 <- rdi) 
                                               (x2 *= x2) 
                                               (dx2 <- x2) 
                                               (dx2 *= 2) 
                                               (tx <- rdi)
                                               :hello
                                               (cjump rax < 100 :again :finish)
                                               (tx *= 3) 
                                               (rax <- dx2)
                                               (rax += tx)
                                               :again
                                               (rax += 4)
                                               (goto :hello)
                                               :finish
                                               (return))))

(define TEST-FUNC-LOOP  (parse-function `(:go 1 0
                                               (x2 <- rdi) 
                                               (x2 *= x2) 
                                               (dx2 <- x2) 
                                               (dx2 *= 2) 
                                               (tx <- rdi)
                                               :hello
                                               (tx *= 3) 
                                               (rax <- dx2)
                                               (goto :hello)
                                               (rax += tx)
                                               :again
                                               (rax += 4)
                                               (cjump rax < 100 :again :finish)
                                               :finish
                                               (return))))