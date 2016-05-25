#lang racket

(require "liveness.rkt" "parser.rkt" "AST.rkt")
;;;;;;;;;;;;;;;;;;; TESTING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define print-debug #t)

(define (debug-printer proc . args)
  (when print-debug (printf "given:    ~a\nexpected: ~a\n\n" (first args) (last args)))
  (apply proc args))

  



;; test in out sets
(module+ test
  (require rackunit)
  (define (test-in-outs function-ast expected)
    (let* ([gen-kills      (map generate-gen-kill (AST-children function-ast))]
          [gens            (map (lambda (gen-kill) (first gen-kill)) gen-kills)]
          [kills           (map (lambda (gen-kill) (second gen-kill)) gen-kills)]
          [successors      (generate-successors function-ast)]
          [in-outs         (generate-in-out gens kills successors)]
          [ins             (first in-outs)]
          [outs            (second in-outs)]
          [expected-ins    (map list->set (first expected))]
          [expected-outs   (map list->set (second expected))])
      (debug-print "Expected: ~a\n" (first expected))
      (for ([i (range (length gens))])
           (debug-print "Instruction ~a:\n" (+ i 1))
           (debug-print "Ins:\n")
           (debug-printer check-equal? (list-ref ins i) (list-ref expected-ins i))
           (debug-print "Outs:\n")
           (debug-printer check-equal? (list-ref outs i) (list-ref expected-outs i)))))
                                                

  (debug-print "****************** TEST FUNC ******************\n")
  (test-in-outs TEST-FUNC `(((r12 r13 r14 r15 rbp rbx rdi)
                             (r12 r13 r14 r15 rbp rbx rdi x2)
                             (r12 r13 r14 r15 rbp rbx rdi x2)
                             (dx2 r12 r13 r14 r15 rbp rbx rdi)
                             (dx2 r12 r13 r14 r15 rbp rbx rdi)
                             (dx2 r12 r13 r14 r15 rbp rbx tx)
                             (dx2 r12 r13 r14 r15 rbp rbx tx)
                             (dx2 r12 r13 r14 r15 rbp rbx tx)
                             (r12 r13 r14 r15 rax rbp rbx tx)
                             (r12 r13 r14 r15 rax rbp rbx)
                             (r12 r13 r14 r15 rax rbp rbx)
                             (r12 r13 r14 r15 rax rbp rbx))
                            ;; out set
                            ((r12 r13 r14 r15 rbp rbx rdi x2)
                             (r12 r13 r14 r15 rbp rbx rdi x2)
                             (dx2 r12 r13 r14 r15 rbp rbx rdi)
                             (dx2 r12 r13 r14 r15 rbp rbx rdi)
                             (dx2 r12 r13 r14 r15 rbp rbx tx)
                             (dx2 r12 r13 r14 r15 rbp rbx tx)
                             (dx2 r12 r13 r14 r15 rbp rbx tx)
                             (r12 r13 r14 r15 rax rbp rbx tx)
                             (r12 r13 r14 r15 rax rbp rbx)
                             (r12 r13 r14 r15 rax rbp rbx)
                             (r12 r13 r14 r15 rax rbp rbx)
                             ())))


  (debug-print "****************** TEST FUNC EXAMPLE LOOP ******************\n")
  (test-in-outs TEST-FUNC-EXAMPLE-LOOP `(((r12 r13 r14 r15 rbp rbx rdi)
                                          (r12 r13 r14 r15 rbp rbx rdi u)
                                          (d r12 r13 r14 r15 rbp rbx u)
                                          (d r12 r13 r14 r15 rbp rbx u)
                                          (r12 r13 r14 r15 rbp rbx u)
                                          (r12 r13 r14 r15 rbp rbx u)
                                          (r12 r13 r14 r15 rax rbp rbx)
                                          (d r12 r13 r14 r15 rbp rbx u)
                                          (d r12 r13 r14 r15 rbp rbx u)
                                          (d r12 r13 r14 r15 rbp rbx u)
                                          (d r12 r13 r14 r15 rbp rbx u))
                                         
                                         ((r12 r13 r14 r15 rbp rbx rdi u)
                                          (d r12 r13 r14 r15 rbp rbx u)
                                          (d r12 r13 r14 r15 rbp rbx u)
                                          (d r12 r13 r14 r15 rbp rbx u)
                                          (r12 r13 r14 r15 rbp rbx u)
                                          (r12 r13 r14 r15 rax rbp rbx)
                                          ()
                                          (d r12 r13 r14 r15 rbp rbx u)
                                          (d r12 r13 r14 r15 rbp rbx u)
                                          (d r12 r13 r14 r15 rbp rbx u)
                                          (d r12 r13 r14 r15 rbp rbx u))))
  (test-in-outs TEST-FUNC-INF-LOOP   `((()()(x)())
                                       (()(x)()())))

  (test-in-outs (parse-function `(:go
                  0 0
                  (rdi <- 5)
                  (call print 1)
                  (return)) )               `(((r12 r13 r14 r15 rbp rbx) 
                                               (r12 r13 r14 r15 rbp rbx rdi) 
                                               (r12 r13 r14 r15 rax rbp rbx)) 
                                              
                                              ((r12 r13 r14 r15 rbp rbx rdi) 
                                               (r12 r13 r14 r15 rax rbp rbx) 
                                               ())))


  (debug-print "****************** 9/10f ******************\n")

  (test-in-outs (parse-function `(:f
                                  2 0
                                  (a <- (mem rdi 0))
                                  (a += 1)
                                  (y <- rdi)
                                  (m <- rsi)
                                  (rdi <- a)
                                  (rsi <- (mem y 8))
                                  (call allocate 2)
                                  (a <- rax)
                                  (y <- (mem a 0))
                                  (a += y)
                                  (y <- (mem rsp 0))
                                  ((mem a 0) <- y)
                                  (return)))               `(((r12 r13 r14 r15 rbp rbx rdi rsi) 
                                                            (a r12 r13 r14 r15 rbp rbx rdi rsi) 
                                                            (a r12 r13 r14 r15 rbp rbx rdi rsi) 
                                                            (a r12 r13 r14 r15 rbp rbx rsi y) 
                                                            (a r12 r13 r14 r15 rbp rbx y) 
                                                            (r12 r13 r14 r15 rbp rbx rdi y) 
                                                            (r12 r13 r14 r15 rbp rbx rdi rsi) 
                                                            (r12 r13 r14 r15 rax rbp rbx) 
                                                            (a r12 r13 r14 r15 rax rbp rbx) 
                                                            (a r12 r13 r14 r15 rax rbp rbx y) 
                                                            (a r12 r13 r14 r15 rax rbp rbx) 
                                                            (a r12 r13 r14 r15 rax rbp rbx y) 
                                                            (r12 r13 r14 r15 rax rbp rbx))

                                                             ((a r12 r13 r14 r15 rbp rbx rdi rsi)
                                                             (a r12 r13 r14 r15 rbp rbx rdi rsi)
                                                             (a r12 r13 r14 r15 rbp rbx rsi y)
                                                             (a r12 r13 r14 r15 rbp rbx y)
                                                             (r12 r13 r14 r15 rbp rbx rdi y)
                                                             (r12 r13 r14 r15 rbp rbx rdi rsi)
                                                             (r12 r13 r14 r15 rax rbp rbx)
                                                             (a r12 r13 r14 r15 rax rbp rbx)
                                                             (a r12 r13 r14 r15 rax rbp rbx y)
                                                             (a r12 r13 r14 r15 rax rbp rbx)
                                                             (a r12 r13 r14 r15 rax rbp rbx y)
                                                             (r12 r13 r14 r15 rax rbp rbx) ())))


  (test-in-outs (parse-function `(:g
                                  1 1
                                  (cjump rdi = 1 :true :false)
                                  :true
                                  (rax <- 5)
                                  (return)
                                  :false
                                  (x <- rdi)
                                  (rdi -= 1)
                                  ((mem rsp -8) <- :g_recur)
                                  (call :g 1)
                                  (rdi <- x)
                                  (rax += rdi)
                                  (return)))                                        `(((r12 r13 r14 r15 rbp rbx rdi)
                                                                                      (r12 r13 r14 r15 rbp rbx)
                                                                                      (r12 r13 r14 r15 rbp rbx)
                                                                                      (r12 r13 r14 r15 rax rbp rbx)
                                                                                      (r12 r13 r14 r15 rbp rbx rdi)
                                                                                      (r12 r13 r14 r15 rbp rbx rdi)
                                                                                      (r12 r13 r14 r15 rbp rbx rdi x)
                                                                                      (r12 r13 r14 r15 rbp rbx rdi x)
                                                                                      (r12 r13 r14 r15 rbp rbx rdi x)
                                                                                      (r12 r13 r14 r15 rax rbp rbx x)
                                                                                      (r12 r13 r14 r15 rax rbp rbx rdi)
                                                                                      (r12 r13 r14 r15 rax rbp rbx))
                                                                                     
                                                                                     ((r12 r13 r14 r15 rbp rbx rdi)
                                                                                      (r12 r13 r14 r15 rbp rbx)
                                                                                      (r12 r13 r14 r15 rax rbp rbx)
                                                                                      ()
                                                                                      (r12 r13 r14 r15 rbp rbx rdi)
                                                                                      (r12 r13 r14 r15 rbp rbx rdi x)
                                                                                      (r12 r13 r14 r15 rbp rbx rdi x)
                                                                                      (r12 r13 r14 r15 rbp rbx rdi x)
                                                                                      (r12 r13 r14 r15 rax rbp rbx x)
                                                                                      (r12 r13 r14 r15 rax rbp rbx rdi)
                                                                                      (r12 r13 r14 r15 rax rbp rbx)
                                                                                      ())))



  (debug-print "****************** 26/15f ******************\n")
  (test-in-outs  (parse-function `(:main
                                   0 0
                                   (s <- 0)
                                   (cjump 1 < 2 :t :f)
                                   :t
                                   (a <- :fun)
                                   (tail-call a 0)
                                   :f
                                   (rax <- s)
                                   (return)))                            `(((r12 r13 r14 r15 rbp rbx)
                                                                           (r12 r13 r14 r15 rbp rbx s)
                                                                           (r12 r13 r14 r15 rbp rbx)
                                                                           (r12 r13 r14 r15 rbp rbx)
                                                                           (a r12 r13 r14 r15 rbp rbx)
                                                                           (r12 r13 r14 r15 rbp rbx s)
                                                                           (r12 r13 r14 r15 rbp rbx s)
                                                                           (r12 r13 r14 r15 rax rbp rbx))

                                                                          ((r12 r13 r14 r15 rbp rbx s)
                                                                          (r12 r13 r14 r15 rbp rbx s)
                                                                          (r12 r13 r14 r15 rbp rbx)
                                                                          (a r12 r13 r14 r15 rbp rbx)
                                                                          ()
                                                                          (r12 r13 r14 r15 rbp rbx s)
                                                                          (r12 r13 r14 r15 rax rbp rbx)
                                                                          ())))

  ;; test robby's 69f. Doesn't make sense since out = union of in sets of successors
  (test-in-outs (parse-function `(:f 3 0 
                                     (call array-error 2) 
                                     (call array-error 2)))       `(((rdi rsi) (rdi rsi)) (() ())))
                                
  (debug-print "Finished in out tests"))


#|
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
  (test-successors TEST-FUNC-EXAMPLE-LOOP (list '(1) '(2) '(3) '(4 7) '(5) '(6) #f '(8) '(9) '(10) '(2)))
  (test-successors TEST-FUNC-INF-LOOP (list '(1) '(2) '(3) '(0)))

  (debug-print "Finished search and successors test\n\n"))



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
  (test-gen-kill `(x2 <- (mem rsp 0)) (list (set 'rsp)      (set 'x2)))
  (test-gen-kill `(rdi <- (mem rsp 64)) (list (set 'rsp)    (set 'rdi)))

  ;;mems2mem
  (test-gen-kill `((mem rsp 24) <- rdi) (list (set 'rsp 'rdi)     (set)))
  (test-gen-kill `((mem rsp -8) <- x2)  (list (set 'rsp 'x2)       (set)))
  
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
  
  (debug-print "Finished gen-kill tests\n\n"))

|#

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


(define TEST-FUNC-EXAMPLE-LOOP (parse-function `(:go 0 0
                                                     (u <- 0) 
                                                     (d <- rdi) 
                                                     :top 
                                                     (cjump d = 0 :z :nz)
                                                     :z 
                                                     (rax <- u) 
                                                     (return) 
                                                     :nz 
                                                     (u += 1) 
                                                     (d -= 1) 
                                                     (goto :top))))

(define TEST-FUNC-INF-LOOP (parse-function `(:go 0 0
                                                 :top
                                                 (x <- 1)
                                                 (x += 1)
                                                 (goto :top))))

