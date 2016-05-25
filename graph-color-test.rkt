#lang racket

(require "graph-color.rkt" "liveness.rkt" "parser.rkt" "AST.rkt" "lib.rkt")
;;;;;;;;;;;;;;;;;;; TESTING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define print-debug #t)

(define (debug-printer proc . args)
  (when print-debug (printf "given:    ~a\nexpected: ~a\n\n" (first args) (last args)))
  (apply proc args))





(module+ test
  (require rackunit)
  (define (test-generate-graph quoted-func expected)
    (let*   ([func-ast           (parse-function quoted-func)]
             [gen-kills          (generate-gens-kills func-ast)]
             [gens               (first gen-kills)]
             [kills              (second gen-kills)]
             [successors         (generate-successors func-ast)]
             [in-outs            (generate-in-out gens kills successors)]
             [ins                (first in-outs)]
             [outs               (second in-outs)]
             [uncolored-graph    (generate-uncolored-graph ins outs kills func-ast)]
             [adjacency-graph    (convert-to-adjacency uncolored-graph)]
             [colored-graph      (color-graph-function uncolored-graph)]
             [var-colored-graph  (variables-only-coloring colored-graph)]
             )

      ;(print-hash uncolored-graph)
      ;(println "")
      ;(println "")
      ;(print-adj adjacency-graph)

      ;(printf "colored graph:\n~a\n" colored-graph)
      (printf "variables graph:\n~a\n" var-colored-graph)
      (for ([i (range (length expected))])
           (debug-printer check-equal? (list-ref adjacency-graph i) (list-ref expected i)))
      ;(debug-printer check-equal? adjacency-graph expected)
      
      ))
  
  (println "Testing generating the graphs")

  
  #|(test-generate-graph `(:f
                         3
                         0
                         (rax <- s1)
                         (rax <- s0)
                         (rax <- w1)
                         (rax <- w0)
                         (w0 <- s0)
                         (s1 <- 0)
                         (s0 <- 1)
                         (w1 <- 2)
                         (w0 <- 3)) `((r10 r11 r12 r13 r14 r15 r8 r9 rax rbp rbx rcx rdi rdx rsi)
                                      (r11 r10 r12 r13 r14 r15 r8 r9 rax rbp rbx rcx rdi rdx rsi)
                                      (r12 r10 r11 r13 r14 r15 r8 r9 rax rbp rbx rcx rdi rdx rsi)
                                      (r13 r10 r11 r12 r14 r15 r8 r9 rax rbp rbx rcx rdi rdx rsi)
                                      (r14 r10 r11 r12 r13 r15 r8 r9 rax rbp rbx rcx rdi rdx rsi)
                                      (r15 r10 r11 r12 r13 r14 r8 r9 rax rbp rbx rcx rdi rdx rsi)
                                      (r8 r10 r11 r12 r13 r14 r15 r9 rax rbp rbx rcx rdi rdx rsi)
                                      (r9 r10 r11 r12 r13 r14 r15 r8 rax rbp rbx rcx rdi rdx rsi)
                                      (rax r10 r11 r12 r13 r14 r15 r8 r9 rbp rbx rcx rdi rdx rsi s0 w0 w1)
                                      (rbp r10 r11 r12 r13 r14 r15 r8 r9 rax rbx rcx rdi rdx rsi)
                                      (rbx r10 r11 r12 r13 r14 r15 r8 r9 rax rbp rcx rdi rdx rsi)
                                      (rcx r10 r11 r12 r13 r14 r15 r8 r9 rax rbp rbx rdi rdx rsi)
                                      (rdi r10 r11 r12 r13 r14 r15 r8 r9 rax rbp rbx rcx rdx rsi)
                                      (rdx r10 r11 r12 r13 r14 r15 r8 r9 rax rbp rbx rcx rdi rsi)
                                      (rsi r10 r11 r12 r13 r14 r15 r8 r9 rax rbp rbx rcx rdi rdx)
                                      (s0 rax s1 w0 w1)
                                      (s1 s0 w0 w1)
                                      (w0 rax s0 s1 w1)
                                      (w1 rax s0 s1 w0)))



  (test-generate-graph `(:f 0 0 (x <- 1) (rax += x) (return))          
                       `((r10 r11 r12 r13 r14 r15 r8 r9 rax rbp rbx rcx rdi rdx rsi)
                         (r11 r10 r12 r13 r14 r15 r8 r9 rax rbp rbx rcx rdi rdx rsi)
                         (r12 r10 r11 r13 r14 r15 r8 r9 rax rbp rbx rcx rdi rdx rsi x)
                         (r13 r10 r11 r12 r14 r15 r8 r9 rax rbp rbx rcx rdi rdx rsi x)
                         (r14 r10 r11 r12 r13 r15 r8 r9 rax rbp rbx rcx rdi rdx rsi x)
                         (r15 r10 r11 r12 r13 r14 r8 r9 rax rbp rbx rcx rdi rdx rsi x)
                         (r8 r10 r11 r12 r13 r14 r15 r9 rax rbp rbx rcx rdi rdx rsi)
                         (r9 r10 r11 r12 r13 r14 r15 r8 rax rbp rbx rcx rdi rdx rsi)
                         (rax r10 r11 r12 r13 r14 r15 r8 r9 rbp rbx rcx rdi rdx rsi x)
                         (rbp r10 r11 r12 r13 r14 r15 r8 r9 rax rbx rcx rdi rdx rsi x)
                         (rbx r10 r11 r12 r13 r14 r15 r8 r9 rax rbp rcx rdi rdx rsi x)
                         (rcx r10 r11 r12 r13 r14 r15 r8 r9 rax rbp rbx rdi rdx rsi)
                         (rdi r10 r11 r12 r13 r14 r15 r8 r9 rax rbp rbx rcx rdx rsi)
                         (rdx r10 r11 r12 r13 r14 r15 r8 r9 rax rbp rbx rcx rdi rsi)
                         (rsi r10 r11 r12 r13 r14 r15 r8 r9 rax rbp rbx rcx rdi rdx)
                         (x r12 r13 r14 r15 rax rbp rbx)))

  (println "*************** tests3.L2f *********************************")
  (test-generate-graph `(:f 0 0 
                            (x <- rdi)
                            (a <- rsi)
                            (rax += rdi)
                            (rsi <- 3)
                            (return))
                       `((a r12 r13 r14 r15 rax rbp rbx rdi)
                         (r10 r11 r12 r13 r14 r15 r8 r9 rax rbp rbx rcx rdi rdx rsi)
                         (r11 r10 r12 r13 r14 r15 r8 r9 rax rbp rbx rcx rdi rdx rsi)
                         (r12 a r10 r11 r13 r14 r15 r8 r9 rax rbp rbx rcx rdi rdx rsi x)
                         (r13 a r10 r11 r12 r14 r15 r8 r9 rax rbp rbx rcx rdi rdx rsi x)
                         (r14 a r10 r11 r12 r13 r15 r8 r9 rax rbp rbx rcx rdi rdx rsi x)
                         (r15 a r10 r11 r12 r13 r14 r8 r9 rax rbp rbx rcx rdi rdx rsi x)
                         (r8 r10 r11 r12 r13 r14 r15 r9 rax rbp rbx rcx rdi rdx rsi)
                         (r9 r10 r11 r12 r13 r14 r15 r8 rax rbp rbx rcx rdi rdx rsi)
                         (rax a r10 r11 r12 r13 r14 r15 r8 r9 rbp rbx rcx rdi rdx rsi x)
                         (rbp a r10 r11 r12 r13 r14 r15 r8 r9 rax rbx rcx rdi rdx rsi x)
                         (rbx a r10 r11 r12 r13 r14 r15 r8 r9 rax rbp rcx rdi rdx rsi x)
                         (rcx r10 r11 r12 r13 r14 r15 r8 r9 rax rbp rbx rdi rdx rsi)
                         (rdi a r10 r11 r12 r13 r14 r15 r8 r9 rax rbp rbx rcx rdx rsi)
                         (rdx r10 r11 r12 r13 r14 r15 r8 r9 rax rbp rbx rcx rdi rsi)
                         (rsi r10 r11 r12 r13 r14 r15 r8 r9 rax rbp rbx rcx rdi rdx x)
                         (x r12 r13 r14 r15 rax rbp rbx rsi)))
|#


  (test-generate-graph `(:f 1 0
                            (x <- rdi)
                            (y <- 1)
                            (y <<= x)
                            (x <<= y)
                            (rax <- x)
                            (return))
                       `((r10 r11 r12 r13 r14 r15 r8 r9 rax rbp rbx rcx rdi rdx rsi x y)
                          (r11 r10 r12 r13 r14 r15 r8 r9 rax rbp rbx rcx rdi rdx rsi x y)
                          (r12 r10 r11 r13 r14 r15 r8 r9 rax rbp rbx rcx rdi rdx rsi x y)
                          (r13 r10 r11 r12 r14 r15 r8 r9 rax rbp rbx rcx rdi rdx rsi x y)
                          (r14 r10 r11 r12 r13 r15 r8 r9 rax rbp rbx rcx rdi rdx rsi x y)
                          (r15 r10 r11 r12 r13 r14 r8 r9 rax rbp rbx rcx rdi rdx rsi x y)
                          (r8 r10 r11 r12 r13 r14 r15 r9 rax rbp rbx rcx rdi rdx rsi x y)
                          (r9 r10 r11 r12 r13 r14 r15 r8 rax rbp rbx rcx rdi rdx rsi x y)
                          (rax r10 r11 r12 r13 r14 r15 r8 r9 rbp rbx rcx rdi rdx rsi x y)
                          (rbp r10 r11 r12 r13 r14 r15 r8 r9 rax rbx rcx rdi rdx rsi x y)
                          (rbx r10 r11 r12 r13 r14 r15 r8 r9 rax rbp rcx rdi rdx rsi x y)
                          (rcx r10 r11 r12 r13 r14 r15 r8 r9 rax rbp rbx rdi rdx rsi)
                          (rdi r10 r11 r12 r13 r14 r15 r8 r9 rax rbp rbx rcx rdx rsi x y)
                          (rdx r10 r11 r12 r13 r14 r15 r8 r9 rax rbp rbx rcx rdi rsi x y)
                          (rsi r10 r11 r12 r13 r14 r15 r8 r9 rax rbp rbx rcx rdi rdx x y)
                          (x r10 r11 r12 r13 r14 r15 r8 r9 rax rbp rbx rdi rdx rsi y)
                          (y r10 r11 r12 r13 r14 r15 r8 r9 rax rbp rbx rdi rdx rsi x)))
                       




  )