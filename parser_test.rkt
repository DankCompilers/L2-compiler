#lang racket

(require "parser.rkt"
         "AST.rkt")

(define print-debug #f)

(define (negate-pred pred) (lambda (x) (not (pred x))))
(define num-only (list 2 5 2 1 592304))
(define var-only `(parax barax raxor rdimine ardi raxrdi hello s0 whatis324))
(define sx-only `(rcx))
(define a-only `(rdi rsi rdx r9 r9))
(define w-only `(rax rbx rbp r10 r11 r12 r13 r14 r15))
(define x-only `(rsp));)
(define aop-only `(+= -= *= &=))
(define sop-only `(>>= <<=))
(define cmp-only `(< <= =))

;; Instruction parsing tests
(module+ test
  (require rackunit)
  (define (check-equal-print v1 v2)
    (when print-debug (printf "given:    ~a\nexpected: ~a\n\n" v1 v2))
    (check-equal? v1 v2))

  ;;aop tests
  (check-equal-print (parse-instruction `(rdi += 43))          (AST 'aop '() (parse-tokens 'rdi   '+= 43)))
  (check-equal-print (parse-instruction `(raxor -= 43))        (AST 'aop '() (parse-tokens 'raxor '-= 43)))
  (check-equal-print (parse-instruction `(rax *= raxor))       (AST 'aop '() (parse-tokens 'rax   '*= 'raxor)))
  (check-equal-print (parse-instruction `(raxor &= rdimate))   (AST 'aop '() (parse-tokens 'raxor '&= 'rdimate)))
  
  (printf "Done with aop tests\n\n")
  
  ;;sop tests
  (check-equal-print (parse-instruction `(rdi <<= 43))      (AST 'sopn  '() (parse-tokens 'rdi '<<= 43)))
  (check-equal-print (parse-instruction `(raxor <<= 10))    (AST 'sopn  '() (parse-tokens 'raxor '<<= 10)))
  (check-equal-print (parse-instruction `(rax >>= rcx))     (AST 'sopsx '() (parse-tokens 'rax '>>= 'rcx)))
  (check-equal-print (parse-instruction `(raxor >>= rcx))   (AST 'sopsx '() (parse-tokens 'raxor '>>= 'rcx)))
  (check-equal-print (parse-instruction `(raxor >>= raxor)) (AST 'sopsx '() (parse-tokens 'raxor '>>= 'raxor)))

  (printf "Done with sop tests\n\n")
  
  ;; assignment tests
  ;; mems2w
  (check-equal-print (parse-instruction `(rdi <- 43))       (AST 'mems2w empty (parse-tokens 'rdi 43)))
  (check-equal-print (parse-instruction `(rdi <- :hello))   (AST 'mems2w empty (parse-tokens 'rdi ':hello)))
  (check-equal-print (parse-instruction `(rax <- 1000))     (AST 'mems2w empty (parse-tokens 'rax 1000)))
  (check-equal-print (parse-instruction `(rdi <- rax))      (AST 'mems2w empty (parse-tokens 'rdi 'rax)))
  (check-equal-print (parse-instruction `(v1  <- v2))       (AST 'mems2w empty (parse-tokens 'v1 'v2)))
  (check-equal-print (parse-instruction `(v1  <- rax))      (AST 'mems2w empty (parse-tokens 'v1 'rax)))
  (check-equal-print (parse-instruction `(rcx <- v2))      (AST 'mems2w empty (parse-tokens 'rcx 'v2)))

  (printf "Done with mems2w tests\n\n")
  
  ;; memmem2w
  (check-equal-print (parse-instruction `(rdi <- (mem rsp 0)))   (AST 'memmem2w empty (list (parse-token 'rdi)
                                                                                            (make-mem-node (parse-token 'rsp) (parse-token 0)))))
  
  (check-equal-print (parse-instruction `(avar <- (mem rsp -8)))  (AST 'memmem2w empty (list (parse-token 'avar)
                                                                                            (make-mem-node (parse-token 'rsp) (parse-token -8)))))

  (check-equal-print (parse-instruction `(rcx <- (mem rsp -8)))  (AST 'memmem2w empty (list (parse-token 'rcx)
                                                                                            (make-mem-node (parse-token 'rsp) (parse-token -8)))))

  (printf "Done with memmem2w tests\n\n")
  ;; mems2mem
  (check-equal-print (parse-instruction `((mem rsp -8) <- rcx))  (AST 'mems2mem empty (list  (make-mem-node (parse-token 'rsp) (parse-token -8))
                                                                                             (parse-token 'rcx))))

  (check-equal-print (parse-instruction `((mem rsp -8) <- :hello))  (AST 'mems2mem empty (list  (make-mem-node (parse-token 'rsp) (parse-token -8))
                                                                                             (parse-token ':hello))))

  (check-equal-print (parse-instruction `((mem rsp -8) <- raxor))  (AST 'mems2mem empty (list  (make-mem-node (parse-token 'rsp) (parse-token -8))
                                                                                               (parse-token 'raxor))))


  (printf "Done with mems2mem tests\n\n")

  ;; memcmp2w
  (check-equal-print (parse-instruction `(rcx <- rdi <= 5))        (AST 'memcmp2w empty  (parse-tokens 'rcx  'rdi '<= 5)))

  (check-equal-print (parse-instruction `(rax <- rax < raxor))     (AST 'memcmp2w empty  (parse-tokens 'rax 'rax '< 'raxor)))

  (check-equal-print (parse-instruction `(raxor <- raxor = raxor))  (AST 'memcmp2w empty  (parse-tokens 'raxor 'raxor '= 'raxor)))


  (printf "Done with memcmp2w tests\n\n")
  
  ;; simple instructions
  (check-equal-print (parse-instruction ':hello) (parse-token ':hello))
  (check-equal-print (parse-instruction `(goto :hello)) (AST 'goto '() (parse-tokens  ':hello)))
  (check-equal-print (parse-instruction `(call array-error 2)) (AST 'array-error empty (parse-tokens 2)))
  (check-equal-print (parse-instruction `(call allocate 2)) (AST 'allocate empty (parse-tokens 2)))
  (check-equal-print (parse-instruction `(call print 1)) (AST 'print empty (parse-tokens 1)))
  (check-equal-print (parse-instruction `(tail-call :hi 1)) (AST 'tail-call empty (parse-tokens ':hi 1)))
  (check-equal-print (parse-instruction `(return)) (AST 'return empty empty))


  ;; Check failure detection
  (check-exn    exn:fail?  (parse-instruction 'hello))
  (check-exn    exn:fail?  (parse-instruction `(call :tester -1)))
  (check-exn    exn:fail?  (parse-instruction `(call array-error 1)))
  (check-exn    exn:fail?  (parse-instruction `(call allocate 1)))
  (check-exn    exn:fail?  (parse-instruction `(call print 2)))
  (check-exn    exn:fail?  (parse-instruction `()))
  (check-exn    exn:fail?  (parse-instruction `(345324 543543 5435)))
  (displayln "Instruction parsing end"))







;; token parsing tests
(module+ test
  (require rackunit)
  (define (validate-token-node type-check expected-data an-ast)
    (check-pred type-check an-ast)
    (check-equal? expected-data (get-first-data an-ast)))
  
  (for ([i var-only])
       (validate-token-node is-var-node? i (parse-token i)))
  
  (for ([i sx-only])
       (validate-token-node is-sx-node? i (parse-token i)))
  
  (for ([i a-only])
       (validate-token-node is-a-node? i (parse-token i)))

  (for ([i w-only])
       (validate-token-node is-w-node? i (parse-token i)))
  
  (for ([i x-only])
       (validate-token-node is-x-node? i (parse-token i)))
  
  (for ([i num-only])
       (validate-token-node is-num-node? i (parse-token i)))
  (displayln "Token parsing tests end"))