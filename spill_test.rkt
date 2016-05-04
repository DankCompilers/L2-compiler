#lang racket


(require "spill.rkt" "parser.rkt")

(define print-debug #f)

(define (debug-printer proc . args)
  (when print-debug (printf "given:    ~a\nexpected: ~a\n\n" (first args) (last args)))
  (apply proc args))


(module+ test
  (require rackunit)
  (require "liveness.rkt")

  ;; used to generate unique temps
  (define current-one -1)
  (define (next-temp)
    (set! current-one (+ current-one 1))
    (string->symbol (format "s~a" current-one)))
    
  (define (test-spill-instruction quoted-inst var-name quoted-expected)
    (let* ([i-ast    (parse-instruction quoted-inst)]
           [gen-kill (generate-gen-kill i-ast)])
    (debug-printer check-equal? (spill-instruction var-name 8 next-temp i-ast (first gen-kill) (second gen-kill))
                   (map parse-instruction quoted-expected))))

  ;; test assignments
  ;; test mems2w
  (test-spill-instruction `(a <- 1)   'a `(((mem rsp 8) <- 1)))
  (test-spill-instruction `(rdi <- 1) 'a `((rdi <- 1)))
  (test-spill-instruction `(rdi <- b) 'b `((rdi <- (mem rsp 8))))
  (test-spill-instruction `(a <- b)   'a `(((mem rsp 8) <- b)))

  ;; test mems2mem
  (test-spill-instruction `((mem rsp 0) <- 1) 'a `(((mem rsp 0) <- 1)))
  (test-spill-instruction `((mem rsp 0) <- a) 'a `((s0 <- (mem rsp 8))
                                                   ((mem rsp 0) <- s0)))

  ;; test memmem2w
  (test-spill-instruction `(a <- (mem rsp 0)) 'a `((s1 <- (mem rsp 0))
                                                   ((mem rsp 8) <- s1)))
  (test-spill-instruction `(rdi <- (mem rsp 0)) 'a `((rdi <- (mem rsp 0))))

  ;; test aop
  (test-spill-instruction `(a += a) 'a `((s2 <- (mem rsp 8))
                                         (s2 += s2)
                                         ((mem rsp 8) <- s2)))
  (test-spill-instruction `(a -= 1) 'a `((s3 <- (mem rsp 8))
                                         (s3 -= 1)
                                         ((mem rsp 8) <- s3)))
  (test-spill-instruction `(rax *= raxor) 'raxor `((s4 <- (mem rsp 8))
                                         (rax *= s4)))


  ; test sopsx/sopn
  (test-spill-instruction `(rax <<= avar) 'avar `((s5 <- (mem rsp 8))
                                                  (rax <<= s5)))
  (test-spill-instruction `(rax <<= 5) 'avar `((rax <<= 5)))
  (test-spill-instruction `(blah <<= avar) 'blah `((s6 <- (mem rsp 8))
                                                  (s6 <<= avar)
                                                  ((mem rsp 8) <- s6)))
  (test-spill-instruction `(rahhhh <<= 5) 'rahhhh `((s7 <- (mem rsp 8))
                                                    (s7 <<= 5)
                                                    ((mem rsp 8) <- s7)))

  ;; test cjump
  (test-spill-instruction `(cjump mine < 5 :live :die) 'mine `((s8 <- (mem rsp 8))
                                                               (cjump s8 < 5 :live :die)))
  (test-spill-instruction `(cjump mine = mine :live :die) 'mine `((s9 <- (mem rsp 8))
                                                               (cjump s9 = s9 :live :die)))
  (test-spill-instruction `(cjump 10 < mine :live :die) 'mine `((s10 <- (mem rsp 8))
                                                               (cjump 10 < s10 :live :die)))

  ;; test memcmp2w
  (test-spill-instruction `(avar <- mine < 5) 'mine `((s11 <- (mem rsp 8))
                                                      (avar <- s11 < 5)))
  (test-spill-instruction `(avar <- mine < 5) 'avar `((s12 <- mine < 5)
                                                      ((mem rsp 8) <- s12)))
  (test-spill-instruction `(avar <- mine < mine) 'mine `((s13 <- (mem rsp 8))
                                                        (avar <- s13 < s13)))
  (test-spill-instruction `(mine <- mine < mine) 'mine `((s14 <- (mem rsp 8))
                                                        (s14 <- s14 < s14)
                                                        ((mem rsp 8) <- s14)))
  
  ;; test call and tail call                       
  (test-spill-instruction `(call raxor 2) 'raxor `((s15 <- (mem rsp 8))
                                                   (call s15 2)))
  (test-spill-instruction `(call :alabel 2) 'raxor `((call :alabel 2)))
  (test-spill-instruction `(tail-call raxor 2) 'raxor `((s16 <- (mem rsp 8))
                                                   (tail-call s16 2)))
  (test-spill-instruction `(tail-call :alabel 2) 'raxor `((tail-call :alabel 2)))
  
  ;; test simple ones
  (test-spill-instruction `(goto :label)  'a  `((goto :label)))
  (test-spill-instruction `(call print 1) 'print  `((call print 1)))
  (test-spill-instruction `(call allocate 2) 'allocate  `((call allocate 2)))
  (test-spill-instruction `(call array-error 2) 'p  `((call array-error 2)))
  (test-spill-instruction `(return) 'return  `((return)))
  )

