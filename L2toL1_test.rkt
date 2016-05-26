#lang racket


(require "L2toL1.rkt")

(define print-debug #f)

(define (debug-printer proc . args)
  (when print-debug (printf "given:    ~a\nexpected: ~a\n\n" (first args) (last args)))
  (apply proc args))

(define (debug-print str . args)
  (when print-debug (apply printf (append (list str) args))))

(module+ test
  (require rackunit)

  (define (test-L2->L1 raw-prog)
    (println  (L2->L1-compile raw-prog))
    ;(debug-printer check-equal? (L2->L1-compile raw-prog) expected-string)
    )
#|

  (test-L2->L1 `(:L_1
                 (:L_1 0 0
                       (x <- 7)
                       (x += 4)
                       (rdi <- x)
                       (call print 1)
                       (return))))

  (test-L2->L1 `(:main
                 
                 (:main
                  0 0
                  (rdi <- 19)
                  (rsi <- 19)
                  (call allocate 2)
                  (rdi <- rax)
                  ((mem rsp -8) <- :f_ret)
                  (call :f 1)
                  :f_ret
                  (return)) 
                 (:f
                  1 0
                  (curr <- rdi)
                  (curr += 8)
                  (end <- (mem rdi 0))
                  (end <<= 3)
                  (end += rdi)
                  (rax <- 0)
                  :start
                  (cjump curr <= end :body :end)
                  :body
                  (val <- (mem curr 0))
                  (val -= 1)
                  (val >>= 1)
                  (rax += val)
                  (curr += 8)
                  (goto :start)
                  :end
                  (rax <<= 1)
                  (rax += 1)
                  (return))))


  
  (test-L2->L1 `(:main
                 (:main
                  0 0
                  (x <- 1)
                  (r10 <- 3)
                  (r11 <- 5)
                  (r8 <- 7)
                  (r9 <- 9)
                  (rax <- 11)
                  (rcx <- 13)
                  (rdi <- 15)
                  (rdx <- 17)
                  (rsi <- 19)
                  (rdi += x)
                  (rdi += r10)
                  (rdi += r11)
                  (rdi += rax)
                  (tail-call :f 6))
                 (:f
                  6 0
                  (call print 1)
                  (return)))) 

  (println "Testing 12-stack-arg-spill.L2")

  (test-L2->L1 `(:main
                 (:main
                  0 0
                  (rdi <- 1)
                  (rsi <- 3)
                  (rcx <- 5)
                  (rdx <- 7)
                  (r8 <- 9)
                  (r9 <- 11)
                  ((mem rsp -8) <- :f_ret)
                  ((mem rsp -16) <- 13)
                  ((mem rsp -24) <- 15)
                  (call :f 8)
                  :f_ret
                  (rdi <- rax)
                  (call print 1)
                  (return))
                 (:f
                  8 2
                  (x <- 1)
                  (rdi <- 3)
                  (call print 1)
                  (rax <- (stack-arg 8))
                  (rdi <- (stack-arg 0))
                  (rax += rdi)
                  (rax += x)
                  (return)))) 


  (test-L2->L1 `(:main
                 (:main
                  0 0
                  (rdi <- 1)
                  (rsi <- 3)
                  (rcx <- 5)
                  (rdx <- 7)
                  (r8 <- 9)
                  (r9 <- 11)
                  ((mem rsp -8) <- :f_ret)
                  ((mem rsp -16) <- 13)
                  ((mem rsp -24) <- 15)
                  (call :f 8)
                  :f_ret
                  (rdi <- rax)
                  (call print 1)
                  (return))
                 (:f
                  8 2
                  (rax <- (stack-arg 8))
                  (rdi <- (stack-arg 0))
                  (rax += 1)
                  (rax += rdi)
                  (return)))
|#

  (println "Testing Robby-20.L2")
  (test-L2->L1 `(:L_1
                 (:L_1
                  0
                  0
                  (rdi <- 21)
                  (rsi <- 23)
                  (call allocate 2)
                  (a <- rax)
                  (x_2 <- 7)
                  (bnds_3 <- (mem a 0))
                  (bnds_3 <<= 1)
                  (bnds_3 += 1)
                  (cjump x_2 < bnds_3 :oka_5 :die_4)
                  :oka_5
                  (cjump 0 <= x_2 :okb_6 :die_4)
                  :die_4
                  (rdi <- a)
                  (rsi <- 7)
                  (call array-error 2)
                  :okb_6
                  (x_2 >>= 1)
                  (x_2 *= 8)
                  (x_2 += a)
                  ((mem x_2 8) <- 643)
                  (i <- 1)
                  (x_7 <- 7)
                  (bnds_8 <- (mem a 0))
                  (bnds_8 <<= 1)
                  (bnds_8 += 1)
                  (cjump x_7 < bnds_8 :oka_10 :die_9)
                  :oka_10
                  (cjump 0 <= x_7 :okb_11 :die_9)
                  :die_9
                  (rdi <- a)
                  (rsi <- 7)
                  (call array-error 2)
                  :okb_11
                  (x_7 >>= 1)
                  (x_7 *= 8)
                  (x_7 += a)
                  (v <- (mem x_7 8))
                  (rdi <- v)
                  (call print 1)
                  (return))))

               
               
  (debug-print "Done with L2-to-L1 tests"))


#|
(:main (:main 0 0
              (rdi <- 19)
              (rsi <- 19)
              (call allocate 2)
              (rdi <- rax)
              ((mem rsp -8) <- :f_ret)
              (call :f 1)
              :f_ret
              (return))
       (:f 1 0
           (rdx <- rdi)
           (rdx += 8)
           (rcx <- (mem rdi 0))
           (rcx <<= 3)
           (rcx += rdi)
           (rax <- 0)
           :start
           (cjump rdx <= rcx :body :end)
           :body
           (r8 <- (mem curr 0))     ;; error is that curr here did not get replace
           (r8 -= 1)
           (r8 >>= 1)
           (rax += r8)
           (rdx += 8)
           (goto :start)
           :end
           (rax <<= 1)
           (rax += 1)
           (return)))



12-stack-arg-spill
(:main (:main 0 0
                (rdi <- 1)
                (rsi <- 3)
                (rcx <- 5)
                (rdx <- 7)
                (r8 <- 9)
                (r9 <- 11)
                ((mem rsp -8) <- :f_ret)
                ((mem rsp -16) <- 13)
                ((mem rsp -24) <- 15)
                (call :f 8)
                :f_ret
                (rdi <- rax)
                (call print 1)
                (return))
         (:f 8 3
             ((mem rsp 16) <- 1)
             (rdi <- 3)
             (call print 1)
             (rax <- (mem rsp 24))
             (rdi <- (mem rsp 16))
             (rax += rdi)
             (rdx <- (mem rsp 16))
             (rax += rdx)
             (return)))

|#




  