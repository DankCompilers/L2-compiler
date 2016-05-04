#lang racket

(require "AST.rkt" "parser.rkt" "backend.rkt" "lib.rkt")


(define print-debug #f)

(define (debug-printer proc . args)
  (when print-debug (printf "given:    ~a\nexpected: ~a\n\n" (first args) (last args)))
  (apply proc args))

(define (debug-print str . args)
  (when print-debug (apply printf (append (list str) args))))


(module+ test
  (require rackunit)
  (define (test-backend parse raw-str)
    (let* ([an-ast          (parse raw-str)]
           [compiled-str    (ast-to-string an-ast)])
      (debug-printer check-equal? compiled-str (to-string raw-str))))

  ;; test token nodes
  (test-backend parse-token 'hello)
  (test-backend parse-token 'l1)
  (test-backend parse-token 5)
  (test-backend parse-token -5)
  (test-backend parse-token 'rdi)
  (test-backend parse-token 'rsp)
  (test-backend parse-token 'rcx)
  (test-backend parse-token 'backatitagain)
  (test-backend parse-token 'withthewhitevans)
  (debug-print "Finished token node tests")

  ;; test compound token node tests
  (debug-printer check-equal?  (ast-to-string (make-mem-node (parse-token 'rsp) (parse-token 16))) "(mem rsp 16)")
  (debug-printer check-equal?  (ast-to-string (make-stack-node (parse-token 2))) "(stack 2)")
  (debug-print "Finished compound token node tests")

  ;; test control flow nodes
  (test-backend parse-instruction '(cjump t1 < t2 :hello :goodbye))
  (test-backend parse-instruction '(cjump t1 < t2 :hello :badplace))
  (test-backend parse-instruction '(call print 1))
  (test-backend parse-instruction '(call allocate 2))
  (test-backend parse-instruction '(call array-error 2))
  (test-backend parse-instruction '(call :hello 0))
  (test-backend parse-instruction '(call rdi 0))
  (test-backend parse-instruction '(tail-call :hello 0))
  (test-backend parse-instruction '(tail-call rax 0))
  (test-backend parse-instruction '(goto :good_time))
  (test-backend parse-instruction '(return))
  (debug-print "Finished control flow node tests")

  ;; test arithmetic nodes
  (test-backend parse-instruction '(rdi += rdi))
  (test-backend parse-instruction '(rdi -= rax))
  (test-backend parse-instruction '(avar *= rdi))
  (test-backend parse-instruction '(r11 &= raxor))
  (test-backend parse-instruction '(rdi <<= 5))
  (test-backend parse-instruction '(nien >>= tien))
  (test-backend parse-instruction '(nien <<= rcx))
  (test-backend parse-instruction '(nien += 6))
  (test-backend parse-instruction '(nien *= far))
  (debug-print "Finished arithmetic node tests")

  ;; test assignment nodes
  (debug-print "Finished assignment node tests")
  (debug-print "Finished collection node tests"))
  
