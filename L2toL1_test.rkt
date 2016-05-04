#lang racket



(define print-debug #f)

(define (debug-printer proc . args)
  (when print-debug (printf "given:    ~a\nexpected: ~a\n\n" (first args) (last args)))
  (apply proc args))

(define (debug-print str . args)
  (when print-debug (apply printf (append (list str) args))))

(module+ test
  (require rackunit)


  (debug-print "Done with L2-to-L1 tests"))



  