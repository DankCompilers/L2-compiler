#lang racket
(require racket/cmdline)
(require "L2toL1.rkt")

(command-line
 #:program "L2 compiler"
 #:args ((filename #f)) ; expect one command-line argument: <filename>
 (cond [filename
        (call-with-input-file filename
          (lambda (in)
            (let    ([l1-code (L2->L1-compile (read in))])
              (if (boolean? l1-code)
                  (write "could not register allocate")
                  (write  (read (open-input-string l1-code)))))))]
       [else (error "Provide a filename")]))

