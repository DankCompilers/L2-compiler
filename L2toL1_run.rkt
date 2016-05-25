#lang racket
(require racket/cmdline)
(require "L2toL1.rkt")

(command-line
 #:program "L2 compiler"
 #:args ((filename #f)) ; expect one command-line argument: <filename>
 (cond [filename
        (call-with-input-file filename
          (lambda (in)
              (write  (read (open-input-string (L2->L1-compile (read in)))))))]
       [else (error "Provide a filename")]))

