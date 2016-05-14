#lang racket
(require "./parser.rkt")
(require "./liveness.rkt")
(require racket/cmdline)
(command-line
 #:program "Liveness analyzer"
 #:args ((filename #f)) ; expect one command-line argument: <filename>
 (cond [filename
        (call-with-input-file filename
          (lambda (in)
            (write (liveness-analysis (parse-function (read in))))))]
       [else (error "Provide a filename bitch!")]))
