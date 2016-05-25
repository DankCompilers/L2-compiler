#lang racket
(require "./parser.rkt")
(require "./backend.rkt")
(require "./spill.rkt")
(require "./liveness.rkt")
(require "./lib.rkt")
(require "./AST.rkt")
(require racket/cmdline)
(command-line
 #:program "spill function"
 #:args ((filename #f)) ; expect one command-line argument: <filename>
 (cond [filename
        (call-with-input-file filename
          (lambda (in)
            (let* ([function-ast   (parse-function (read in))]
                   [spill-var      (read in)]
                   [spill-prefix   (read in)]
                   [gens-kills     (generate-gens-kills function-ast)]
                   [gens           (first  gens-kills)]
                   [kills          (second gens-kills)]
                   [spilled-func   (spill-function spill-var spill-prefix function-ast gens kills)]
                   [converted-func (ast-to-string spilled-func)])
                  (write (read (open-input-string converted-func))))))]
       [else (error "Provide a filename")]))

