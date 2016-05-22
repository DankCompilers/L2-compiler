#lang racket
(require "./parser.rkt")
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
           (let*
                  ([functionAST (parse-function (read in))]
                  [spillVar (read in)]
                  [spillPref (read in)]
                  [instructions-count (num-children functionAST)]
                  [gen  `()]
                  [kill `()])
                  (begin
                      (for ([i (range instructions-count)])
                           (let* ([gen-kill-singleton-before (ast-child functionAST i)]
                                  [gen-kill-singleton (generate-gen-kill gen-kill-singleton-before)])
                                (begin (set! gen  (append gen  (list (first gen-kill-singleton))))
                                       (set! kill (append kill (list (second gen-kill-singleton))))
                                       ;;(write gen)
                                       ;;(write kill)
                                       )))
                      ;;Writing to test
                      ;;(write gen)
                      ;;(write kill)
                      ;;(write spillPref)
                      ;;(write spillVar)
                      ;;(write functionAST)
                      ;;Writing output to io
                      (let* ([spilled-func-AST (spill-function spillVar spillPref functionAST gen kill)]
							 [spilled-instr-count (num-children spilled-func-AST)])
						(write (walk-functionAST spilled-func-AST spilled-instr-count)))))))]
      [else (error "Provide a filename bitch!")]))

