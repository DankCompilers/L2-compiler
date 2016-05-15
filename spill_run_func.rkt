#lang racket
(require "./parser.rkt")
(require "./spill.rkt")
(require "./liveness.rkt")
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
                                [gen-kill-singleton (set->list (generate-gen-kill gen-kill-singleton-before))])
                                (begin (set! gen (append gen (car gen-kill-singleton)))
                                        (set! kill (append kill (rest gen-kill-singleton))))))
                    (write  (spill-function spillVar spillPref functionAST gen kill))))))]
      [else (error "Provide a filename bitch!")]))

