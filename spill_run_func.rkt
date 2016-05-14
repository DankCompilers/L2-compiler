#lang racket
(require "./parser.rkt")
(require "./spill.rkt")
(require "./liveness")
(require racket/cmdline)
(command-line
 #:program "spill function"
 #:args ((filename #f)) ; expect one command-line argument: <filename>
 (cond [filename
        (call-with-input-file filename
         (lambda (in)
           (let* ([spillFileReturn (spillFile (read in))]
                  [functionAST (parse-function (car spillFileReturn))]
                  [spillVar (car(rest spillFileReturn))]
                  [spillPref (rest(rest spillFileReturn))]
                  [gen-kill (generate-gen-kill functionAST)])
         (write  (spill-function spillVar spillPref (car gen-kill) (rest gen-kill))))))]
      [else (error "Provide a filename bitch!")]))

(define (spillFile fileInput)
 (let * ([fileList (file->list fileInput)])
  fileList))

