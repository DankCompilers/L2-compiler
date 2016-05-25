#lang racket
(require racket/cmdline)
(require "graph-color.rkt" "liveness.rkt" "parser.rkt" "AST.rkt")

(command-line
 #:program "graph coloring"
 #:args ((filename #f)) ; expect one command-line argument: <filename>
 (cond [filename
        (call-with-input-file filename
          (lambda (in)
            (let* ([func-ast           (parse-function (read in))]         
                   [gen-kills          (generate-gens-kills func-ast)]
                   [gens               (first gen-kills)]
                   [kills              (second gen-kills)]
                   [successors         (generate-successors func-ast)]
                   [in-outs            (generate-in-out gens kills successors)]
                   [ins                (first in-outs)]
                   [outs               (second in-outs)]
                   [uncolored-graph    (generate-uncolored-graph ins outs kills func-ast)]
                   [adjacency-graph    (convert-to-adjacency uncolored-graph)]
                   [colored-graph      (color-graph-function uncolored-graph)]
                   [var-colored-graph  (variables-only-coloring colored-graph)])
              ;(printf "adjacency-graph:\n~a\n\n" adjacency-graph)
              ;(printf "var-colored-graph:\n~a\n\n"    var-colored-graph)
              (write  adjacency-graph)
              (write  var-colored-graph)
              )))
        ]
       [else (error "Provide a filename")]))

