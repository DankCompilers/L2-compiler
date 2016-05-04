#lang racket


(require "AST.rkt" "lib.rkt")



;; AST -> string
(define (ast-to-string an-ast)
  ;; string listof(symbols) -> string 
  (define (handle-general-case f-str . args)
    (apply format (cons f-str (map ast-to-string args))))
  
  (let* ([children      (AST-children an-ast)]
      (match (AST-type i-ast)
        ['program]
        ['func                 (handle )]
        ;; handle instruction nodes
        [(or 'mems2w
             'memmem2w
             'mems2mem)        (handle-general-case "(~a <- ~a)"  children)]
        ['memcmp2w             (handle-general-case "(~a <- ~a ~a ~a)" children)]
        ;; handle arithmetic ops
        [(or 'aop
             'sopsx
             'sopn)             (handle-general-case "(~a ~a ~a)" (flip-operator children))]
        ['cjump                 (handle-general-case "(cjump ~a ~a ~a ~a ~a" (flip-frst children))]
        ['goto                  (handle-general-case "(goto ~a)" children)  ]
        ['print                 (handle-general-case "(call print ~a)" children)]
        ['allocate              (handle-general-case "(call allocate ~a)" children) ]
        ['array-error           (handle-general-case "(call array-error ~a)" children)]
        ['call                  (handle-general-case "(call ~a ~a)" children)]
        ['tail-call             (handle-general-case "(tail-call ~a ~a)" children)]
        ['return                "(return)"]
        ;; handle compound token nodes
        ['mem                  (handle-general-case "(mem ~a ~a)" children)]
        ['stack                (handle-general-case "(stack ~a)"  children)]
        ;; handle token nodes - return their data
        [(or 'label
             'var
             (? is-reg-node?))  (to-string (get-first-data an-ast))]
        [_                      (lambda () (error "generate-gen-kill: could not understand ~a" i-ast))])))

