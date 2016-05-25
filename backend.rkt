#lang racket

(require "AST.rkt" "lib.rkt")
(provide ast-to-string)

;; AST -> string
(define (ast-to-string an-ast)
  ;; string listof(symbols) -> string 
  (define (handle-general-case f-str args)
    (apply format (cons f-str (map ast-to-string args))))

  (define (handle-collection-case start-str children)
    (string-join (cons start-str
                       (map ast-to-string children))
                 #:after-last ")"))
  
  (let* ([children      (AST-children an-ast)])
      ;(printf "~a" (AST-type an-ast))
      (match (AST-type an-ast)
        ;; handles collection nodes
        ['program              (handle-collection-case  (format "(~a" (get-first-data an-ast)) children)]
        ['func                 (handle-collection-case  (format "(~a ~a ~a"
                                                                (get-first-data an-ast)
                                                                (get-second-data an-ast)
                                                                (get-third-data an-ast))
                                                        children)]
        ;; handle assignment nodes
        [(or 'mems2w
             'memmem2w
             'mems2mem
             'memstack2w)        (handle-general-case "(~a <- ~a)"  children)]
        ['memcmp2w             (handle-general-case "(~a <- ~a ~a ~a)" children)]
        ;; handle arithmetic ops
        [(or 'aop
             'sopsx
             'sopn)             (handle-general-case "(~a ~a ~a)" children)]
        ;; handle control flow nodes
        ['cjump                 (handle-general-case "(cjump ~a ~a ~a ~a ~a)" children)]
        ['goto                  (handle-general-case "(goto ~a)" children)  ]
        ['print                 (handle-general-case "(call print ~a)" children)]
        ['allocate              (handle-general-case "(call allocate ~a)" children) ]
        ['array-error           (handle-general-case "(call array-error ~a)" children)]
        ['call                  (handle-general-case "(call ~a ~a)" children)]
        ['tail-call             (handle-general-case "(tail-call ~a ~a)" children)]
        ['return                "(return)"]
        ;; handle compound token nodes
        ['mem                  (handle-general-case "(mem ~a ~a)" children)]
        ['stack                (handle-general-case "(stack-arg ~a)" children)]
        ;; handle token nodes - return their data
        [(or 'num
             'label
             'var
             'cmp
             'aopop
             'sopop)              (to-string (get-first-data an-ast))]
        [_                      (if (is-reg-node? an-ast) (to-string (get-first-data an-ast))
                                    (error "ast-to-string: could not understand " an-ast))])))