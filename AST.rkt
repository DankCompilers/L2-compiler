#lang racket

(provide (all-defined-out))
;;;;;;;;;;;;;; STRUCT DEFINITION ;;;;;;;;;;;;;;;;;;

(struct AST (type data children)
  #:transparent
  #:mutable)

 
(define caller-save `(r8 r9 r10 r11 rdi rsi rdx rcx rax))
(define callee-save `(r12 r13 r14 r15 rbp rbx))
(define args        `(rdi rsi rdx rcx r8 r9))
(define result      `(rax))
(define register-names (append caller-save callee-save args `(rsp)))



;;;;;;;;;;;;;; DISPLAY HELPERS ;;;;;;;;;;;;;;;;;;


(define (node-to-string an-ast)
  (format "label:~a  data: ~a  children:~a" (AST-type an-ast) (AST-data an-ast) (AST-children an-ast)))

(define (print-node an-ast)
  (display (node-to-string an-ast)))


;;;;;;;;;;;;;; FACTORY HELPERS ;;;;;;;;;;;;;;;;;;

;; x-AST num-AST -> AST
(define (make-mem-node x-ast n8-ast)
  (AST 'mem '() (list x-ast  n8-ast)))

;; x8 -> AST
(define (make-stack-node n8-ast)
  (AST 'stack '() (list n8-ast)))


;; cmp t t -> AST
(define (make-cmp-node cmpop t1 t2)
  (AST 'cmp (list cmpop t1 t2) empty ))


;; token-type token-date -> AST
(define (make-token-node node-type token)
  (AST node-type (list token) empty))


;; symbol quoted -> AST
;;(define (make-goto-node label raw)
;;  (AST 'goto (list label) (parse-tokens label) m))


;;;;;;;;;;;;;; ACCESSOR HELPERS ;;;;;;;;;;;;;;;;;;


;; returns a copy of an-ast with the children replaced with new-children
;; AST -> AST
(define (set-AST-children an-ast new-children)
  (AST (AST-type an-ast) (AST-data an-ast) new-children))

;; returns a copy of an-ast with the data replaced with new-data
;; AST -> AST
(define (set-AST-data an-ast new-data)
  (AST (AST-type an-ast) new-data (AST-children an-ast)))
  


;; quoted-func -> listof AST
(define (get-instructions quoted-func)
  (rest (rest (rest quoted-func))))

;; AST nat -> AST
(define (ast-child ast pos)
  (list-ref (AST-children ast) pos))

;; AST -> AST
(define (get-first-child ast)
  (ast-child ast 0))

;; AST nat -> AST
(define (get-second-child ast)
  (ast-child ast 1))

;; AST -> listof AST
(define (get-token-children an-ast)
  (filter is-token-node? (AST-children an-ast)))


;; AST -> number?
(define (num-children an-ast)
  (length (AST-children an-ast)))

;; Convenience to get data from AST
;; get-nth-data: AST nat? -> datum
(define (get-nth-data an-ast n)
  (list-ref (AST-data an-ast) n))


;; Convenience to get data from AST
;; get-first-data: AST -> datum
(define (get-first-data an-ast )
  (get-nth-data an-ast 0))


;; Convenience to get data from AST
;; get-first-data: AST -> datum
(define (get-second-data an-ast )
  (get-nth-data an-ast 1))

;; Convenience to get data from AST
;; get-first-data: AST -> datum
(define (get-third-data an-ast )
  (get-nth-data an-ast 2))


;; AST -> listof symbols|nums
(define (get-token-children-data an-ast)
  (map (lambda (token-node) (get-first-data token-node)) (get-token-children an-ast)))

;; helper that gets list of positions in children nodes here varname appears
;; AST symbol -> listof nats
(define (get-token-positions an-ast token)
  (let [(index -1)]
    (filter number? (for/list ([child-data (get-token-children-data an-ast)])
                              (set! index (+ index 1))
                              (if (equal? token child-data) index #f)))))
                              

(define (get-children-types an-ast)
  (map AST-type (AST-children an-ast)))
;;;;;;;;;;;;;; CHECKERS ;;;;;;;;;;;;;;;;;;


;; Checks whether it is any of the register token nodes
;; is-reg-node?: AST -> bool
(define (is-reg-node? token-ast)
  (match (AST-type token-ast)
    [(or 'sx 'a 'w 'x) #t]
    [_ #f]))


;; Checks if the AST has any immediate children that are variable token nodes
;; With the var-name provided
;; AST symbol? -> bool
(define (has-variable? an-ast var-name)
  (cond
    [(symbol? var-name) (ormap (lambda (x) (symbol=? var-name x)) (get-token-children-data an-ast))]
    [else #f]))


;; replaces a child with provided value
;  AST NAT AST -> AST
(define (replace-child an-ast pos new-child)
  (set-AST-children! an-ast
                     (list-set (AST-children an-ast) pos new-child))
  an-ast)

;; replaces a child with provided value
;  AST NAT AST -> AST
(define (replace-type an-ast new-label)
  (set-AST-type! an-ast new-label)
  an-ast)

;; AST listof(nat) v -> AST
(define (replace-children an-ast pos-list new-child)
  (for ([pos pos-list])
       (set-AST-children! an-ast
                     (list-set (AST-children an-ast) pos new-child)))
  an-ast)


;; Checks if it has any var children
;; AST -> bool
(define (has-any-variable? an-ast)
  (ormap (lambda (child-ast) (symbol=? 'var (AST-type child-ast))) (AST-children an-ast)))


;; Checks label to see if its a token node
;; AST -> bool
(define (is-token-node? token-ast)
  (match (AST-type token-ast)
    [(or 'num 'label 'var 'sx 'a 'w' x' 's 't 'u 'cmp 'sopop 'aopop) #t]
    [_ #f]))


;; checks if an AST has the label
;; is-type-node?: AST symbol? -> bool?
(define (is-type-node? an-ast desired-label)
  (symbol=? (AST-type an-ast) desired-label))


;;; Instruction Node checkers ;;;


;; checks if AST is label node
;; is-label-node?: AST -> bool
(define (is-mems2w-node? an-ast)
  (is-type-node? an-ast 'mems2w))

;; checks if AST is label node
;; is-label-node?: AST -> bool
(define (is-memmem2w-node? an-ast)
  (is-type-node? an-ast 'memmem2w))

;; checks if AST is label node
;; is-label-node?: AST -> bool
(define (is-mems2mem-node? an-ast)
  (is-type-node? an-ast 'mems2mem))

;; checks if AST is label node
;; is-label-node?: AST -> bool
(define (is-memcmp2w-node? an-ast)
  (is-type-node? an-ast 'memcmp2w))

;; checks if AST is label node
;; is-label-node?: AST -> bool
(define (is-sopsx-node? an-ast)
  (is-type-node? an-ast 'sopsx))

;; checks if AST is label node
;; is-label-node?: AST -> bool
(define (is-sopn-node? an-ast)
  (is-type-node? an-ast 'sopn))

;; checks if AST is label node
;; is-label-node?: AST -> bool
(define (is-aop-node? an-ast)
  (is-type-node? an-ast 'aop))



;;; Token node checkers ;;;

;; checks if AST is program node
;; is-label-node?: AST -> bool
(define (is-program-node? an-ast)
  (is-type-node? an-ast 'program))

;; checks if AST is program node
;; is-label-node?: AST -> bool
(define (is-function-node? an-ast)
  (is-type-node? an-ast 'func))


;; checks if AST is label node
;; is-label-node?: AST -> bool
(define (is-label-node? an-ast)
  (is-type-node? an-ast 'label))

;; checks if AST is label node
;; is-label-node?: AST -> bool
(define (is-var-node? an-ast)
  (is-type-node? an-ast 'var))

;; checks if AST is label node
;; is-label-node?: AST -> bool
(define (is-num-node? an-ast)
  (is-type-node? an-ast 'num))

;; checks if AST is label node
;; is-label-node?: AST -> bool
(define (is-sx-node? an-ast)
  (is-type-node? an-ast 'sx))

;; checks if AST is label node
;; is-label-node?: AST -> bool
(define (is-a-node? an-ast)
  (is-type-node? an-ast 'a))

;; checks if AST is label node
;; is-label-node?: AST -> bool
(define (is-w-node? an-ast)
  (is-type-node? an-ast 'w))

;; checks if AST is label node
;; is-label-node?: AST -> bool
(define (is-x-node? an-ast)
  (is-type-node? an-ast 'x))

;; checks if AST is label node
;; is-label-node?: AST -> bool
(define (is-s-node? an-ast)
  (is-type-node? an-ast 's))

;; checks if AST is label node
;; is-label-node?: AST -> bool
(define (is-t-node? an-ast)
  (is-type-node? an-ast 't))

;; checks if AST is label node
;; is-label-node?: AST -> bool
(define (is-cmp-node? an-ast)
  (is-type-node? an-ast 'cmp))