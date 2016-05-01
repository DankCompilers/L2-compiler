#lang racket

(require "AST.rkt" "lib.rkt")
(provide parse-program parse-function parse-instruction parse-token parse-tokens)

(define print-debug #f)


;;(module grammar racket
;; (provide (all-defined-out))
(define (negate-pred pred) (lambda (x) (not (pred x))))
(define num-only (list 2 5 2 1 592304))
(define var-only `(parax barax raxor rdimine ardi raxrdi hello s0 whatis324))
(define sx-only `(rcx))
(define a-only `(rdi rsi rdx r9 r9))
(define w-only `(rax rbx rbp r10 r11 r12 r13 r14 r15))
(define x-only `(rsp));)
(define aop-only `(+= -= *= &=))
(define sop-only `(>>= <<=))
(define cmp-only `(< <= =))

;;;;;;;;;;; REGEX DECLARATIONS ;;;;;;;;;;;;;;;;;;

(define label-str "^:[a-zA-Z_][a-zA-Z_0-9]*")
(define var-str "^[a-zA-Z_][a-zA-Z_0-9]*")
(define num-str "\\b[1-9][0-9]*\\b")
(define a-str-only "\\b(?:rdi|rsi|rdx|r8|r9)\\b")
(define w-str-only "\\b(?:rax|rbx|rbp|r10|r11|r12|r13|r14|r15)\\b")
(define x-str-only "\\brsp\\b")
(define sx-str-only "\\brcx\\b")
(define sx-str (string-join (list sx-str-only var-str) "|"))
;;(define sx-str sx-str-only)
(define a-str (string-join (list sx-str a-str-only) "|"))
(define w-str (string-join (list a-str w-str-only) "|"))
(define x-str (string-join (list w-str x-str-only) "|"))
(define s-str (string-join (list x-str num-str label-str) "|"))
(define t-str (string-join (list x-str num-str) "|"))
(define u-str (string-join (list w-str label-str) "|"))
(define cmp-str "\\B(?:<=|<|=)\\B")
(define sop-str "\\B(?:<<=|>>=)\\B")
(define aop-str "\\B[-+*&]=\\B")


(define label-regexp   (pregexp label-str))
(define var-regexp     (pregexp var-str))
(define sx-regexp      (pregexp sx-str))
(define sx-regexp-only (pregexp sx-str-only))
(define a-regexp       (pregexp a-str))
(define a-regexp-only  (pregexp a-str-only))
(define w-regexp       (pregexp w-str))
(define w-regexp-only  (pregexp w-str-only))
(define x-regexp       (pregexp x-str))
(define x-regexp-only  (pregexp x-str-only))
(define s-regexp       (pregexp s-str))
(define t-regexp       (pregexp t-str))
(define u-regexp       (pregexp u-str))
(define cmp-regexp     (pregexp cmp-str))
(define sop-regexp     (pregexp sop-str))
(define aop-regexp     (pregexp aop-str))



;;;;;;;;;;; PARSING PROCEDURES DEFINITIONS ;;;;;;;;;;;;;;;;;;


;; Takes in a complete program and returns an AST
;; parse-program: string -> AST
(define (parse-program raw-program)
  (let* ([quoted-program `raw-program]
         [program-name (first quoted-program)]
         [ast-root (AST 'root '(,program-name) empty)])
    (if  (not (symbol? program-name)) (error "Main procedure name unspecified")
         (set-AST-children! ast-root
                            (for/list ([func (rest quoted-program)])
                                      (parse-function func))))))


;; Takes in a complete function and returns an AST
;; parse-function: quoted -> AST
(define (parse-function quoted-function)
  (let  ([func-name (first quoted-function)]
         [arity (second quoted-function)]
         [local-num (third quoted-function)])
    (cond
      [(not (is-label? func-name))  (lambda () (error (format "Function: Found ~a instead of label for function" func-name)))]
      [(not (number? arity))        (lambda () (error (format "Function ~a invalue arity provided" func-name)))]
      [(not (number? local-num))    (lambda () (error (format "Function ~a invalue arity provided" func-name)))]
      [else (AST 'func '(func-name arity local-num) 
                 (for/list ([instruction (get-instructions quoted-function)])
                           (parse-instruction instruction)))])))


;; Takes in a instruction and returns an AST
;; parse-instruction: quoted -> AST
(define (parse-instruction quoted-instruction)
    (match quoted-instruction
      [`(,(? is-w? w) <- (mem ,(? is-x? x) ,n8))             (AST 'memmem2w '()    (list (parse-token w)   (make-mem-node x n8)) )]
      [`((mem ,(? is-x? x) ,n8) <- ,(? is-s? s))             (AST 'mems2mem '()    (list (make-mem-node x n8) (parse-token s)) )]
      [`(,(? is-w? w) <- ,(? is-t? t1) ,cmp ,(? is-t? t2))   (AST 'memcmp2w '()    (parse-tokens w cmp t1 t2) )]
      ;; this mem has to go last since it's the most general
      [`(,(? is-w? w) <- ,(? is-s? s))                       (AST 'mems2w   '()    (parse-tokens w s) )]
      [`(,(? is-w? w) ,(? is-aop? aop) ,(? is-t? t))         (AST 'aop      (list aop) (parse-tokens w t)  )]
      [`(,(? is-w? w) ,(? is-sop? sop) ,(? is-sx? sx))       (AST 'sopsx    (list sop) (parse-tokens w sx) )]
      [`(,(? is-w? w) ,(? is-sop? sop) ,(? number? n))       (AST 'sopn     (list sop) (parse-tokens w n)  )]
      [(? is-label? l)                                       (parse-token l)]
      [`(goto ,(? is-label? l))                              (AST 'goto (list l) (parse-tokens l) )]
      [`(cjump  ,(? is-t? t1) ,cmp ,(? is-t? t2)  ,(? is-label? l1) ,(? is-label? l2))
                                                             (AST 'cjump (list l1 l2) (parse-tokens cmp t1 t2 l1 l2) )]
      [`(call print ,(? number? n))                             (if (= n 1)
                                                                 (AST 'print '() (parse-tokens 1) )
                                                                 (lambda () (error "print call with wrong arity")))]
      [`(call allocate ,(? number? n))                          (if (= n 2)
                                                                 (AST 'allocate '() (parse-tokens 2) )
                                                                 (lambda () (error "allocate call with wrong arity")))]
      [`(call array-error ,(? number? n))                       (if (= n 2)
                                                                 (AST 'array-error '() (parse-tokens 2) )
                                                                 (lambda () (error "array-error call with wrong arity")))]
      [`(call ,(? is-u? u) ,(? number? nat))                    (if (>= nat 0)
                                                                 (AST 'call  '()  (parse-tokens u nat) )
                                                                 (lambda () (error "call with negative arity")))]
      [`(tail-call ,(? is-u? u) ,(? number? nat))               (if (and (>= nat 0) (<= nat 6))
                                                                 (AST 'tail-call '() (parse-tokens u nat) )
                                                                 (lambda () (error "tail call with invalid arity")))]
      [`(return)                                             (AST 'return '() '() )]
      [_ (lambda () (error "instruction did not match any cases"))]))



;; Instruction parsing tests
(module+ test
  (require rackunit)
  (define (check-equal-print v1 v2)
    (when print-debug (printf "given:    ~a\nexpected: ~a\n\n" v1 v2))
    (check-equal? v1 v2))

  ;;aop tests
  (check-equal-print (parse-instruction `(rdi += 43))          (AST 'aop (list '+=) (parse-tokens 'rdi 43)))
  (check-equal-print (parse-instruction `(raxor -= 43))        (AST 'aop (list '-=) (parse-tokens 'raxor 43)))
  (check-equal-print (parse-instruction `(rax *= raxor))       (AST 'aop (list '*=) (parse-tokens 'rax 'raxor)))
  (check-equal-print (parse-instruction `(raxor &= rdimate))   (AST 'aop (list '&=) (parse-tokens 'raxor 'rdimate)))
  
  (printf "Done with aop tests\n\n")
  
  ;;sop tests
  (check-equal-print (parse-instruction `(rdi <<= 43))      (AST 'sopn  (list '<<=) (parse-tokens 'rdi 43)))
  (check-equal-print (parse-instruction `(raxor <<= 10))    (AST 'sopn  (list '<<=) (parse-tokens 'raxor 10)))
  (check-equal-print (parse-instruction `(rax >>= rcx))     (AST 'sopsx (list '>>=) (parse-tokens 'rax 'rcx)))
  (check-equal-print (parse-instruction `(raxor >>= rcx))   (AST 'sopsx (list '>>=) (parse-tokens 'raxor 'rcx)))
  (check-equal-print (parse-instruction `(raxor >>= raxor)) (AST 'sopsx (list '>>=) (parse-tokens 'raxor 'raxor)))

  (printf "Done with sop tests\n\n")
  
  ;; assignment tests
  ;; mems2w
  (check-equal-print (parse-instruction `(rdi <- 43))       (AST 'mems2w empty (parse-tokens 'rdi 43)))
  (check-equal-print (parse-instruction `(rdi <- :hello))   (AST 'mems2w empty (parse-tokens 'rdi ':hello)))
  (check-equal-print (parse-instruction `(rax <- 1000))     (AST 'mems2w empty (parse-tokens 'rax 1000)))
  (check-equal-print (parse-instruction `(rdi <- rax))      (AST 'mems2w empty (parse-tokens 'rdi 'rax)))
  (check-equal-print (parse-instruction `(v1  <- v2))       (AST 'mems2w empty (parse-tokens 'v1 'v2)))
  (check-equal-print (parse-instruction `(v1  <- rax))      (AST 'mems2w empty (parse-tokens 'v1 'rax)))
  (check-equal-print (parse-instruction `(rcx  <- v2))      (AST 'mems2w empty (parse-tokens 'rcx 'v2)))

  (printf "Done with mems2w tests\n\n")
  
  ;; memmem2w
  (check-equal-print (parse-instruction `(rdi <- (mem rsp 0)))   (AST 'memmem2w empty (list (parse-token 'rdi)
                                                                                            (make-mem-node 'rsp 0))))
  
  (check-equal-print (parse-instruction `(avar <- (mem rsp -8)))  (AST 'memmem2w empty (list (parse-token 'avar)
                                                                                            (make-mem-node 'rsp -8 ))))

  (check-equal-print (parse-instruction `(rcx <- (mem rsp -8)))  (AST 'memmem2w empty (list (parse-token 'rcx)
                                                                                            (make-mem-node 'rsp -8))))

  (printf "Done with memmem2w tests\n\n")
  ;; mems2mem
  (check-equal-print (parse-instruction `((mem rsp -8) <- rcx))  (AST 'mems2mem empty (list  (make-mem-node 'rsp -8)
                                                                                             (parse-token 'rcx))))

  (check-equal-print (parse-instruction `((mem rsp -8) <- :hello))  (AST 'mems2mem empty (list  (make-mem-node 'rsp -8)
                                                                                             (parse-token ':hello))))

  (check-equal-print (parse-instruction `((mem rsp -8) <- raxor))  (AST 'mems2mem empty (list  (make-mem-node 'rsp -8)
                                                                                             (parse-token 'raxor))))


  (printf "Done with mems2mem tests\n\n")

  ;; memcmp2w
  (check-equal-print (parse-instruction `(rcx <- rdi <= 5))        (AST 'memcmp2w empty  (parse-tokens 'rcx '<= 'rdi 5)))

  (check-equal-print (parse-instruction `(rax <- rax < raxor))     (AST 'memcmp2w empty  (parse-tokens 'rax '< 'rax 'raxor)))

  (check-equal-print (parse-instruction `(raxor <- raxor = raxor))  (AST 'memcmp2w empty  (parse-tokens 'raxor '= 'raxor 'raxor)))


  (printf "Done with memcmp2w tests\n\n")
  
  ;; simple instructions
  (check-equal-print (parse-instruction ':hello) (parse-token ':hello))
  (check-equal-print (parse-instruction `(goto :hello)) (AST 'goto (list ':hello) (parse-tokens  ':hello)))
  (check-equal-print (parse-instruction `(call array-error 2)) (AST 'array-error empty (parse-tokens 2)))
  (check-equal-print (parse-instruction `(call allocate 2)) (AST 'allocate empty (parse-tokens 2)))
  (check-equal-print (parse-instruction `(call print 1)) (AST 'print empty (parse-tokens 1)))
  (check-equal-print (parse-instruction `(tail-call :hi 1)) (AST 'tail-call empty (parse-tokens ':hi 1)))
  (check-equal-print (parse-instruction `(return)) (AST 'return empty empty))


  ;; Check failure detection
  (check-exn    exn:fail?  (parse-instruction 'hello))
  (check-exn    exn:fail?  (parse-instruction `(call :tester -1)))
  (check-exn    exn:fail?  (parse-instruction `(call array-error 1)))
  (check-exn    exn:fail?  (parse-instruction `(call allocate 1)))
  (check-exn    exn:fail?  (parse-instruction `(call print 2)))
  (check-exn    exn:fail?  (parse-instruction `()))
  (check-exn    exn:fail?  (parse-instruction `(345324 543543 5435)))
  (displayln "Instruction parsing end"))



;; Takes in a token returns an AST
;; symbol/num -> AST
(define (parse-token token)
  (match token
    [(? number?)           (make-token-node 'num token)]
    [(? is-label?)         (make-token-node 'label token)]
    [(? is-cmp?)           (make-token-node 'cmp token)]
    [(? is-sx-only?)       (make-token-node 'sx token)]
    [(? is-a-only?)        (make-token-node 'a token)]
    [(? is-w-only?)        (make-token-node 'w token)]
    [(? is-x-only?)        (make-token-node 'x token)]
    [(? is-var?)           (make-token-node 'var token)]
    [(? is-u?)             (make-token-node 's token)]
    [(? is-t?)             (make-token-node 't token)]
    [(? is-s?)             (make-token-node 'u token)]))


;; Helper function to parse many tokens at once
;; listof? symbols -> listof? AST
(define (parse-tokens . tokens)
  (map parse-token tokens))


;; token parsing tests
(module+ test
  (require rackunit)
  (define (validate-token-node type-check expected-data an-ast)
    (check-pred type-check an-ast)
    (check-equal? expected-data (get-first-data an-ast)))
  
  (for ([i var-only])
       (validate-token-node is-var-node? i (parse-token i)))
  
  (for ([i sx-only])
       (validate-token-node is-sx-node? i (parse-token i)))
  
  (for ([i a-only])
       (validate-token-node is-a-node? i (parse-token i)))

  (for ([i w-only])
       (validate-token-node is-w-node? i (parse-token i)))
  
  (for ([i x-only])
       (validate-token-node is-x-node? i (parse-token i)))
  
  (for ([i num-only])
       (validate-token-node is-num-node? i (parse-token i)))
  (displayln "Token parsing tests end"))


;;;;;;;;;;;;; TYPE CHECKER HELPERS ;;;;;;;;;;;;;;;;;;;;;;

;; p/regexp token -> bool
(define (match-token? reg raw-token)
  (regexp-match? reg (to-string raw-token)))

;; token -> bool
(define (is-sx? raw-token)
  (match-token? sx-regexp raw-token))

;; token -> bool
(define (is-sx-only? raw-token)
  (match-token? sx-regexp-only raw-token))

;; token -> bool
(define (is-a? raw-token)
  (match-token? a-regexp raw-token))

;; token -> bool
(define (is-a-only? raw-token)
  (match-token? a-regexp-only raw-token))

;; token -> bool
(define (is-w? raw-token)
  (match-token? w-regexp raw-token))

;; token -> bool
(define (is-w-only? raw-token)
  (match-token? w-regexp-only raw-token))

;; token -> bool
(define (is-x? raw-token)
  (match-token? x-regexp raw-token))

;; token -> bool
(define (is-x-only? raw-token)
  (match-token? x-regexp-only raw-token))

;; token -> bool
(define (is-s? raw-token)
  (match-token? s-regexp raw-token))

;; token -> bool
(define (is-t? raw-token)
  (match-token? t-regexp raw-token))

;; token -> bool
(define (is-u? raw-token)
  (match-token? u-regexp raw-token))

;; token -> bool
(define (is-label? raw-token)
  (match-token? label-regexp raw-token))

;; token -> bool
(define (is-var? raw-token)
  (match-token? var-regexp raw-token))

;; token -> bool
(define (is-aop? raw-token)
  (match-token? aop-regexp raw-token))

;; token -> bool
(define (is-sop? raw-token)
  (match-token? sop-regexp raw-token))

;; token -> bool
(define (is-cmp? raw-token)
  (match-token? cmp-regexp raw-token))


;; type checker tests
(module+ test
  (require rackunit)

  (for ([i aop-only])
       (check-pred is-aop? i))

  (for ([i sop-only])
       (check-pred is-sop? i))

  (for ([i cmp-only])
       (check-pred is-cmp? i))
  
  ;; tests everything that should recognize vars
  (for ([i var-only])
       (check-pred is-var? i)
       (check-pred is-sx? i)
       (check-pred is-a? i)
       (check-pred is-w? i)
       (check-pred is-x? i)
       (check-pred is-s? i))

  ;; tests everything that should recognize sx
  (for ([i sx-only])
       (check-pred is-sx? i)
       (check-pred is-a? i)
       (check-pred is-w? i)
       (check-pred is-x? i)
       (check-pred is-s? i))

  ;; tests for a
  (for ([i a-only])
       (check-pred is-a? i)
       (check-pred is-w? i)
       (check-pred is-x? i)
       (check-pred is-s? i))

  ;; tests for w
  (for ([i w-only])
       (check-pred is-w? i)
       (check-pred is-x? i)
       (check-pred is-s? i))

  ;; tests for x
  (for ([i x-only])
       (check-pred is-w? i)
       (check-pred is-x? i)
       (check-pred is-s? i))
  (displayln "Type checker helpers tests end"))


