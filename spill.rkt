#lang racket

(require "AST.rkt" "lib.rkt" "parser.rkt")


(define print-debug #f)

(define (debug-printer proc . args)
  (when print-debug (printf "given:    ~a\nexpected: ~a\n\n" (first args) (last args)))
  (apply proc args))


;;;;;;;;;;;;;;; Spilling ;;;;;;;;;;;;;;;;;;;;;;;;


;; used to spill all the uses of a variable
;; symbol? symbol? AST listof(sets) listof(sets) -> AST
(define (spill-function var-name prefix function-ast gens kills)
  (let* ([instructions-count (num-children function-ast)]
         [mem-location       (calculate-spillage function-ast)]
         [temp-count -1]
         [next-temp (lambda () (set! temp-count (+ temp-count 1))
                      (string->symbol (format "~a~a" prefix temp-count)))])
    ;; returns basically a copy of the function ast with
    (AST (AST-type function-ast)
         ;; change the number of locals
         (list (get-first-data function-ast) (+ (get-second-data) 1))
         ;; spill all the instructions for new children
         (for/list ([i (range instructions-count)])
                   (spill-instruction var-name
                                      mem-location
                                      next-temp
                                      (ast-child function-ast i)
                                      (list-ref gens i)
                                      (list-ref kills i)))
        )))



;; spills a single instruction. Must return a series of instructions
;; symbol? nat lambda AST set set -> listof AST
(define (spill-instruction var-name mem-location next-temp i-ast gen kill)
  ;; helper functions read and write the temps to memory
  (define (write-temp temp-name)
    (parse-instruction `((mem rsp ,mem-location) <- ,temp-name)))
  
  (define (read-temp temp-name)
    (parse-instruction `(,temp-name <- (mem rsp ,mem-location))))
  
  (define (spill-mem)
    (make-mem-node 'rsp mem-location))  

  ;; handles the general case
  (define (handle-general-case var-read var-written temp-name temp-ast)
    (let* ([positions (get-token-positions i-ast var-name)]
           [read-ast  (if var-read (list (read-temp temp-name)) empty)]
           [replaced-ast (list (replace-children i-ast positions temp-ast))]
           [write-ast  (if var-written (list (write-temp temp-name)) empty)])
      (printf "read: ~a replaced: ~a write: ~a\n" read-ast replaced-ast write-ast)
      (append read-ast replaced-ast write-ast)))

  ;; big switch to handle cases
  (let* ([i-type (AST-type i-ast)]
         [var-written (set-member? kill var-name)]
         [var-read    (set-member? gen var-name)]
         [need-temp   (and (not (symbol=? i-type 'mems2w)) (or var-read var-written))] 
                      ;(or (and var-read (not (symbol=? 'mems2w i-type)))
                       ;   (and var-written (or (symbol=? (symbol=? 'memmem2w i-type)))]
         ;; if the var is used, needs to be read into temp
         [temp-name   (if need-temp (next-temp) #f)]
         [temp-ast    (if need-temp (parse-token temp-name) #f)])
    (printf "written?: ~a read?: ~a needed?: ~a temp: ~a temp-ast: ~a\n" var-written var-read need-temp temp-name temp-ast)
    ;; if not in the gen or kill, just return the original instruction
    (if (not (or  var-written var-read))
        (list i-ast)
      (match i-type
         ['mems2w    (cond  [(and var-written var-read)  empty] ;; get rid of instruction, pointless
                            [var-written                 (list (replace-type (replace-child i-ast 0 (spill-mem))
                                                                             'mems2mem))]
                            [var-read                    (list (replace-type (replace-child i-ast 1 (spill-mem))
                                                                             'memmem2w))])]
         ;; must be w. Replace with temp, then write temp
         ['memmem2w  (list  (replace-child i-ast 0 temp-ast)
                            (write-temp temp-name))]  
         ;; must be s, read to temp, replace second child
         ['mems2mem   (list (read-temp temp-name)
                            (replace-child i-ast 1 temp-ast))]
         ;; All of these follow the same rules
         [(or 'cjump 'aop 'sopsx 'sopn 'memcmp2w 'call 'tail-call)   (handle-general-case var-read var-written temp-name temp-ast)]
         ;; should never be done because can't use variables
         [(or 'goto 'return 'print 'allocate 'array-error)   (list i-ast)]
         ;; is not a recognized AST type
         [_            (lambda () (error "spill: could not understand ~a" i-ast))]))))



(module+ test
  (require rackunit)
  (require "liveness.rkt")

  ;; used to generate unique temps
  (define current-one -1)
  (define (next-temp)
    (set! current-one (+ current-one 1))
    (string->symbol (format "s~a" current-one)))
    
  (define (test-spill-instruction quoted-inst var-name quoted-expected)
    (let* ([i-ast    (parse-instruction quoted-inst)]
           [gen-kill (generate-gen-kill i-ast)])
    (debug-printer check-equal? (spill-instruction var-name 8 next-temp i-ast (first gen-kill) (second gen-kill))
                   (map parse-instruction quoted-expected))))

  ;; test assignments
  ;; test mems2w
  (test-spill-instruction `(a <- 1) 'a `(((mem rsp 8) <- 1)))
  (test-spill-instruction `(rdi <- 1) 'a `((rdi <- 1)))
  (test-spill-instruction `(rdi <- b) 'b `((rdi <- (mem rsp 8))))
  (test-spill-instruction `(a <- b)   'a `(((mem rsp 8) <- b)))

  ;; test mems2mem
  (test-spill-instruction `((mem rsp 0) <- 1) 'a `(((mem rsp 0) <- 1)))
  (test-spill-instruction `((mem rsp 0) <- a) 'a `((s0 <- (mem rsp 8))
                                                   ((mem rsp 0) <- s0)))

  ;; test memmem2w
  (test-spill-instruction `(a <- (mem rsp 0)) 'a `((s1 <- (mem rsp 0))
                                                   ((mem rsp 8) <- s1)))
  (test-spill-instruction `(rdi <- (mem rsp 0)) 'a `((rdi <- (mem rsp 0))))

  ;; test aop
  (test-spill-instruction `(a += a) 'a `((s2 <- (mem rsp 8))
                                         (s2 += s2)
                                         ((mem rsp 8) <- s2)))
  (test-spill-instruction `(a -= 1) 'a `((s3 <- (mem rsp 8))
                                         (s3 -= 1)
                                         ((mem rsp 8) <- s3)))
  (test-spill-instruction `(rax *= raxor) 'raxor `((s4 <- (mem rsp 8))
                                         (rax *= s4)))


  ; test sopsx/sopn
  (test-spill-instruction `(rax <<= avar) 'avar `((s5 <- (mem rsp 8))
                                                  (rax <<= s5)))
  (test-spill-instruction `(rax <<= 5) 'avar `((rax <<= 5)))
  (test-spill-instruction `(blah <<= avar) 'blah `((s6 <- (mem rsp 8))
                                                  (s6 <<= avar)
                                                  ((mem rsp 8) <- s6)))
  (test-spill-instruction `(rahhhh <<= 5) 'rahhhh `((s7 <- (mem rsp 8))
                                                    (s7 <<= 5)
                                                    ((mem rsp 8) <- s7)))

  ;; test cjump
  (test-spill-instruction `(cjump mine < 5 :live :die) 'mine `((s8 <- (mem rsp 8))
                                                               (cjump s8 < 5 :live :die)))
  (test-spill-instruction `(cjump mine = mine :live :die) 'mine `((s9 <- (mem rsp 8))
                                                               (cjump s9 = s9 :live :die)))
  (test-spill-instruction `(cjump 10 < mine :live :die) 'mine `((s10 <- (mem rsp 8))
                                                               (cjump 10 < s10 :live :die)))

  ;; test memcmp2w
  (test-spill-instruction `(avar <- mine < 5) 'mine `((s11 <- (mem rsp 8))
                                                      (avar <- s11 < 5)))
  (test-spill-instruction `(avar <- mine < 5) 'avar `((s12 <- mine < 5)
                                                      ((mem rsp 8) <- s12)))
  (test-spill-instruction `(avar <- mine < mine) 'mine `((s13 <- (mem rsp 8))
                                                        (avar <- s13 < s13)))
  (test-spill-instruction `(mine <- mine < mine) 'mine `((s14 <- (mem rsp 8))
                                                        (s14 <- s14 < s14)
                                                        ((mem rsp 8) <- s14)))
  
  ;; test call and tail call                       
  (test-spill-instruction `(call raxor 2) 'raxor `((s15 <- (mem rsp 8))
                                                   (call s15 2)))
  (test-spill-instruction `(call :alabel 2) 'raxor `((call :alabel 2)))
  (test-spill-instruction `(tail-call raxor 2) 'raxor `((s16 <- (mem rsp 8))
                                                   (tail-call s16 2)))
  (test-spill-instruction `(tail-call :alabel 2) 'raxor `((tail-call :alabel 2)))
  
  ;; test simple ones
  ;(test-spill-instruction `(goto :label)  'a  `((goto :label)))
  ;(test-spill-instruction `(call print 1) 'print  `((call print 1)))
  ;(test-spill-instruction `(call allocate 2) 'allocate  `((call allocate 2)))
  ;(test-spill-instruction `(call array-error 2) 'p  `((call array-error 2)))
  ;(test-spill-instruction `(return) 'return  `((return)))
  )
