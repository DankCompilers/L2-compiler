#lang racket

(require "AST.rkt" "lib.rkt" "parser.rkt")
(provide spill-function spill-instruction)


;;;;;;;;;;;;;;; Spilling ;;;;;;;;;;;;;;;;;;;;;;;;


;; used to spill all the uses of a variable
;; symbol? symbol? AST listof(sets) listof(sets) -> AST
(define (spill-function var-name prefix function-ast gens kills)
  (let* ([instructions-count (num-children function-ast)]
         [mem-location       (calculate-spillage function-ast)]
         [temp-count -1]
         #|[next-temp (lambda () (set! temp-count (+ temp-count 1))
                      (string->symbol (format "~a~a~a" prefix var-name temp-count)))]|#
         [next-temp (lambda () prefix)]
         )
    ;; returns basically a copy of the function ast with
    (AST (AST-type function-ast)
         ;; change the number of locals
         (list (get-first-data function-ast) (+ (get-second-data function-ast) 1))
         ;; spill all the instructions for new children
         (for/list ([i (range instructions-count)])
                   (spill-instruction var-name
                                      mem-location
                                      next-temp
                                      (ast-child function-ast i)
                                      (list-ref gens i)
                                      (list-ref kills i))))))



;; spills a single instruction. Must return a series of instructions
;; symbol? nat lambda AST set set -> listof AST
(define (spill-instruction var-name mem-location next-temp i-ast gen kill)
  ;; helper functions read and write the temps to memory
  (define (write-temp temp-name)
    (parse-instruction `((mem rsp ,mem-location) <- ,temp-name)))

  (define (read-temp temp-name)
    (parse-instruction `(,temp-name <- (mem rsp ,mem-location))))

  (define (spill-mem)
    (make-mem-node (parse-token 'rsp) (parse-token mem-location)))

  ;; handles the general case
  (define (handle-general-case var-read var-written temp-name temp-ast)
    (let* ([positions (get-token-positions i-ast var-name)]
           [read-ast  (if var-read (list (read-temp temp-name)) empty)]
           [replaced-ast (list (replace-children i-ast positions temp-ast))]
           [write-ast  (if var-written (list (write-temp temp-name)) empty)])
      ;;(printf "read: ~a replaced: ~a write: ~a\n" read-ast replaced-ast write-ast)
      (append read-ast replaced-ast write-ast)))

  ;; big switch to handle cases
  (let* ([i-type (AST-type i-ast)]
         #|[var-written (equal? var-name kill)]
          [var-read    (equal? var-name gen )]|#
          [var-written (set-member? kill var-name)]
          [var-read    (set-member? gen var-name)]
          [need-temp   (and (not (symbol=? i-type 'mems2w)) (or var-read var-written))]
                      ;(or (and var-read (not (symbol=? 'mems2w i-type)))
                       ;   (and var-written (or (symbol=? (symbol=? 'memmem2w i-type)))]
         ;; if the var is used, needs to be read into temp
          [temp-name   (if need-temp (next-temp) #f)]
          [temp-ast    (if need-temp (parse-token temp-name) #f)])
    ;;(raise 'failed #t)
    ;;(printf "written?: ~a read?: ~a needed?: ~a temp: ~a temp-ast: ~a\n" var-written var-read need-temp temp-name temp-ast)
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
          [(or 'goto 'return 'print 'allocate 'array-error)  i-ast]
         ;; is not a recognized AST type
          [_            (lambda () (error "spill: could not understand ~a" i-ast))]))))
