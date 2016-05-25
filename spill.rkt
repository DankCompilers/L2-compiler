#lang racket

(require "AST.rkt" "lib.rkt" "parser.rkt")
(provide spill-function spill-instruction)


;;;;;;;;;;;;;;; Spilling ;;;;;;;;;;;;;;;;;;;;;;;;


;; used to spill all the uses of a variable
;; symbol? symbol? AST listof(sets) listof(sets) -> AST
(define (spill-function var-name prefix function-ast gens kills)
  (let* ([instructions-count (num-children function-ast)]
         [mem-location       (calculate-spillage function-ast)]
         [temp-count         -1]
         [func-label         (get-first-data function-ast)]
         [func-arity         (get-second-data function-ast)]
         [func-locals        (get-third-data function-ast)]
         [next-temp (lambda ()
                      (set! temp-count (+ temp-count 1))
                      (string->symbol (format "~a~a" prefix temp-count)))]
         [ret-insts        empty])
    ;; spill all the instructions for new children
    (for ([i (range instructions-count)])
         (set! ret-insts
               (append ret-insts (spill-instruction var-name
                                                    mem-location
                                                    next-temp
                                                    (ast-child function-ast i)
                                                    (list-ref gens i)
                                                    (list-ref kills i)))))
    ;; returns basically a copy of the function ast with
    (AST (AST-type function-ast)
         ;; data list - change the number of locals
         `(,func-label ,func-arity ,(+ func-locals 1))
         ret-insts)))



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
         [var-written (set-member? kill var-name)]
         [var-read    (set-member? gen var-name)]
         [need-temp   (and (not (symbol=? i-type 'mems2w)) (or var-read var-written))]
         ;; if the var is used, needs to be read into temp
         [temp-name   (if need-temp (next-temp) #f)]
         [temp-ast    (if need-temp (parse-token temp-name) #f)])
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
          ;; must be w. Replace with temp, then write temp. mem might use variable too
          ['memmem2w      (let* ([w              (get-first-data (get-first-child i-ast))]
                                 [replace-w?     (symbol=? w var-name)]
                                 [mem-node       (get-second-child i-ast)]
                                 [mem-x          (get-first-data (get-first-child mem-node))]
                                 [replace-mem?   (symbol=? mem-x var-name)])
                                 ;; if replacing mem, need to read temp, then replace in mem node
                                 (cond
                                   [(and replace-mem?
                                         replace-w?)       `(,(read-temp temp-name)
                                                             ,(replace-child (replace-child i-ast 1 (replace-child mem-node 0 temp-ast)) 0 temp-ast)
                                                             ,(write-temp temp-name))]
                                   [replace-mem?           `(,(read-temp temp-name)
                                                             ,(replace-child i-ast 1 (replace-child mem-node 0 temp-ast)))]
                                   [replace-w?             `(,(replace-child i-ast 0 temp-ast)
                                                             ,(write-temp temp-name))]))]
          ['mems2mem       (let* ([s              (get-first-data (get-second-child i-ast))]
                                  [replace-s?     (symbol=? s var-name)]
                                  [mem-node       (get-first-child i-ast)]
                                  [mem-x          (get-first-data (get-first-child mem-node))]
                                  [replace-mem?   (symbol=? mem-x var-name)])
                                 ;; if replacing mem, need to read temp, then replace in mem node
                                 (cond
                                   [(and replace-mem?
                                         replace-s?)       `(,(read-temp temp-name)
                                                             ,(replace-child (replace-child i-ast 0 (replace-child mem-node 0 temp-ast)) 1 temp-ast))]
                                   [replace-mem?           `(,(read-temp temp-name)
                                                             ,(replace-child i-ast 0 (replace-child mem-node 0 temp-ast)))]
                                   [replace-s?             `(,(read-temp temp-name)
                                                             ,(replace-child i-ast 1 temp-ast))]))]


         ;  (list (read-temp temp-name)
          ;                   (replace-child i-ast 1 temp-ast))]
          ;; must be write to w. have to write
          ['memstack2w (list (replace-child i-ast 0 temp-ast)
                             (write-temp temp-name))]
          ;; All of these follow the same rules
          [(or 'cjump 'aop 'sopsx 'sopn 'memcmp2w 'call 'tail-call)   (handle-general-case var-read var-written temp-name temp-ast)]
          ;; should never be done because can't use variables
          [(or 'goto 'return 'print 'allocate 'array-error)  i-ast]
          ;; is not a recognized AST type
          [_            (lambda () (error "spill: could not understand ~a" i-ast))]))))
