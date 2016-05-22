#lang racket

(require "AST.rkt")
(provide to-string calculate-spillage walk-functionAST)

;; symbol|string -> string
(define (to-string token)
  (format "~a" token))



;; AST -> NAT
(define (calculate-spillage func-ast)
  (let ([arg-places (if (> (get-second-data func-ast) 6)
                        (- (get-second-data func-ast) 6)
                        0)])
    (* 8 (+ arg-places (get-third-data func-ast)))))

;; AST -> s_expression
(define (walk-functionAST func-ast instr-count)
   ;;(let ([instructions (ast-child func-ast 1)]))
	(for/list ([i (range instr-count)])
	   (match-ast (ast-child func-ast i))))

(define (match-ast i-ast)
  (let* ([i-type (AST-type i-ast)]
		 [children (AST-children i-ast)])

	(match i-type
	#|[`(,(? is-w? w) <- (mem ,(? is-x? x) ,n8))             (AST 'memmem2w '()    (list (parse-token w)   (make-mem-node (parse-token x)
																													(parse-token n8))))]|#
	['memmem2w					(let* ([dest-data (AST-data (first children))]
									   [mem-data  (AST-data (second children))]
									   [x-data    (AST-data (first  mem-data))]
									   [nat-data  (AST-data (second mem-data))])
									  `(,dest-data <- (mem ,x-data ,nat-data)))]
	#|[`((mem ,(? is-x? x) ,n8) <- ,(? is-s? s))             (AST 'mems2mem '()    (list (make-mem-node (parse-token x) (parse-token n8))
																						(parse-token s)))]|#
	['mems2mem 					(let* ([mem-data  (AST-data (first children))]
									   [rest-data (AST-data (second children))]
									   [x-data 	  (AST-data (first  mem-data))]
									   [nat-data  (AST-data (second mem-data))])
									  `((mem ,x-data ,nat-data) <- rest-data))]
	#|[`(,(? is-w? w) <- ,(? is-t? t1) ,cmp ,(? is-t? t2))   (AST 'memcmp2w '()    (parse-tokens w t1 cmp t2))]|#
	['memcmp2w					(let* ([first-data  (AST-data (first  children))]
									   [second-data (AST-data (second children))]
									   [third-data  (AST-data (third  children))]
									   [fourth-data (AST-data (fourth children))])
									  `(,first-data <- ,second-data ,third-data ,fourth-data))]
	#|[`(,(? is-w? w) <- (stack-arg ,n8))                    (AST 'memstack2w '()  (list (parse-token w) (make-stack-node (parse-token n8))))]|#
	['memstack2w                   (let* ([first-data  (AST-data (first children))]
										  [second-data (AST-data (second children))])
										`(,first-data <- (stack-arg ,second-data)))]
	;; this mem has to go last since it's the most general
	['mems2w                       (let* ([first-data  (AST-data (first children))]
										  [second-data (AST-data (second children))])
										`(,first-data <- ,second-data))]
	#|[`(,(? is-w? w) ,(? is-aop? aop) ,(? is-t? t))         (AST 'aop      '()    (parse-tokens w aop t)  )]|#
	['aop 							(let* ([w-data  (AST-data (first  children))]
										  [aop-data (AST-data (second children))]
										  [t-data   (AST-data (third  children))])
										 `(,w-data ,aop-data ,t-data))]
	#|[`(,(? is-w? w) ,(? is-sop? sop) ,(? is-sx? sx))       (AST 'sopsx    '()    (parse-tokens w sop sx) )]|#
	['sopsx							(let* ([w-data   (AST-data (first  children))]
										   [sop-data (AST-data (second children))]
										   [sx-data  (AST-data (third  children))])
										  `(,w-data ,sop-data ,sx-data))]
	#|[`(,(? is-w? w) ,(? is-sop? sop) ,(? number? n))       (AST 'sopn     '()    (parse-tokens w sop n)  )]|#
	['sopn							(let* ([w-data   (AST-data (first  children))]
										   [sop-data (AST-data (second children))]
										   [n-data	 (AST-data (third  children))])
										  `(,w-data ,sop-data ,n-data))]
	#|[(? is-label? l)                                       (parse-token l)]|#

	#|[`(goto ,(? is-label? l))                              (AST 'goto     '()    (parse-tokens l))]|#
	['goto							(let* ([label-data (AST-data children)])
										  `(goto ,label-data))]
	#|[`(cjump  ,(? is-t? t1) ,cmp ,(? is-t? t2)  ,(? is-label? l1) ,(? is-label? l2))
	(AST 'cjump    '()    (parse-tokens t1 cmp t2 l1 l2) )]|#
	['cjump 						(let* ([t-data   (AST-data (first children))]
										   [cmp-data (AST-data (second children))]
										   [t2-data  (AST-data (third  children))]
										   [label1   (AST-data (fourth children))]
										   [label2	 (AST-data (fifth  children))])
										`(cjump ,t-data ,cmp-data ,t2-data ,label1 ,label2))]
	#|[`(call print ,(? number? n))                             (if (= n 1)
																(AST 'print '() (parse-tokens 1) )
																(lambda () (error "print call with wrong arity")))]|#
	['print 						`(call print 1)]
	#|[`(call allocate ,(? number? n))                          (if (= n 2)
																(AST 'allocate '() (parse-tokens 2) )
																(lambda () (error "allocate call with wrong arity")))]|#
	['allocate						`(call allocate 2)]
	#|[`(call array-error ,(? number? n))                       (if (= n 2)
																(AST 'array-error '() (parse-tokens 2) )
																(lambda () (error "array-error call with wrong arity")))]|#
	['array-error					`(call array-error 2)]
	#|[`(call ,(? is-u? u) ,(? number? nat))                    (if (>= nat 0)
																(AST 'call  '()  (parse-tokens u nat) )
																(lambda () (error "call with negative arity")))]|#
	['call							(let* ([u-data   (AST-data (first children))]
										   [nat-data (AST-data (second children))])
										 `(call ,u-data ,nat-data))]
	#|[`(tail-call ,(? is-u? u) ,(? number? nat))               (if (and (>= nat 0) (<= nat 6))
																(AST 'tail-call '() (parse-tokens u nat) )
																(lambda () (error "tail call with invalid arity")))]|#
	['tail-call 					(let* ([u-data   (AST-data (first children))]
										   [nat-data (AST-data (second children))])
										  `(tail-call ,u-data ,nat-data))]
	#|[`(return)                                              (AST 'return '() '() )]|#
	['return						'return]
	[_ (AST-data children)])))

