;;;;;;;;;;;;;;;;;;;
;; Exercise 4.25 ;;
;;;;;;;;;;;;;;;;;;;

;; with applicative-order, if we attempt to evaluate (factorial 5),
;; it will hang forever, because on every application of unless the
;; arguments will be evaluated, and therefore (factorial (- n 1))
;; will continue running forever

;; with normal-order the combination (* n (factorial (- n 1))) will
;; not be evaluated when (= n 1), so the definition will work

;;;;;;;;;;;;;;;;;;;
;; Exercise 4.26 ;;
;;;;;;;;;;;;;;;;;;;

;; (unless (> age 18)
;;   (error "Only adult people can drive")
;;   'drive)
;; unless as syntax
;; (define (unless->if exp)
;;   (list 'if
;; 	(list 'not (if-predicate exp))
;; 	(if-consequent exp)
;; 	(if-alternative exp)))

;; unless as normal procedure
;;(map unless conditions errors success)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 4.2.2 An Interpreter with Lazy Evaluation ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EVAL
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-variable-value exp env))
	((quoted? exp) (text-of-quotation exp))
	((assignment? exp) (eval-assignment exp env))
	((definition? exp) (eval-definition exp env))
	((if? exp) (eval-if exp env))
	((lambda? exp)
	 (make-procedure (lambda-parameters exp)
			 (lambda-body exp)
			 env))
	((begin? exp)
	 (eval-sequence (begin-actions exp) env))
	((cond? exp) (eval (cond->if exp) env))
	;;((let? exp) (eval (let->combination1 exp) env))
	((application? exp)
	 (apply (actual-value (operator exp) env)
		(operands exp)
		env))
	(else
	 (error "Unknown expression type -- EVAL" exp))))

(define (actual-value exp env)
  (force-it (eval exp env)))

;; the next definition of apply will shadow the underlying
;; lisp apply, so we save it here as an alias
(define apply-in-underlying-scheme apply)

;; APPLY
(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
	 (apply-primitive-procedure
	  procedure
	  (list-of-arg-values arguments env)))
	((compound-procedure? procedure)
	 (eval-sequence
	  (procedure-body procedure)
	  (extend-environment (procedure-parameters procedure)
			      (list-of-delayed-args arguments env)
			      (procedure-environment procedure))))
	(else (error
	       "Unknown procedure type -- APPLY" procedure))))

;; Representing Thunks
(define (force-it obj) ;; with memoization
  (cond ((thunk? obj)
	 (let ((result (actual-value
			(thunk-exp obj)
			(thunk-env obj))))
	   (set-car! obj 'evaluated-thunk)
	   (set-car! (cdr obj) result) ; replace exp with its value
	   (set-cdr! (cdr obj) '()) ; forget unneeded env
	   result))
	((evaluated-thunk? obj)
	 (thunk-value obj))
	(else obj)))

(define (force-it obj) ;; without memoization
  (if (thunk? obj)
      (actual-value (thunk-exp obj)
		    (thunk-env obj))
      obj))

(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk)
  (cadr evaluated-thunk))

;; PROCEDURE ARGUMENTS (with lazy evaluation)
(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
	    (list-of-arg-values (rest-operands exps) env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps) env)
	    (list-of-delayed-args (rest-operands exps) env))))

;; CONDITIONALS (with lazy evaluation)
(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;; SEQUENCES
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
	(else (eval (first-exp exps) env)
	      (eval-sequence (rest-exps exps) env))))

;; ASSIGNMENTS AND DEFINITIONS
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
		       (eval (assignment-value exp) env)
		       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

;; REPRESENTING EXPRESSIONS
(define (self-evaluating? exp)
  (cond ((number? exp) #t)
	((string? exp) #t)
	(else #f)))

(define (variable? exp)
  (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) ;; formal parameters
		   (cddr exp)))) ;; body

(define (lambda? exp)
  (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp)
  (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp)
  (tagged-list? exp 'begin))
(define (begin-actions exp)
  (cdr exp))
(define (last-exp? seq)
  (null? (cdr seq)))
(define (first-exp seq)
  (car seq))
(define (rest-exps seq)
  (cdr seq))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
	((last-exp? seq) (first-exp seq))
	(else (make-begin seq))))
(define (make-begin seq)
  (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp)
  (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause)
  (car clause))
(define (cond-actions clause)
  (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
	    (rest (cdr clauses)))
	(if (cond-else-clause? first)
	    (if (null? rest)
		(sequence->exp (cond-actions first))
		(error "ELSE clause isn't last -- COND->IF"
		       clauses))
	    (make-if (cond-predicate first)
		     (sequence->exp (cond-actions first))
		     (expand-clauses rest))))))

;; EVALUATOR DATA STRUCTURES
;; testing of predicates
(define (true? x)
  (not (eq? x #f)))

(define (false? x)
  (eq? x #f))

;; respresenting procedures
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))
;; (define (procedure-body p)
;;   (scan-out-defines (caddr p)))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

;; operations on environments
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     (env-loop (enclosing-environment env)))
	    ((eq? var (car vars))
	     (car vals))
	    (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable" var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame)
		(frame-values frame)))))
  (env-loop env))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
	  (error "Too many arguments supplied" vars vals)
	  (error "Too few arguments supplied" vars vals))))

(define (define-variable! var value env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
	     (add-binding-to-frame! var value frame))
	    ((eq? var (car vars))
	     (set-car! vals value))
	    (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
	  (frame-values frame))))

(define (set-variable-value! var value env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     (env-loop (enclosing-environment env)))
	    ((eq? var (car vars))
	     (set-car! vals value))
	    (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable -- SET!" var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame)
		(frame-values frame)))))
  (env-loop env))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame)
  (car frame))
(define (frame-values frame)
  (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

;; RUNNING THE EVALUATOR AS A PROGRAM

(define (setup-environment)
  (let ((initial-env
	 (extend-environment (primitive-procedure-names)
			     (primitive-procedure-objects)
			     the-empty-environment)))
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define (my-map f elts)
  (cond ((null? elts) '())
	(else
	 (cons (f (car elts))
	       (my-map f (cdr elts))))))

(define primitive-procedures
  (list (list 'car car)
	(list 'cdr cdr)
	(list 'cons cons)
	(list 'null? null?)
	(list 'square (lambda (x) (* x x)))
	(list 'map my-map)
	(list '+ +)
	(list '= =)
	(list '* *)
	(list '- -)
	(list 'first car)))

(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))
  

(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (actual-value input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
		     (procedure-parameters object)
		     (procedure-body object)
		     '<procedure-env>))
      (display object)))

(define the-global-environment (setup-environment))

;;;;;;;;;;;;;;;;;;;
;; Exercise 4.27 ;;
;;;;;;;;;;;;;;;;;;;

;; count => 1
;; w => 10
;; count => 2

;;;;;;;;;;;;;;;;;;;
;; Exercise 4.28 ;;
;;;;;;;;;;;;;;;;;;;

(define (something f)
  (f 3 4))

;;;;;;;;;;;;;;;;;;;
;; Exercise 4.29 ;;
;;;;;;;;;;;;;;;;;;;

(define (do-something)
  (display-and-return (super-long-running-command 10)))

(define (display-and-return long-running-command-result)
  (display "result:")
  (display long-running-command-result)
  long-running-command-result)

;; (square (id 10))
;; 100
;; count
;; 1 (with memo)
;; 2 (without memo)

