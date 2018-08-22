;;;;;;;;;;;;;;;;;;
;; Exercise 4.1 ;;
;;;;;;;;;;;;;;;;;;

;; left to right
(define (list-of-values-ltor exps env)
  (if (no-operands? exps)
      '()
      (let ((first (eval (first-operand exps) env)))
	(cons first
	      (list-of-values-ltor (rest-operands exps) env)))))
;; right to left
(define (list-of-values-rtol exps env)
  (if (no-operands? exps)
      '()
      (let ((rest (list-of-values-rtol (rest-operands exps) env)))
	(cons (eval (first-operand exps) env)
	      rest))))

;;;;;;;;;;;;;;;;;;
;; Exercise 4.2 ;;
;;;;;;;;;;;;;;;;;;

;; a) it will evaluate the operands first '(x 3) and can raise an
;; error if x is not defined. The key problem is that the re-order
;; of the case analisis will evaluate any combination as if it
;; were a normal application, and thus eliminate the possibility
;; of "special forms"

;; b)
(define (tagged-list1? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))
;; (call + 1 2) 
(define (application1? exp)
  (tagged-list? exp 'call))

(define (operator1 exp)
  (cadr exp))
(define (operands1 exp)
  (cddr exp))

;;;;;;;;;;;;;;;;;;
;; Exercise 4.3 ;;
;;;;;;;;;;;;;;;;;;
	 
;; (define (get) 1)
;; (define (eval1 exp env)
;;   (cond ((self-evaluating? exp) exp)
;; 	((variable? exp) (lookup-variable-value exp env))
;; 	((get 'eval (car exp)) exp env)
;; 	((application? exp)
;; 	 (apply (eval (operator exp) env)
;; 		(list-of-values (operands exp) env)))
;; 	(else
;; 	 (error "Unknown expression type -- EVAL" exp))))

;;;;;;;;;;;;;;;;;;
;; Exercise 4.4 ;;
;;;;;;;;;;;;;;;;;;

(define (empty-exp? exps) (null? exps))
;; (and (list? '()) (number? 2) 3) => 3
(define (eval-and exps env)
  (cond ((empty-exp? exps) #t)
	(else
	 (let ((first (eval (first-exp exps) env)))
	   (cond ((last-exp? exps) first)
		 (first (eval-and (rest-exps exps) env))
		 (else #f))))))
		      
(define (eval-or exps env)
  (cond ((empty-exp? exps) #f)
	(else
	 (let ((first (eval (first-exp exps) env)))
	   (cond ((last-exp? exps) first)
		 (first #t)
		 (else
		  (eval-or (rest-exps exps) env)))))))

;; (and (list? '()) (number? 2) 3) => 3
(if (list? '())
    (if (number? 2)
	3
	#f)
    #f)

(define (and-clauses exp)
  (cdr exp))
(define (or-clauses exp)
  (cdr exp))

(define (and->if exp)
  (expand-and-clauses (and-clauses exp)))

(define (expand-and-clauses clauses)
  (cond ((empty-exp? clauses) 'false)
	((last-exp? clauses) (first-exp clauses))
	(else (make-if (first-exp clauses)
		       (expand-and-clauses (rest-exps clauses))
		       #f))))

(define (or->if exp)
  (expand-or-clauses (or-clauses exp)))

(define (expand-or-clauses clauses)
  (cond ((empty-exp? clauses) 'false)
	((last-exp? clauses) (first-exp clauses))
	(else (make-if (first-exp clauses)
		       #t
		       (expand-or-clauses (rest-exps clauses))))))


;;;;;;;;;;;;;;;;;;
;; Exercise 4.5 ;;
;;;;;;;;;;;;;;;;;;

;; (cond ((assoc 'b '((a 1) (b 2))) => cadr)
;;       (else #f))
;; => 2
(define (expand-clauses1 clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
	    (rest (cdr clauses)))
	(cond ((cond-else-clause? first)
	       (if (null? rest)
		   (sequence->exp (cond-actions first))
		   (error "ELSE clause isn't last -- COND->IF"
			  clauses)))
	      ((cond-arrow-clause? first)
	       (make-if-let (cond-predicate first)
			    (cond-action-arrow first)
			    (expand-clauses rest)))
	      (else
	       (make-if (cond-predicate first)
			(sequence->exp (cond-actions first))
			(expand-clauses rest)))))))

(define (cond-arrow-clause? clause)
  (eq? (cadr clause) '=>))
(define (cond-action-arrow clause)
  (caddr clause))
(define (make-if-let predicate consequent alternative)
  '(list 'let (list (list 'pred-value predicate))
	 (list 'if
	       'pred-value
	       (list consequent 'pred-value)
	       alternative)))
  
;;;;;;;;;;;;;;;;;;
;; Exercise 4.6 ;;
;;;;;;;;;;;;;;;;;;

(define (let? exp)
  (tagged-list? exp 'let))
(define (let-clauses exp)
  (cadr exp))
(define (let-body exp)
  (cddr exp))
(define (let-clause-var clause)
  (car clause))
(define (let-clause-value clause)
  (cadr clause))
(define (let-first-clause clauses)
  (car clauses))
(define (let-rest-clauses clauses)
  (cdr clauses))
(define (let-last-clause? clauses)
  (null? (cdr clauses)))

;; long, error-checking version
(define (let-vars clauses)
  (cond ((null? clauses)
	 (error "LET clauses can't be empty -- LET->COMBINATION"
		clauses))
	((let-last-clause? clauses)
	 (list (let-clause-var (let-first-clause clauses))))
	(else
	 (cons (let-clause-var (let-first-clause clauses))
	       (let-vars (let-rest-clauses clauses))))))

;; short version, and if let-vals
;; is given the full let expression
(define (let-vals exp)
  (map cadr (cadr exp)))
;; (let ((a 1) (b 2))
;;   (do-something a b))
;; ((lambda (a b) (do-something a b)) 1 2)
(define (let->combination1 exp)
  (cons (make-lambda (let-vars (let-clauses exp)) (let-body exp))
	(let-vals exp)))

(define (eval2 exp env)
  (cond ((let? exp) (eval (let->combination1 exp) env))))

;;;;;;;;;;;;;;;;;;
;; Exercise 4.7 ;;
;;;;;;;;;;;;;;;;;;

;; (let* ((x 3)
;;        (y (+ x 2))
;;        (z (+ x y 5)))
;;   (* x z))
;; (let ((x 3))
;;   (let ((y (+ x 2)))
;;     (let ((z (+ x y 5)))
;;       (* x z))))
(define (let*->nested-lets exp)
  (expand-let* (let-clauses exp) (let-body exp)))

(define (expand-let* clauses body)
  (cond ((let-last-clause? (let-first-clause clauses))
	 (make-let clauses body))
	(else
	 (make-let (list (let-first-clause clauses))
		   (expand-let* (let-rest-clauses clauses)
				body)))))

(define (make-let clauses body)
  (cons 'let (cons clauses body)))

;; It is sufficient to add the clause to eval whose action is:
;; (eval (let*->nested-lets exp) env)

;;;;;;;;;;;;;;;;;;
;; Exercise 4.8 ;;
;;;;;;;;;;;;;;;;;;

;; (let <var> <bindings> <body>)
(define (let->combination exp)
  (cond ((symbol? (cadr exp))
	 (let->combination
	  (make-let (let-clauses exp)
		    (list (make-proc-definition
			   (let-var-name exp)
			   (let-vars (let-clauses exp))
			   (let-body exp))
			  (list (let-var-name exp)
				(let-vals (let-clauses exp)))))))
	;; normal let
	(else (cons (make-lambda (let-vars (let-clauses exp)) (let-body exp))
		    (let-vals (let-clauses exp))))))

(define (let-var-name exp)
  (cadr exp))

(define (make-proc-definition name params body)
  (cons 'define (cons (cons name params) body)))

;; (let fib-iter ((a 1) (b 0) (count n))
;;   (if (= count 0)
;;       b
;;       (fib iter (+ a b) a (- count 1))))

;; (let ((a 1) (b 0) (count n))
;;   (define (fib-iter a b count)
;;     (if (= count 0)
;; 	b
;; 	(fib iter (+ a b) a (- count 1))))
;;   (fib-iter 1 0 n))

		  
;;;;;;;;;;;;;;;;;;;
;; Exercise 4.11 ;;
;;;;;;;;;;;;;;;;;;;

(define (make-binding var val) (list var val))
(define (binding-var binding) (car binding))
(define (binding-val binding) (cadr binding))
(define (set-binding-val! binding newval)
  (set-car! (cdr binding) newval))

(define (make-frame1 variables values)
  (cond ((null? variables) '())
	(else (cons (make-binding (car variables)
				  (car values))
		    (make-frame (cdr variables)
				(cdr values))))))
(define (frame-variables1 frame)
  (cond ((null? frame) '())
	(else (cons (binding-var (car frame))
		    (frame-variables (cdr frame))))))
(define (frame-values1 frame)
  (cond ((null? frame) '())
	(else (cons (binding-val (car frame))
		    (frame-values (cdr frame))))))

(define (add-binding-to-frame1! var val frame)
  (cons (make-binding var val) frame))

(define (first-binding frame) (car frame))
(define (rest-bindings frame) (cdr frame))  

;;;;;;;;;;;;;;;;;;;
;; Exercise 4.12 ;;
;;;;;;;;;;;;;;;;;;;

(define (lookup-variable-value1 var env)
  (let ((binding (lookup-binding-env var env)))
    (if binding
	(binding-var binding)
	(error "Unbound variable" var))))

(define (identity i) i)

(define (lookup-binding-frame var frame)
  (cond ((null? frame) #f)
	((eq? var (binding-var (first-binding frame)))
	 (first-binding frame))
	(else
	 (lookup-binding-frame var (rest-bindings frame)))))

;; return false if not found
(define (lookup-binding-env var env)
  (cond ((eq? env the-empty-environment) #f)
	((lookup-binding-frame var (first-frame env)) => identity)
	(else (lookup-binding-env var (enclosing-environment env)))))

;; review lookup-variable-value
;; do set-variable-value! & define-variable!
(define (set-variable-value1! var val env)
  (let ((binding (lookup-binding-env var env)))
    (if binding
	(set-binding-val! binding val)
	(error "Unbound variable -- SET!" var))))

(define (define-variable1! var val env)
  (let ((binding (lookup-binding-frame var (first-frame env))))
    (if binding
	(set-binding-val! binding val)
	(add-binding-to-frame! var val (first-frame env)))))

;;;;;;;;;;;;;;;;;;;
;; Exercise 4.13 ;;
;;;;;;;;;;;;;;;;;;;

(define (eval3 exp env)
  (cond (;; previous types of expressions
	 ((make-unbound? exp)
	  (make-unbound! (make-unbound-var exp) env)))))

(define (make-unbound? exp)
  (tagged-list? exp 'make-unbound))
(define (make-unbound-var exp) (cadr exp))

(define (make-unbound! var env)
  (let ((frame (first-frame env)))
    (define (unbound-rest-binding! frame)
      (let ((first (first-binding frame))
	    (second (first-binding (rest-bindings frame))))
	(if (eq? var (binding-var second))
	    (set-cdr! first (cdr second))
	    (unbound-rest-binding! (rest-bindings frame)))))
    (cond ((eq? var (binding-var (first-binding frame)))
	   (set! frame (rest-bindings frame)))
	  (else (unbound-rest-binding! frame)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 4.1.4 Running the Evaluator as a Program ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
	((let? exp) (eval (let->combination1 exp) env))
	((application? exp)
	 (apply (eval (operator exp) env)
		(list-of-values (operands exp) env)))
	(else
	 (error "Unknown expression type -- EVAL" exp))))

;; the next definition of apply will shadow the underlying
;; lisp apply, so we save it here as an alias
(define apply-in-underlying-scheme apply)

;; APPLY
(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
	 (apply-primitive-procedure procedure arguments))
	((compound-procedure? procedure)
	 (eval-sequence
	  (procedure-body procedure)
	  (extend-environment (procedure-parameters procedure)
			      arguments
			      (procedure-environment procedure))))
	(else (error
	       "Unknown procedure type -- APPLY" procedure))))

;; PROCEDURE ARGUMENTS
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
	    (list-of-values (rest-operands exps) env))))

;; CONDITIONALS
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
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
(define (procedure-body p)
  (scan-out-defines (caddr p)))
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
	(list 'first car)))

(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))
  

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
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

;; (define (map f elts)
;;   (cond ((null? elts) null)
;; 	(else
;; 	 (cons (f (car elts))
;; 	       (map f (cdr elts))))))
		  
;;;;;;;;;;;;;;;;;;;
;; Exercise 4.14 ;;
;;;;;;;;;;;;;;;;;;;

;; Louis's map fails because when the mapping function is
;; applied internally by map, it is a function constructed
;; by the interpreter, and the underlying apply conflicts
;; with the "type" (procedure or primitive) tag put there

;; in other words, the underlying application of a procedure
;; constructed in the interpreter (one level above) generates
;; an error

;;;;;;;;;;;;;;;;;;;
;; Exercise 4.16 ;;
;;;;;;;;;;;;;;;;;;;

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     (env-loop (enclosing-environment env)))
	    ((eq? var (car vars))
	     (let ((desired-value (car vals)))
	       (if (eq? '*unassigned* desired-value)
		   (error "Using not-yet-assigned variable" var)
		   desired-value)))
	    (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable" var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame)
		(frame-values frame)))))
  (env-loop env))


(define (extract-vars-and-vals body)
  (let ((defines
	  (filter (lambda (e) (and (list? e) (eq? (car e) 'define)))
		  body)))
    (map (lambda (def-clause)
	   (cons (cadr def-clause) (caddr def-clause)))
	 defines)))

(define (extract-non-defines body)
  (filter (lambda (e) (or (not (list? e)) (not (eq? (car e) 'define))))
	  body))

(define (unassigned-clause data)
  (list (car data) (quote (quote *unassigned*))))
(define (var-set data)
  (list 'set! (car data) (cdr data)))

(define (scan-out-defines body)
  (let ((vars-and-vals
	 (extract-vars-and-vals body))
	(non-defines
	 (extract-non-defines body)))
    (if (> (length vars-and-vals) 0)
	(list (make-let (map unassigned-clause vars-and-vals)
			(append (map var-set vars-and-vals)
				non-defines)))
	body)))

;;;;;;;;;;;;;;;;;;;
;; Exercise 4.17 ;;
;;;;;;;;;;;;;;;;;;;

in a correct program, both environment structures will work
the same way.

;;;;;;;;;;;;;;;;;;;
;; Exercise 4.18 ;;
;;;;;;;;;;;;;;;;;;;

;; this procedure wont work if internal definitions are scanned
;; out as shown, because (stream-map f y) will evaluate y and
;; raise an error (because of it being initialized to *unassigned*)

;; the scan-out-defines method will work fine

;;;;;;;;;;;;;;;;;;;
;; Exercise 4.19 ;;
;;;;;;;;;;;;;;;;;;;

;; I support the viewpoint of Ben because I find it to be the most
;; strightforward way to evaluate such an expression. Maybe
;; the programmer wants exactly that behavior and, if the interpeter
;; re-orders the expressions in a way that changes the meaning of
;; the program, it is hard to debug.


;;;;;;;;;;;;;;;;;;;
;; Exercise 4.20 ;;
;;;;;;;;;;;;;;;;;;;

;; (letrec ((a 1)
;;     	    (b 2))
;;   (+ a b))
;; (let ((a '*unassigned*)
;;       (b '*unassigned*))
;;   (set! a 1)
;;   (set! b 2)
;;   (+ a b))
  
;; a.
(define (letrec->let exp)
  (define (variable-creation clause)
    (list (let-clause-var clause)
	  (quote (quote *unassigned*))))
  (define (variable-assignation clause)
    (list 'set!
	  (let-clause-var clause)
	  (let-clause-value clause)))
  (make-let (map variable-creation (let-clauses exp))
	    (append (map variable-assignation
			 (let-clauses exp))
		    (let-body exp))))

