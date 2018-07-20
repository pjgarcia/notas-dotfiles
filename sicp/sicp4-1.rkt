#lang racket

;;;;;;;;;;;;;;;;;;
;; Exercise 4.1 ;;
;;;;;;;;;;;;;;;;;;

;; left to right
(define (list-of-values-ltor exps env)
  (if (no-operands? exps)
      '()
      (let (first (eval (first-operand) env))
	(cons first
	      (list-of-values (rest-operands exps) env)))))
;; right to left
(define (list-of-values-ltor exps env)
  (if (no-operands? exps)
      '()
      (let ((rest (list-of-values (rest-operands exps) env)))
	(cons (first (eval (first-operand) env))
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
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))
;; (call + 1 2) 
(define (application? exp)
  (tagged-list? exp 'call))

(define (operator exp)
  (cadr exp))
(define (operands exp)
  (cddr exp))

;;;;;;;;;;;;;;;;;;
;; Exercise 4.3 ;;
;;;;;;;;;;;;;;;;;;
	 
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-variable-value exp env))
	((get 'eval (car exp)) exp env)
	((application? exp)
	 (apply (eval (operator exp) env)
		(list-of-values (operands exp) env)))
	(else
	 (error "Unknown expression type -- EVAL" exp))))

;;;;;;;;;;;;;;;;;;
;; Exercise 4.4 ;;
;;;;;;;;;;;;;;;;;;

(define (and-clauses exp) (cdr exp))
(define (first-exp seq) (car seq))
(define (rest-exp seq) (cdr seq))
(define (empty-exp? seq) (null? seq))
(define (last-exp? seq) (null? (cdr seq)))

;; (and (list? '()) (number? 2) 3) => 3
(define (eval-and exps env)
  (cond ((empty-exp? exps) #t)
	(else
	 (let ((first (eval (first-expt exps) env)))
	   (cond ((last-exp? exps) first)
		 (first (eval-and (rest-exp exps) env))
		 (else #f))))))
		      
(define (eval-or exps env)
  (cond ((empty-exp? exps) #f)
	(else
	 (let ((first (eval (first-exp exps) env)))
	   (cond ((last-exp? exps) first)
		 (first #t)
		 (else
		  (eval-or (rest-exp exps) env)))))))

;; (and (list? '()) (number? 2) 3) => 3
(if (list? '())
    (if (number? 2)
	3
	#f)
    #f)

(define (and->if exp)
  (expand-and-clauses (and-clauses exp)))

(define (expand-and-clauses clauses)
  (cond ((empty-exp? clauses) 'false)
	((last-exp? clauses) (first-exp clauses))
	(else (make-if (first-exp clauses)
		       (expand-and-clauses (rest-exp clauses))
		       #f))))

(define (or->if exp)
  (expand-or-clauses (or-clauses exp)))

(define (expand-or-clauses clauses)
  (cond ((empty-exp? clauses) 'false)
	((last-exp? clauses) (first-exp clauses))
	(else (make-if (first-exp clauses)
		       #t
		       (expand-or-clauses (rest-exp clauses))))))


;;;;;;;;;;;;;;;;;;
;; Exercise 4.5 ;;
;;;;;;;;;;;;;;;;;;

;; (cond ((assoc 'b '((a 1) (b 2))) => cadr)
;;       (else #f))
;; => 2
(define (expand-clauses clauses)
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
	((let-last-clause? (let-first-clause clauses))
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
(define (let->combination exp)
  (cons (make-lambda (let-vars (let-clauses exp)) (let-body exp))
	(let-vals (let-clauses exp))))

(define (eval exp env)
  (cond ((let? exp) (eval (let->combiantion exp) env))))

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
  (set! (binding-val binding) newval))

(define (make-frame variables values)
  (cond ((null? variables) '())
	(else (cons (make-binding (car variables)
				  (car values))
		    (make-frame (cdr variables)
				(cdr values))))))
(define (frame-variables frame)
  (cond ((null? frame) '())
	(else (cons (binding-var (car frame))
		    (frame-variables (cdr frame))))))
(define (frame-values frame)
  (cond ((null? frame) '())
	(else (cons (binding-val (car frame))
		    (frame-values (cdr frame))))))

(define (add-binding-to-frame! var val frame)
  (cons (make-binding var val) frame))

(define (first-bindin frame) (car frame))
(define (rest-bindings frame) (cdr frame))  

;;;;;;;;;;;;;;;;;;;
;; Exercise 4.12 ;;
;;;;;;;;;;;;;;;;;;;

(define (lookup-variable-value var env)
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
  (cond ((eq? env the-empty-environment?) #f)
	((lookup-binding-frame var (first-frame env)) => identity)
	(else (lookup-binding-env var (enclosing-env env)))))

;; review lookup-variable-value
;; do set-variable-value! & define-variable!
(define (set-variable-value! var val env)
  (let ((binding (lookup-binding-env var env)))
    (if binding
	(set-binding-val! binding val)
	(error "Unbound variable -- SET!" var))))

(define (define-variable! var val env)
  (let ((binding (lookup-binding-frame (first-frame env))))
    (if binding
	(set-binding-val! binding val)
	(add-binding-to-frame! var val (first-frame env)))))

;;;;;;;;;;;;;;;;;;;
;; Exercise 4.13 ;;
;;;;;;;;;;;;;;;;;;;

(define (eval exp env)
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
