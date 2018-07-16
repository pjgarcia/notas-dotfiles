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
