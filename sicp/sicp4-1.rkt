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


