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
