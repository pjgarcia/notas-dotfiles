;;;;;;;;;;;;;;;;;;
;; Exercise 5.2 ;;
;;;;;;;;;;;;;;;;;;

(controller
 test-counter
 (test (op >) (reg counter) (reg n))
 (branch (label fact-done))
 (assign product (op *) (reg product) (reg counter))
 (assign counter (op +) (reg counter) (const 1))
 (goto (label test-counter))
 fact-done)

;;;;;;;;;;;;;;;;;;
;; Exercise 5.3 ;;
;;;;;;;;;;;;;;;;;;

(controller
 test-good-enough
 (test (op good-enough?) (reg guess))
 (branch (label sqrt-done))
 (assign guess (op improve) (reg guess))
 (goto (label test-good-enough))
 sqrt-done)

(controller
 test-good-enough
 (assign sq (op square) (reg guess))
 (assign subs (op -) (reg x) (reg sq))
 (assign abs (op abs) (reg subs))
 (test (op <) (reg abs) (const 0.1))
 (branch (label fact-done))
 (assign rel (op /) (reg guess) (reg x))
 (assign guess (op average) (reg rel) (reg guess))
 (goto test-good-enough)
 fact-done)
 
;;;;;;;;;;;;;;;;;;
;; Exercise 5.4 ;;
;;;;;;;;;;;;;;;;;;

;; recursive exponentiation
(controller
 (assign continue (label expt-done))
 expt-loop
 (test (op =) (reg n) (const 0))
 (branch (label base-case))
 (save continue)
 (save n)
 (assign n (op -) (reg n) (const 1))
 (assign continue (label after-expt))
 (goto (label expt-loop))
 after-expt
 (restore n)
 (restore continue)
 (assign val (op *) (reg b) (reg val))
 (goto (reg continue))
 base-case 
 (assign val (const 1))
 (goto (reg continue))
 expt-done)

;; iterative exponentiation
(controller
 (assign counter (reg n))
 (assign product (const 1))
 (assign continue (label expt-done))
 expt-loop
 (test (op =) (reg counter) (const 0))
 (branch (label expt-done))
 (assign counter (op -) (reg counter) (const 1))
 (assign product (op *) (reg product) (reg b))
 (goto expt-loop)
 expt-done)

;;;;;;;;;;;;;;;;;;
;; Exercise 5.5 ;;
;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
;; Exercise 5.6 ;;
;;;;;;;;;;;;;;;;;;

;; (restore continue) & (save continue) after afterfib-n-1

;;;;;;;;;;;;;;;;;;
;; Exercise 5.8 ;;
;;;;;;;;;;;;;;;;;;

;; the value of register a will be 3
(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
		      (lambda (inst labels)
			(let ((next-inst (car text)))
			  (if (symbol? next-inst)
			      (let ((val
				     (assoc next-inst labels)))
				(if val
				    (error "Duplicated labels -- ASSEMBLE" next-inst)
				    (receive
					insts
					(cons (make-label-entry next-inst insts)))))
			      (receive
				  (cons (make-instruction next-inst)
					insts)
				  labels)))))))



				  
			  
					
				     
