;;;;;;;;;;;;;;;;;;;;;;;
;; The Machine Model ;;
;;;;;;;;;;;;;;;;;;;;;;;

(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
		((machine 'allocate-register) register-name))
	      register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

;;;;;;;;;;;;;;;
;; Registers ;;
;;;;;;;;;;;;;;;

(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
	    ((eq? message 'set)
	     (lambda (value) (set! contents value)))
	    (else
	     (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))

;;;;;;;;;;;;;;;
;; The Stack ;;
;;;;;;;;;;;;;;;

(define (make-stack)
  (let ((s '()))
    (define (push x)
      (set! s (cons x s)))
    (define (pop)
      (if (null? s)
	  (error "Empty stack -- POP")
	  (let ((top (car s)))
	    (set! s (cdr s))
	    top)))
    (define (initialize)
      (set s '())
      'done)
    (define (dispatch message)
      (cond ((eq? message 'push) push)
	    ((eq? message 'pop) (pop))
	    ((eq? message 'initialize) (initialize))
	    (else (error "Unknown request -- STACK"
			 message))))
    dispatch))

(define (pop stack)
  (stack 'pop))

(define (push stack value)
  ((stack 'push) value))

;;;;;;;;;;;;;;;;;;;;;;;
;; The basic machine ;;
;;;;;;;;;;;;;;;;;;;;;;;

(define (start machine)
  (machine 'start))

(define (get-register-contents machine register-name)  
  (get-contents (get-register machine register-name)))

(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
	(flag (make-register 'flag))
	(stack (make-stack))
	(the-instructions-sequence '()))
    (let ((the-ops
	   (list (list 'initialize-stack
		       (lambda () (stack 'initialize)))))
	  (register-table
	   (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
	(if (assoc name register-table)
	    (error "Multiply defined register: " name)
	    (set! register-table
		  (cons (list name (make-register name))
			register-table)))
	'register-allocated)
      (define (lookup-register name)
	(let ((val (assoc name register-table)))
	  (if val
	      (cadr val)
	      (error "Unknown register:" name))))
      (define (execute)
	(let ((insts (get-contents pc)))
	  (if (null? insts)
	      'done
	      (begin
		((instruction-execution-proc (car insts)))
		(execute)))))
      (define (dispatch message)
	(cond ((eq? message 'start)
	       (set-contents! pc the-instructions-sequence)
	       (execute))
	      ((eq? message 'install-instruction-sequence)
	       (lambda (seq) (set! the-instructions-sequence seq)))
	      ((eq? message 'allocate-register) allocate-register)
	      ((eq? message 'get-register) lookup-register)
	      ((eq? message 'install-operations)
	       (lambda (ops) (set! the-ops (append the-ops ops))))
	      ((eq? message 'stack) stack)
	      ((eq? message 'operations) the-ops)
	      (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

;;;;;;;;;;;;;;;;;;;
;; The Assembler ;;
;;;;;;;;;;;;;;;;;;;
	      
(define (assemble controller-text machine)
  (extract-labels controller-text
		  (lambda (insts labels)
		    (update-insts! insts labels machine)
		    insts)))

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
		      (lambda (insts labels)
			(let ((next-inst (car text)))
			  (if (symbol? next-inst)
			      (receive insts
				  (cons (make-label-entry next-inst insts)
					labels))
			      (receive (cons (make-instruction next-inst)
					     insts)
				  labels)))))))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
	(flag (get-register machine 'flag))
	(stack (machine 'stack))
	(ops (machine 'operations)))
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc!
	inst
	(make-execution-procedure
	 (instruction-text inst) labels machine
	 pc flag stack ops)))
     insts)))

(define (make-instruction text)
  (cons text '()))

(define (instruction-text inst)
  (car inst))

(define (instruction-execution-proc inst)
  (cdr inst))

(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
	(cdr val)
	(error "Undefined label -- ASSEMBLE" label-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generating Execution Procedures for Instructions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-execution-procedure inst labels machine
				  pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
	 (make-assign inst machine labels ops pc))
	((eq? (car inst) 'test)
	 (make-test inst machine labels ops flag pc))
	((eq? (car inst) 'branch)
	 (make-branch inst machine labels flag pc))
	((eq? (car inst) 'goto)
	 (make-goto inst machine labels pc))
	((eq? (car inst) 'save)
	 (make-save inst machine stack pc))
	((eq? (car inst) 'restore)
	 (make-restore inst machine stack pc))
	((eq? (car inst) 'perform)
	 (make-perform inst machine labels ops pc))
	(else (error "Unknown instruction type -- ASSEMBLE"
		     inst))))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Assign instruction ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-assign inst machine labels operations pc)
  (let ((target
	 (get-register machine (assign-reg-name inst)))
	(value-exp (assign-value-exp inst)))
    (let ((value-proc
	   (if (operation-exp? value-exp)
	       (make-operation-exp
		value-exp machine labels operations)
	       (make-primitive-exp
		(car value-exp) machine labels))))
      (lambda ()
	(set-contents! target (value-proc))
	(advance-pc pc)))))

;; (assign t (op rem) (reg a) (reg b))
(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))

(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test, branch and goto instructions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (test (op <) (reg t) (reg b))
(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
	(let ((condition-proc
	       (make-operation-exp
		condition machine labels operations)))
	  (lambda ()
	    (set-contents! flag (condition-proc))
	    (advance-pc pc)))
	(error "Bad TEST instruction -- ASSEMBLE" inst))))

(define (test-condition test-instruction)
  (cdr test-instruction))

;; (branch (label gcd-done))
(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
	(let ((insts
	       (lookup-label labels (label-exp-label dest))))
	  (lambda ()
	    (if (get-contents flag)
		(set-contents! pc insts)
		(advance-pc pc))))
	(error "Bad BRANCH instruction -- ASSEMBLE" inst))))

(define (branch-dest branch-instruction)
  (cadr branch-instruction))

;; (goto (label test-b))
(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
	   (let ((insts
		  (lookup-label labels
				(label-exp-label dest))))
	     (lambda () (set-contents! pc insts))))
	  ((register-exp? dest)
	   (let ((reg
		  (get-register machine
				(register-exp-reg dest))))
	     (lambda ()
	       (set-contents! pc (get-contents reg)))))
	  (else (error "Bad GOTO instruction -- ASSEMBLE"
		       inst)))))

(define (goto-dest goto-instruction)
  (cadr goto-instruction))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Other instructions ;;
;;;;;;;;;;;;;;;;;;;;;;;;
	 
(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
			   (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
			   (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))

(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
	(let ((action-proc
	       (make-operation-exp
		action machine labels operations)))
	  (lambda ()
	    (action-proc)
	    (advance-pc pc)))
	(error "Bad PERFORM instruction -- ASSEMBLE" inst))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Execution procedures for subexpressions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (assign continue (label afterfib-n-2))
;; (assign val (reg n))
(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
	 (let ((c (constant-exp-value exp)))
	   (lambda () c)))
	((label-exp? exp)
	 (let ((insts
		(lookup-label labels
			      (label-exp-label exp))))
	   (lambda () insts)))
	((register-exp? exp)
	 (let ((r (get-register machine
				(register-exp-reg exp))))
	   (lambda () (get-contents r))))
	(else
	 (error "Unknown expression type -- ASSEMBLE" exp))))

(define (tagged-list? a tag)
  (and (list? a) (eq? (car a) tag)))

(define (register-exp? exp) (tagged-list? exp 'reg))

(define (register-exp-reg exp) (cadr exp))

(define (constant-exp? exp) (tagged-list? exp 'const))

(define (constant-exp-value exp) (cadr exp))

(define (label-exp? exp) (tagged-list? exp 'label))

(define (label-exp-label exp) (cadr exp))

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
	(aprocs
	 (map (lambda (e)
		(make-primitive-exp e machine labels))
	      (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))

(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))

(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
	(cadr val)
	(error "Unknown operation -- ASSEMBLE" symbol))))

;; definir el ejemplo de gcd-machine y probarlo :)
(define gcd-machine
  (make-machine
   '(a b t)
   (list (list 'rem remainder) (list '= =))
   '(test-b
     (test (op =) (reg b) (const 0))
     (branch (label gcd-done))
     (assign t (op rem) (reg a) (reg b))
     (assign a (reg b))
     (assign b (reg t))
     (goto (label test-b))
     gcd-done)))
     
;;;;;;;;;;;;;;;;;;
;; Exercise 5.9 ;;
;;;;;;;;;;;;;;;;;;

(define (make-operation-exp2 exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
	(aprocs
	 (map (lambda (e)
		(if (label-exp? e)
		    (error "Bad operand exp -- ASSEMBLE" e)
		    (make-primitive-exp e machine labels)))
	      (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

;;;;;;;;;;;;;;;;;;;
;; Exercise 5.11 ;;
;;;;;;;;;;;;;;;;;;;

;; b.
(define (make-stack2)
  (let ((s '()))
    (define (push x reg)
      (set! s (cons (cons reg x) s)))
    (define (pop reg)
      (cond ((null? s)
	     (error "Empty stack -- POP"))
	    ((not (eq? reg (caar s)))
	     (error "Wrong register -- POP"))
	    (else
	     (let ((top (car s)))
	       (set! s (cdr s))
	       top))))
    (define (initialize)
      (set s '())
      'done)
    (define (dispatch message)
      (cond ((eq? message 'push) push)
	    ((eq? message 'pop) (pop))
	    ((eq? message 'initialize) (initialize))
	    (else (error "Unknown request -- STACK"
			 message))))
    dispatch))

(define (pop2 stack reg)
  ((stack 'pop) reg))

(define (push2 stack value reg)
  ((stack 'push) value reg))

(define (make-save2 inst machine stack pc)
  (let ((reg (get-register machine
			   (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg) (stack-inst-reg-name inst))
      (advance-pc pc))))

(define (make-restore2 inst machine stack pc)
  (let ((reg (get-register machine
			   (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack (stack-inst-reg-name inst)))
      (advance-pc pc))))

;; c.
(define (make-stack2)
  (let ((s '()))
    (define (push x reg)
      (let ((reg-stack (assoc reg s)))
	(if (null? reg-stack)
	    (set! s (cons (list reg (list x)) s))
	    (set-cdr! reg-stack (cons x (cdr reg-stack))))))
    (define (pop reg)
      (let ((reg-stack (assoc reg s)))
	(cond ((null? reg-stack)
	       (error "Empty stack -- POP"))
	      (else
	       (let ((top (car (cdr reg-stack))))
		 (set-cdr! reg-stack (cdr (cdr reg-stack)))
		 top)))))
    (define (initialize)
      (set s '())
      'done)
    (define (dispatch message)
      (cond ((eq? message 'push) push)
	    ((eq? message 'pop) (pop))
	    ((eq? message 'initialize) (initialize))
	    (else (error "Unknown request -- STACK"
			 message))))
    dispatch))


