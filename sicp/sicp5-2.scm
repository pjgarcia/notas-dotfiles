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
  (let ((contents '*unassigned*)
	(tracing #f))
    (define (trace old new)
      (if tracing
	  (begin
	    (newline)
	    (display (list name ':
			   'old old
			   'new new)))))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
	    ((eq? message 'set)
	     (lambda (value)
	       (trace contents value)
	       (set! contents value)))
	    ((eq? message 'trace-on)
	     (set! tracing #t))
	    ((eq? message 'trace-off)
	     (set! tracing #f))
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

;; se reemplaza despues para la seccion 5.2.4
(define (make-stack-old)
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
	(the-instructions-sequence '())
	(raw-instructions '())
	(entry-points '())
	(instruction-count 0)
	(tracing #f)
	(stacked-registers '())
	(labels '())
	(assign-records '()))
    (let ((the-ops
	   (list (list 'initialize-stack
		       (lambda () (stack 'initialize)))
		 ;; para la seccion 5.2.4
		 (list 'print-stack-statistics
		       (lambda () (stack 'print-statistics)))))
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
		;; for 5.16
		(if tracing
		    (trace (car insts) labels))
		((instruction-execution-proc (car insts)))
		;; for 5.15
		(set! instruction-count (+ instruction-count 1))
		(execute)))))
      (define (trace inst labels)
	(let ((instruction-labels
	       (filter (lambda (label)
			 (and (not (null? (label-instructions label)))
			      (eq? (car (label-instructions label)) inst)))
		       labels)))
	  (newline)
	  (display
	   (append
	    (map (lambda (l) (label-name l)) instruction-labels)
	    (list " > " (instruction-text inst))))))
      ;; for Exercise 5.12
      (define (store-instruction inst)
	(set! raw-instructions (cons inst raw-instructions)))
      (define (store-entry-point reg)
	(set! entry-points (cons reg entry-points)))
      (define (store-stack-register reg)
	(set! stacked-registers (cons reg stacked-registers)))
      (define (store-assign reg place)
	(let ((assign-record (assoc reg assign-records)))
	  (if assign-record
	      (set-cdr! assign-record (cons place (cdr assign-record)))
	      (set! assign-records (cons (list reg place) assign-records)))))
      (define (install-register reg-name)
	(if (assoc reg-name register-table)
	    'already-defined
	    (allocate-register reg-name)))
      (define (track-labels l)
	(set! labels l))
      (define (trace-on-register reg-name)
	(let ((reg (lookup-register reg-name)))
	  (reg 'trace-on)))
      (define (trace-off-register reg-name)
	(let ((reg (lookup-register reg-name)))
	  (reg 'trace-off)))
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
	      ((eq? message 'store-instruction) store-instruction)
	      ((eq? message 'store-entry-point) store-entry-point)
	      ((eq? message 'store-stack-register) store-stack-register)
	      ((eq? message 'store-assign) store-assign)
	      ((eq? message 'stored-instructions) raw-instructions)
	      ((eq? message 'entry-points) entry-points)
	      ((eq? message 'stacked-registers) stacked-registers)
	      ((eq? message 'assigned-records) assign-records)
	      ((eq? message 'operations) the-ops)
	      ((eq? message 'install-register) install-register)
	      ;; for Exercise 5.15
	      ((eq? message 'instruction-count) instruction-count)
	      ((eq? message 'reset-instruction-count)
	       (set! instruction-count 0))
	      ;; for Exercise 5.16
	      ((eq? message 'trace-on)
	       (set! tracing #t)
	       'now-tracing)
	      ((eq? message 'trace-off)
	       (set! tracing #f)
	       'stopped-tracing)
	      ((eq? message 'trace-on-register) trace-on-register)
	      ((eq? message 'trace-off-register) trace-off-register)
	      ((eq? message 'track-labels) track-labels)
	      (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

;;;;;;;;;;;;;;;;;;;
;; The Assembler ;;
;;;;;;;;;;;;;;;;;;;
	      
(define (assemble controller-text machine)
  (extract-labels controller-text
		  (lambda (insts labels)
		    (update-insts! insts labels machine)
		    ((machine 'track-labels) labels)
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
       ;; for Exercise 5.13
       (let ((instruction-registers
	      (get-instruction-registers (instruction-text inst))))
	 (for-each (lambda (instr-reg)
		     ((machine 'install-register) instr-reg))
		   instruction-registers))
       ;; for Exercise 5.12
       (save-machine-data (instruction-text inst) machine)
       
       (set-instruction-execution-proc!
	inst
	(make-execution-procedure
	 (instruction-text inst) labels machine
	 pc flag stack ops)))
     insts)))

(define (save-machine-data inst machine)
  ;; save instruction
  ((machine 'store-instruction) inst)
  ;; save entry point
  (if (and (eq? (car inst) 'goto) (eq? (caadr inst) 'reg))
      ((machine 'store-entry-point) (cadadr inst)))
  ;; save pushed or pulled registers
  (if (or (eq? (car inst) 'save) (eq? (car inst) 'restore))
      ((machine 'store-stack-register) (cadr inst)))
  ;; save assigns
  (if (eq? (car inst) 'assign)
      ((machine 'store-assign) (cadr inst) (cddr inst))))

;; (assign continue (label fact-done))
(define (get-instruction-registers inst)
  (let ((type (car inst)))
    (cond ((or (eq? type 'save) (eq? type 'restore))
	   (list (cadr inst)))
	  ((eq? type 'assign)
	   (append (list (cadr inst))
		   (extract-registers (cddr inst))))
	  ((eq? type 'test)
	   (extract-registers (cddr inst)))
	  ((eq? type 'goto)
	   (extract-registers (cdr inst)))
	  (else '()))))

(define (extract-registers l)
  (map (lambda (e) (cadr e))
       (filter (lambda (e) (eq? (car e) 'reg)) l)))


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

(define (label-name label) (car label))

(define (label-instructions label) (cdr label))

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
(define (make-stack3)
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

;;;;;;;;;;;;;;;;;;;
;; Exercise 5.12 ;;
;;;;;;;;;;;;;;;;;;;

;; done in the assembler implementation 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 5.2.4 Monitoring Machine Performance ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-stack)
  (let ((s '())
	(number-pushes 0)
	(max-depth 0)
	(current-depth 0))
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ number-pushes 1))
      (set! current-depth (+ current-depth 1))
      (set! max-depth (max max-depth current-depth)))
    (define (pop)
      (if (null? s)
	  (error "Empty stack -- POP")
	  (let ((top (car s)))
	    (set! s (cdr s))
	    (set! current-depth (- current-depth 1))
	    top)))
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)
    (define (print-statistics)
      (newline)
      (display (list 'total-pushes '= number-pushes
		     'max-depth '= max-depth)))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
	    ((eq? message 'pop) (pop))
	    ((eq? message 'initialize) (initialize))
	    ((eq? message 'print-statistics)
	     (print-statistics))
	    (else (error "Unknown request -- STACK"
			 message))))
    dispatch))


(define fact-machine
  (make-machine
   '()
   (list (list '= =)
	 (list '- -)
	 (list '* *))
   '((assign continue (label fact-done))
     fact-loop
     (test (op =) (reg n) (const 1))
     (branch (label base-case))
     (save continue)
     (save n)
     (assign n (op -) (reg n) (const 1))
     (assign continue (label after-fact))
     (goto (label fact-loop))
     after-fact
     (restore n)
     (restore continue)
     (assign val (op *) (reg n) (reg val))
     (goto (reg continue))
     base-case
     (assign val (const 1))
     (goto (reg continue))
     fact-done)))
	    
(define gcd-machine
  (make-machine
   ;;'(a b t)
   '()
   (list (list 'rem remainder) (list '= =))
   '(test-b
     (test (op =) (reg b) (const 0))
     (branch (label gcd-done))
     (assign t (op rem) (reg a) (reg b))
     (assign a (reg b))
     (assign b (reg t))
     (goto (label test-b))
     gcd-done)))

(define fib-machine
  (make-machine
   ;;'(continue n val)
   '()
   (list (list '< <) (list '- -) (list '+ +))
   '((assign continue (label fib-done))
     fib-loop
     (test (op <) (reg n) (const 2))
     (branch (label immediate-answer))
     ;; set up to compute Fib(n-1)
     (save continue)
     (assign continue (label afterfib-n-1))
     (save n)
     (assign n (op -) (reg n) (const 1))
     (goto (label fib-loop))
     afterfib-n-1
     (restore n)
     (restore continue)
     ;; set up to compute Fib(n-2)
     (assign n (op -) (reg n) (const 2))
     (save continue)
     (assign continue (label afterfib-n-2))
     (save val)
     (goto (label fib-loop))
     afterfib-n-2
     (assign n (reg val))
     (restore val)
     (restore continue)
     (assign val (op +) (reg val) (reg n))
     (goto (reg continue))
     immediate-answer
     (assign val (reg n))
     (goto (reg continue))
     fib-done)))

(define (fact-statistics rounds)
  (for-each (lambda (n)
	      (set-register-contents! fact-machine 'n n)
	      ((fact-machine 'stack) 'initialize)
	      (fact-machine 'reset-instruction-count)
	      (fact-machine 'start)
	      ((fact-machine 'stack) 'print-statistics)
	      (display (list 'n '= n 'val '= (get-register-contents fact-machine 'val)))
	      (display (fact-machine 'instruction-count)))
	    rounds))

;;;;;;;;;;;;;;;;;;;
;; Exercise 5.14 ;;
;;;;;;;;;;;;;;;;;;;
	      
;; TP(n) = 2 * n - 2
;; MD(n) = 2 * n - 2

