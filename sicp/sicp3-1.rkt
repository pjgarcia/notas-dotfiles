#lang racket/base

;;;;;;;;;;;;;;;;;;
;; Exercise 3.1 ;;
;;;;;;;;;;;;;;;;;;

(define (make-accumulator accum)
  (lambda (increment)
    (set! accum (+ accum  increment))
    accum))
  
;;;;;;;;;;;;;;;;;;
;; Exercise 3.2 ;;
;;;;;;;;;;;;;;;;;;

(define (make-monitored f)
  (let ((times-called 0))
    (lambda (x)
      (cond ((eq? x 'how-many-calls?)
	     times-called)
	    ((eq? x 'reset-count)
	     (set! times-called 0))
	    (else
	     (set! times-called (+ times-called 1))
	     (f x))))))

;;;;;;;;;;;;;;;;;;
;; Exercise 3.3 ;;
;;;;;;;;;;;;;;;;;;

(define (make-account balance password)
  (let ((pass-missings 0))
    (define (withdraw amount)
      (if (>= balance amount)
	  (begin (set! balance (- balance amount))
		 balance)
	  "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (dispatch pass method)
      (if (eq? pass password)
	  (cond ((eq? method 'withdraw) withdraw)
		((eq? method 'deposit) deposit)
		(else
		 (error "Unknown request -- MAKE-ACCOUNT"
			method)))
	  (begin
	    (set! pass-missings (+ pass-missings 1))
	    (if (< pass-missings 3)
		(lambda (x) "Incorrect password")
		(call-the-cops)))))
    dispatch))

(define (call-the-cops)
  "1-14-52534 CALLING COPS!")

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Monte Carlo Method ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define random-init 3)
(define (rand-update x)
  (+ x 1))

(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
	   (/ trials-passed trials))
	  ((experiment)
	   (iter (- trials-remaining 1)
		 (+ trials-passed 1)))
	  (else
	   (iter (- trials-remaining 1)
		 trials-passed))))
  (iter trials 0))

;; same computation using rand-update directly
;; (rather than rand), the way we would be forced
;; to proceed if we did not use assignment to model
;; local state:

(define (estimate-pi-2 trials)
  (sqrt (/ 6 (random-gcd-test trials random-init))))

(define (random-gcd-test trials initial-x)
  (define (iter trials-remaining trials-passed x)
    (let ((x1 (rand-update x)))
      (let ((x2 (rand-update x1)))
	(cond ((= trials-remaining 0)
	       (/ trials-passed trials))
	      ((= (gcd x1 x2) 1)
	       (iter (- trials-remaining 1)
		     (+ trials-passed 1)))
	      (else
	       (iter (- trials-remaining 1)
		     trials-passed
		     x2))))))
  (iter trials 0 initial-x))

;;;;;;;;;;;;;;;;;;
;; Exercise 3.5 ;;
;;;;;;;;;;;;;;;;;;

(define (square x)
  (* x x))

;; circle centered at 0
(define (within-unitary-circle? x y)
  (let ((radius 1)
	(center-x 0)
	(center-y 0))
    (not (> (+ (square (- x center-x))
	       (square (- y center-y)))
	    (square radius)))))

(define (estimate-pi-3 trials)
  (* 1.0 (estimate-integral within-unitary-circle?
		     -5 5 -5 5
		     trials)))

(define (random-in-range a b)
  (let ((range (- b a)))
    (+ a (* (random) range))))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (region-test)
    (P (random-in-range x1 x2)
       (random-in-range y1 y2)))
  (let ((rectangle-area (* (- x2 x1)
			   (- y2 y1))))
    (* (monte-carlo trials region-test)
       rectangle-area)))
   
;;;;;;;;;;;;;;;;;;
;; Exercise 3.6 ;;
;;;;;;;;;;;;;;;;;;

(define rand2
  (let ((x random-init))
    (lambda (action)
      (cond ((eq? action 'generate)
	     (set! x (rand-update x))
	     x)
	    ((eq? action 'reset)
	     (lambda (new-x)
	       (set! x new-x)
	       x))
	    (else
	     (error "Unknown action -- MY-RAND" action))))))

;;;;;;;;;;;;;;;;;;
;; Exercise 3.7 ;;
;;;;;;;;;;;;;;;;;;

(define (make-joint account original-passwd new-passwd)
  (lambda (passwd action)
    (if (eq? passwd new-passwd)
	(account original-passwd action)
	(lambda (x) "Incorrect password"))))

;;;;;;;;;;;;;;;;;;
;; Exercise 3.8 ;;
;;;;;;;;;;;;;;;;;;

;; (+ (f 0) (f 1))

(define f (let ((first-time-called #t))
	    (lambda (x)
	      (if first-time-called
		  (begin
		    (set! first-time-called #f)
		    x)
		  0))))

;;;;;;;;;;;;;;;;;;;
;; Exercise 3.13 ;;
;;;;;;;;;;;;;;;;;;;

(define (last-pair l)
  (if (null? (mcdr l))
      l
      (last-pair (mcdr l))))

(define (make-cycle l)
  (set-mcdr! (last-pair l) l)
  l)

;; si tratamos de evaluar (last-pair (make-cycle L))
;; entramos en un ciclo de cdr, cdr, ...

;;;;;;;;;;;;;;;;;;;
;; Exercise 3.14 ;;
;;;;;;;;;;;;;;;;;;;

(define (mystery x)
  (define (loop x y)
    (if (null? x)
	y
	(let ((temp (mcdr x)))
	  (set-mcdr! x y)
	  (loop temp x))))
  (loop x '()))

;; in general, mystery reverses the list fiven as an argument

;;;;;;;;;;;;;;;;;;;
;; Exercise 3.16 ;;
;;;;;;;;;;;;;;;;;;;

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
	 (count-pairs (cdr x))
	 1)))

(define (count-mpairs x)
  (if (not (mpair? x))
      0
      (+ (count-mpairs (mcar x))
	 (count-mpairs (mcdr x))
	 1)))

(define should-be-3 (mcons 1 (mcons 2 (mcons 3 '()))))
(define should-be-4 (mcons 1 (mcons 2 (mcons 3 '()))))
(set-mcar! should-be-4 (mcdr (mcdr should-be-4)))
(define should-be-7 (mcons 1 (mcons 2 (mcons 3 '()))))
(set-mcar! should-be-7 (mcdr should-be-7))
(set-mcar! (mcdr should-be-7) (mcdr (mcdr should-be-7)))
(define should-be-endless (mcons 1 (mcons 2 (mcons 3 '()))))
(set-mcar! (mcdr (mcdr should-be-endless)) should-be-endless)

;;;;;;;;;;;;;;;;;;;
;; Exercise 3.17 ;;
;;;;;;;;;;;;;;;;;;;

(define (refined-count-mpairs list)
  (let ((already-seen '()))
    (define (is-already-seen? node)
      (not (not (memq node already-seen))))
    (define (internal x)
      (cond ((or (and (mpair? x) (is-already-seen? x))
		 (not (mpair? x)))
	     0)
	    (else
	     (set! already-seen (cons x already-seen))
	     (+ (internal (mcar x))
		(internal (mcdr x))
		1))))
    (internal list)))

;;;;;;;;;;;;;;;;;;;
;; Exercise 3.18 ;;
;;;;;;;;;;;;;;;;;;;

(define (has-loop? list)
  (let ((already-seen '()))
    (define (is-already-seen? node)
      (not (not (memq node already-seen))))
    (define (add-to-seen node)
      (set! already-seen (cons node already-seen)))
    (define (internal x)
      (if (not (mpair? x))
	  #f
	  (begin
	    (add-to-seen x)
	    (if (is-already-seen? (mcdr x))
		#t
		(internal (mcdr x))))))
    (internal list)))

(define should-be-endless2 (mcons 1 (mcons 2 (mcons 3 '()))))
(set-mcdr! (mcdr (mcdr should-be-endless2)) should-be-endless2)

;;;;;;;;;;;;;;;;;;;
;; Exercise 3.21 ;;
;;;;;;;;;;;;;;;;;;;

(define (front-ptr queue)
  (mcar queue))
(define (rear-ptr queue)
  (mcdr queue))
(define (set-front-ptr! queue item)
  (set-mcar! queue item))
(define (set-rear-ptr! queue item)
  (set-mcdr! queue item))

;; QUEUE IMPLEMENTATION
;; constructor
(define (make-queue)
  (mcons '() '()))
;; accessors
(define (empty-queue? queue)
  (null? (front-ptr queue)))
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (mcar (front-ptr queue))))
;; mutators
(define (insert-queue! queue item)
  (let ((new-pair (mcons item null)))
    (cond ((empty-queue? queue)
	   (set-front-ptr! queue new-pair)
	   (set-rear-ptr! queue new-pair)
	   queue)
	  (else
	   (set-mcdr! (rear-ptr queue) new-pair)
	   (set-rear-ptr! queue new-pair)
	   queue))))
(define (delete-queue! queue)
  (cond ((empty-queue? queue)
	 (error "DELETE called with an empty queue" queue))
	(else
	 (set-front-ptr! queue (mcdr (front-ptr queue)))
	 queue)))

(define (print-queue queue)
  (define (print-queue-contents items)
    (cond ((null? items)
	   (display ""))
	  (else
	   (display (mcar items))
	   (display " ")
	   (print-queue-contents (mcdr items)))))
  (cond ((empty-queue? queue)
	 (display "[ ]")
	 (newline))
	(else
	 (display "[ ")
	 (print-queue-contents (front-ptr queue))
	 (display "]")
	 (newline))))

;;;;;;;;;;;;;;;;;;;
;; Exercise 3.22 ;;
;;;;;;;;;;;;;;;;;;;

(define (make-queue2)
  (let ((front-ptr '())
	(rear-ptr '()))
    ;; selectors
    (define (empty-queue?)
      (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
	  (error "FRONT called with an empty queue" front-ptr)
	  (mcar front-ptr)))
    ;; mutators
    (define (insert-queue! item)
      (let ((new-pair (mcons item null)))
	(cond ((empty-queue?)
	       (set! front-ptr new-pair)
	       (set! rear-ptr new-pair)
	       dispatch)
	      (else
	       (set-mcdr! rear-ptr new-pair)
	       (set! rear-ptr new-pair)
	       dispatch))))
    (define (delete-queue!)
      (cond ((empty-queue?)
	     (error "DELETE called with an empty queue"))
	    (else
	     (set! front-ptr (mcdr front-ptr)))))
    ;; dispatch
    (define (dispatch m)
      (cond ((eq? m 'empty-queue?)
	     (empty-queue?))
	    ((eq? m 'front-queue)
	     (front-queue))
	    ((eq? m 'insert-queue!)
	     insert-queue!)
	    ((eq? m 'delete-queue!)
	     (delete-queue!))
	    (else
	     (error "DISPATCH called with an unknown message:" m))))
    dispatch))

;;;;;;;;;;;;;;;;;;;
;; Exercise 3.23 ;;
;;;;;;;;;;;;;;;;;;;

(define (make-deque)
  (mcons '() '()))

;; predicate
(define (empty-deque? deque)
  (null? (front-ptr deque)))

;; selectors
(define (front-deque deque)
  (cond ((empty-deque? deque)
	 (error "EMPTY"))
	(else
	 (mcar (mcar (front-ptr deque))))))
(define (rear-deque deque)
  (cond ((empty-deque? deque)
	 (error "EMPTY"))
	(else
	 (mcar (mcar (rear-ptr deque))))))

;; mutators
(define (front-insert-deque! deque item)
  (let ((new-pair (mcons (mcons item null) null)))
  (cond ((empty-deque? deque)
	 (set-front-ptr! deque new-pair)
	 (set-rear-ptr! deque new-pair))
	(else
	 (set-previous-ptr! (front-ptr deque) new-pair)
	 (set-mcdr! new-pair (front-ptr deque))
	 (set-front-ptr! deque new-pair)))))
(define (rear-insert-deque! deque item)
  (let ((new-pair (mcons (mcons item null) null)))
  (cond ((empty-deque? deque)
	 (set-front-ptr! deque new-pair)
	 (set-rear-ptr! deque new-pair))
	(else
	 (set-previous-ptr! new-pair (rear-ptr deque))
	 (set-mcdr! (rear-ptr deque) new-pair)
	 (set-rear-ptr! deque new-pair)))))
(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
	 (error "EMPTY"))
	((null? (mcdr (front-ptr deque)))
	 (set-front-ptr! deque null)
	 (set-front-ptr! deque null))
	(else
	 (set-front-ptr! deque (mcdr (front-ptr deque)))
	 (set-previous-ptr! (front-ptr deque) null))))
(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
	 (error "EMPTY"))
	;; if one item
	((null? (previous-ptr (rear-ptr deque)))
	 (set-front-ptr! deque null)
	 (set-rear-ptr! deque null))
	(else
	 (set-rear-ptr! deque (previous-ptr (rear-ptr deque)))
	 (set-mcdr! (rear-ptr deque) null))))

(define (set-previous-ptr! item prev-item)
  (set-mcdr! (mcar item) prev-item))
(define (previous-ptr item)
  (mcdr (mcar item)))

;;;;;;;;;;;;;;;;;;;
;; Exercise 3.24 ;;
;;;;;;;;;;;;;;;;;;;

(define (make-table same-key?)
  (define (assoc key records)
    (cond ((null? records) #f)
	  ((same-key? key (mcar (mcar records))) (mcar records))
	  (else #f)))
  (let ((table (mcons '*table* '())))
    (define (lookup key)
      (let ((record (assoc key (mcdr table))))
	(if record
	    (mcdr record)
	    #f)))
    (define (insert key value)
      (let ((record (assoc key (mcdr table))))
	(if record
	    (set-mcdr! record value)
	    (set-mcdr! table
		       (mcons (mcons key value)
			      (mcdr table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
	    ((eq? m 'insert) insert)
	    (else (error "Unknown operation"))))
    dispatch))
    
;;;;;;;;;;;;;;;;;;;
;; Exercise 3.25 ;;
;;;;;;;;;;;;;;;;;;;

(define (make-recursive-table)
  (let ((table (mcons '*table* '())))
    (define (assoc key records)
      (cond ((null? records) #f)
	    ((eq? key (mcar (mcar records))) (mcar records))
	    (else (assoc key (mcdr records)))))
    (define (lookup keys)
      (define (iter remaining-keys subtable)
	(if (null? remaining-keys)
	    (mcdr subtable)
	    (let ((record (assoc (car remaining-keys)
				 (mcdr subtable))))
	      (cond ((and record (null? (cdr remaining-keys)))
		     (iter (cdr remaining-keys)
			   record))
		    ((and record (not (null? (cdr remaining-keys))))
		     (iter (cdr remaining-keys)
			   (mcdr record)))
		    (else #f)))))
      (iter keys table))
    (define (insert! keys value)
      (define (new-table not-so-dummy-value)
	(mcons not-so-dummy-value '()))
      (define (iter remaining-keys subtable)
	(if (null? remaining-keys)
	    (set-mcdr! subtable value)
	    (let ((record (assoc (car keys) (mcdr subtable))))
	      (cond ((and (not record) (null? (cdr remaining-keys)))
		     (set-mcdr! subtable
				(mcons (mcons (car remaining-keys)
					      value)
				       (mcdr subtable))))
		    ((and (not record) (not (null? (cdr remaining-keys))))
		     (set-mcdr! subtable
				(mcons (mcons (car remaining-keys)
					      (new-table '*dummy-table*))
				       (mcdr subtable)))
		     ;; itero con la tabla interna que acabo de crear
		     (iter (cdr remaining-keys) (mcdr (mcar (mcdr subtable)))))
		    ((and record (null? (cdr remaining-keys)))
		     (set-mcdr! value))
		    ((and record (not (null? (cdr remaining-keys))))
		     (iter (cdr remaining-keys) (mcdr record)))))))
      (iter keys table))
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
	    ((eq? m 'insert!) insert!)
	    (else (error "Unknown operation"))))
    dispatch))

;;;;;;;;;;;;;;;;;;;
;; Exercise 3.27 ;;
;;;;;;;;;;;;;;;;;;;

(define (fib n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (+ (fib (- n 1))
		 (fib (- n 2))))))

(define (memoize f)
  (let ((table (make-recursive-table)))
    (lambda (x)
      (let ((previously-computed-result ((table 'lookup) (list x))))
	(display "prev result? (")
	(display x)
	(display ") ")
	(display previously-computed-result)
	(newline)
	(or previously-computed-result
	    (let ((result (f x)))
	      ((table 'insert!) (list x) result)
	      result))))))
(define memo-fib
  (memoize (lambda (n)
	     (cond ((= n 0) 0)
		   ((= n 1) 1)
		   (else (+ (memo-fib (- n 1))
			    (memo-fib (- n 2))))))))

;;;;;;;;;;;;;;;;;;;
;; Exercise 3.28 ;;
;;;;;;;;;;;;;;;;;;;

;; definiciones para que no putee el (enter!..)
(define (get-signal w) 1)
(define (after-delay) 1)
(define or-gate-delay 1)
(define and-gate-delay 1)
(define inverter-delay 1)
(define (set-signal!) 1)
(define (add-action!) 1)
(define (make-wire) 1)
(define (inverter) 1)
(define (and-gate) 1)
(define (full-adder) 1)

(define (or-gate a1 a2 output)
  (define (or-action)
    (let ((new-value (logical-or (get-signal a1)
				 (get-signal a2))))
      (after-delay or-gate-delay
		   (lambda ()
		     (set-signal! output new-value)))))
  (add-action! a1 or-action)
  (add-action! a2 or-action)
  'ok)

(define (logical-or s1 s2)
  (or (= s1 1) (= s2 1)))

;;;;;;;;;;;;;;;;;;;
;; Exercise 3.29 ;;
;;;;;;;;;;;;;;;;;;;

(define (or-gate-alternative a1 a2 output)
  (let ((b1 (make-wire))
	(b2 (make-wire))
	(c (make-wire)))
    (inverter a1 b1)
    (inverter a2 b2)
    (and-gate b1 b2 c)
    (inverter c output)
    'ok))
;; the delay is: 2 * inverter-delay + and-gate-delay

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.30 ;;
;;;;;;;;;;;;;;;;;;;

;; (define (ripple-carry-adder a-wires b-wires s-wires carry-input carry-output)
;;   (define (iter as bs ss c)
;;     (cond ((null? as)
;; 	   'ready)
;; 	  (else
;; 	   (let ((c1 (if (null? (cdr as))
;; 			 carry-output
;; 			 (make-wire))))
;; 	     (full-adder (car a) (car b) c
;; 			 (car s) c1)
;; 	     (iter (cdr a) (cdr b) (cdr s) c1)))))
;;   (iter a-wires b-wires s-wires carry-input))

(define (ripple-carry-adder a-wires b-wires s-wires C)
  (if (null? a-wires)
      'ok
      (let ((an (car a-wires))
	    (bn (car b-wires))
	    (sn (car s-wires))
	    (cn (make-wire)))
	(full-adder an bn cn sn C)
	(ripple-carry-adder
	 (cdr a-wires) (cdr b-wires) (cdr s-wires) cn))))
				  
;;;;;;;;;;;;;;;;;;;
;; Exercise 3.31 ;;
;;;;;;;;;;;;;;;;;;;

;; The initialization is needed because it guarantees that
;; the function boxes behave correctly (have the correct output
;; for their inputs)

;;;;;;;;;;;;;;;;;;;
;; Exercise 3.32 ;;
;;;;;;;;;;;;;;;;;;;

;; The procedures to be run during each time segment of the agenda
;; must be kept in a queue to maintain the order in which they
;; were originated (and thus put in the queue).

;; If the inputs of an AND function changes from (0,1) to (1,0) at
;; the same time, using a list instead of a queue will produce an
;; incorrect output.

;; Altough in the simulation the wire's signal change at the same
;; time, they are processed in sequence, and that order must be
;; preserved to generate the appropriate output.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3.3.5 Propagation of Constraints ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
	   (set-value! sum
		       (+ (get-value a1) (get-value a2))
		       me-adder))
	  ((and (has-value? a1) (has-value? sum))
	   (set-value! a2
		       (- (get-value sum) (get-value a1))
		       me-adder))
	  ((and (has-value? a2) (has-value? sum))
	   (set-value! a1
		       (- (get-value sum) (get-value a2))
		       me-adder))))
  (define (process-forget-value)
    (forget-value! sum me-adder)
    (forget-value! a1 me-adder)
    (forget-value! a2 me-adder)
    (process-new-value))
  (define (me-adder request)
    (cond ((eq? request 'I-have-a-value)
	   (process-new-value))
	  ((eq? request 'I-lost-my-value)
	   (process-forget-value))
	  (else
	   (error "Unknown request -- ADDER" request))))
  (connect a1 me-adder)
  (connect a2 me-adder)
  (connect sum me-adder)
  me-adder)

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
	       (and (has-value? m2) (= (get-value m2) 0)))
	   (set-value! product 0 me-multip))
	  ((and (has-value? m1) (has-value? m2))
	   (set-value! product
		       (* (get-value m1) (get-value m2))
		       me-multip))
	  ((and (has-value? product) (has-value? m1))
	   (set-value! m2
		       (/ (get-value product) (get-value m1))
		       me-multip))
	  ((and (has-value? product) (has-value? m2))
	   (set-value! m1
		       (/ (get-value product) (get-value m2))
		       me-multip))))
  (define (process-forget-value)
    (forget-value! product me-multip)
    (forget-value! m1 me-multip)
    (forget-value! m2 me-multip)
    (process-new-value))
  (define (me-multip request)
    (cond ((eq? request 'I-have-a-value)
	   (process-new-value))
	  ((eq? request 'I-lost-my-value)
	   (process-forget-value))
	  (else
	   (error "Unknown request -- MULTIPLIER" request))))
  (connect m1 me-multip)
  (connect m2 me-multip)
  (connect product me-multip)
  me-multip)

(define (constant value connector)
  (define (me-const request)
    (error "Unknown CONSTANT" request))
  (connect connector me-const)
  (set-value! connector value me-const)
  me-const)

(define (probe name connector)
  (define (print-probe value)
    (newline)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me-probe request)
    (cond ((eq? request 'I-have-a-value)
	   (process-new-value))
	  ((eq? request 'I-lost-my-value)
	   (process-forget-value))
	  (else
	   (error "Unknown request -- PROBE" request))))
  (connect connector me-probe)
  me-probe)

(define (make-connector)
  (let ((value #f) (informant #f) (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me-conn))
	     (set! value newval)
	     (set! informant setter)
	     (for-each-except setter
			      inform-about-value
			      constraints))
	    ((not (= value newval))
	     (error "Contradiction" (list value newval)))
	    (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
	  (begin (set! informant #f)
		 (for-each-except retractor
				  inform-about-no-value
				  constraints))
	  'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
	  (set! constraints
	    (cons new-constraint constraints))
	  'already-connected)
      (if (has-value? me-conn)
	  (begin
	    (inform-about-value new-constraint))
	  'already-has-value)
      'done)
    (define (me-conn request)
      (cond ((eq? request 'has-value?)
	     (if informant #t #f))
	    ((eq? request 'value) value)
	    ((eq? request 'set-value!) set-my-value)
	    ((eq? request 'forget) forget-my-value)
	    ((eq? request 'connect) connect)
	    (else (error "Unknown operation -- CONNECTOR"
			 request))))
    me-conn))

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
	  ((eq? exception (car items))
	   (loop (cdr items)))
	  (else
	   (procedure (car items))
	   (loop (cdr items)))))
  (loop list))

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'value))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))
 
	     

(define (celcius-farenheit-converter c f)
  (let ((u (make-connector))
	(v (make-connector))
	(w (make-connector))
	(x (make-connector))
	(y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))
;; running the program	
(define C (make-connector))
(define F (make-connector))
(celcius-farenheit-converter C F)
(probe "Celcius temp" C)
(probe "Farenheit temp" F)

;;;;;;;;;;;;;;;;;;;
;; Exercise 3.33 ;;
;;;;;;;;;;;;;;;;;;;

(define (averager a b c)
  (let ((d (make-connector))
	(e (make-connector)))
    (adder a b d)
    (multiplier c e d)
    (constant 2 e)))

(define A (make-connector))
(define B (make-connector))
(define C (make-connector))
(probe "A" A)
(probe "B" B)
(probe "C" C)
(averager A B C)

;;;;;;;;;;;;;;;;;;;
;; Exercise 3.34 ;;
;;;;;;;;;;;;;;;;;;;

(define (squarer a b)
  (multiplier a a b))

(define A (make-connector))
(define B (make-connector))
(probe "A" A)
(probe "B" B)
(squarer A B)

;; Cuando el valor se pone desde la terminal "A", todo sale bien
;; Cuando se pone desde "B", al constraint multiplier todavia le
;; quedan 2 terminales sin valor y no puede calcular el valor
;; (por mas que sean el mismo connector)

;;;;;;;;;;;;;;;;;;;
;; Exercise 3.35 ;;
;;;;;;;;;;;;;;;;;;;

(define (squarer2 a b)
  (define (process-new-value)
    (cond ((has-value? b)
	   (if (< (get-value b) 0)
	       (error "square less than 0 -- SQUARER" (get-value b))
	       (set-value! a (sqrt (get-value b)) me-squarer)))
	  ((has-value? a)
	   (set-value! b (* (get-value a) (get-value a)) me-squarer))))
  (define (process-forget-value)
    (forget-value! a me-squarer)
    (forget-value! b me-squarer)
    (process-new-value))
  (define (me-squarer request)
    (cond ((eq? request 'I-have-a-value)
	   (process-new-value))
	  ((eq? request 'I-lost-my-value)
	   (process-forget-value))
	  (else
	   (error "Unknown request -- SQUARER" request))))
  (connect a me-squarer)
  (connect b me-squarer)
  me-squarer)

(define A (make-connector))
(define B (make-connector))
(probe "A" A)
(probe "B" B)
(squarer2 A B)
    
	
