#lang racket

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

(define A1 (make-connector))
(define B1 (make-connector))
(define C1 (make-connector))
(probe "A" A1)
(probe "B" B1)
(probe "C" C1)
(averager A1 B1 C1)

;;;;;;;;;;;;;;;;;;;
;; Exercise 3.34 ;;
;;;;;;;;;;;;;;;;;;;

(define (squarer a b)
  (multiplier a a b))

(define A2 (make-connector))
(define B2 (make-connector))
(probe "A" A2)
(probe "B" B2)
(squarer A2 B2)

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
    
;;;;;;;;;;;;;;;;;;;
;; Exercise 3.37 ;;
;;;;;;;;;;;;;;;;;;;

(define (celcius-farenheit-converter2 x)
  (c+ (c* (c/ (cv 9) (cv 5))
	  x)
      (cv 32)))

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ z y)
  (let ((x (make-connector)))
    (multiplier x y z)
    x))

(define (cv v)
  (let ((c (make-connector)))
    (constant v c)
    c))

(define C2 (make-connector))
(define F2 (celcius-farenheit-converter2 C2))


    
;;;;;;;;;;;;;;;;;;;
;; Exercise 3.44 ;;
;;;;;;;;;;;;;;;;;;;

;; There is no need to use a more sophisticated method to
;; transfer money between accounts.
;; The essential difference between the transfer problem
;; and the exchange problem is that the exchange must be
;; an atomic operation. That is, the two transacctions
;; must be done at the same time (with no modifications
;; to the accounts in between). The transfer process is
;; different in that the deposit in the destination account
;; can be preceded with modifications to both accounts.

;;;;;;;;;;;;;;;;;;;
;; Exercise 3.45 ;;
;;;;;;;;;;;;;;;;;;;

;; What is wrong with Louis's reasoning is that when
;; serialized-exchange is called, it internally calls
;; withdraw and deposit, two operations that are
;; prohibited to run at the same time that the exchange
;; process. Thus, they will block.

;;;;;;;;;;;;;;;;;;;
;; Exercise 3.47 ;;
;;;;;;;;;;;;;;;;;;;

(define make-mutexes 'mock)

;; a - in terms of mutexes
(define (make-semaphore n)
  (let ((mutexes (make-mutexes n))
	(current-mutex-idx -1))
    (define (aquire-mutex!)
      (define (iter ms index)
	(cond ((null? ms) (aquire-mutex!))
	      ((not (> index current-mutex-idx))
	       (iter (cdr ms) (+ index 1)))
	      (else
	       ((car ms) 'aquire)
	       (set! current-mutex-idx index))))
      (iter mutexes 0))
    (define (release-mutex!)
      (define (iter ms index)
	(cond ((eq? index current-mutex-idx)
	       ((car ms) 'release)
	       (set! current-mutex-idx (- current-mutex-idx 1)))
	      ((< index current-mutex-idx)
	       (iter (cdr ms) (+ index 1)))))
      (iter mutexes 0))
    (define (the-semaphore m)
      (cond ((eq? m 'aquire) (aquire-mutex!))
	    ((eq? m 'release) (release-mutex!))
	    (else (error "Unknown operation THE-SEMAPHORE" m))))
    the-semaphore))
	      
  
;; b - in terms of atomic test-and-set!
(define (make-semaphore2 n)
  (define (make-n-sized-cells-list n)
    (cond ((eq? n 0) null)
	  (else (cons #f
		      (make-n-sized-cells-list (- n 1))))))
  (let ((cells (make-n-sized-cells-list n)))
    (define (the-semaphore m)
      (cond ((eq? m 'aquire)
	     (if (test-and-set! cells)
		 (the-semaphore 'aquire)
		 'aquired)) ;; retry
	    ((eq? m 'release) (clear-first! cells))))
    the-semaphore))

(define (clear-first! cells)
  (cond ((null? cells)
	 (error "No cells to clear -- CLEAR-FIRST!"))
	((car cells)
	 (set-mcar! cells #f)
	 "cleared!")
	(else
	 (clear-first! (cdr cells)))))

(define (test-and-set! cells)
  (cond ((null? cells) #t)
	((car cells) (test-and-set! (cdr cells)))
	(else (set-mcar! cells #t)
	      #f)))

;;;;;;;;;;;;;;;;;;;
;; Exercise 3.48 ;;
;;;;;;;;;;;;;;;;;;;

(define exchange 'mock)

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
	(serializer2 (account2 'serializer)))
    (if (< (account1 'number) (account2 'number))
	(serializer1 (serializer2 exchange))
	(serializer2 (serializer1 exchange)))
     account1
     account2))

;;;;;;;;;;;;;;;;;;;
;; Exercise 3.50 ;;
;;;;;;;;;;;;;;;;;;;

(define (stream-map proc . argstreams)
  (if (stream-empty? (car argstreams))
      empty-stream
      (stream-cons
       (apply proc (map stream-first argstreams))
       (apply stream-map
	      (cons proc (map stream-rest argstreams))))))

;;;;;;;;;;;;;;;;;;;
;; Exercise 3.51 ;;
;;;;;;;;;;;;;;;;;;;

;; (define x (stream-map show (stream-enumerate-interval 0 10)))
;; >> 0
;; x -> (cons 0 (delay (stream-map show (stream-rest (cons 0 (stream-enumerate-interval 1 10))))))
;; (stream-ref x 5)
;; >> 1
;; >> 2
;; >> 3
;; >> 4
;; >> 5
;; 5
;; (stream-ref x 7)
;; >> 6
;; >> 7
;; 7

;;;;;;;;;;;;;;;;;;;
;; Exercise 3.52 ;;
;;;;;;;;;;;;;;;;;;;

;; (define seq (stream-map accum (stream-enumerate-interval 1 20)))
;; 1 1
;; sum = 1
;; (define y (stream-filter even? seq))
;; 2 3, 3 6
;; sum = 6
;; (define z (stream-filter (lambda (x) (= (remainder x 5) 0)) seq))
;; 4 10, 5 15
;; sum = 10
;; (stream-ref y 7)
;; 6 21, 7 28, 8 36, 9 45, 10 55, 11 66, 12 78, 13 91, 14 105, 15 120, 16 136
;; sum = 136
;; >> 136
;; (display-stream z)
;; 17 153, 18 171, 19 190, 20 210
;; 10 15 45 55 105 120 190 210
;; sum = 210

;; These responses would differ if we had implemented delay without using the
;; optimization.
;; Each time we evaluate stream-rest, the delay is forced to be evaluated but
;; it remembers the value if it was called previously. 
;; That means that accum (in stream-map) will not be called again and modify
;; the sum value.

;;;;;;;;;;;;;;;;;;;
;; Exercise 3.53 ;;
;;;;;;;;;;;;;;;;;;;

;; s = (1 2 4 8 16 32...)

;;;;;;;;;;;;;;;;;;;
;; Exercise 3.54 ;;
;;;;;;;;;;;;;;;;;;;

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))


(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorials (stream-cons 1 (mul-streams (stream-rest integers)
					       factorials)))

;; code needed
(define (add-streams s1 s2)
  (stream-map + s1 s2))

;;;;;;;;;;;;;;;;;;;
;; Exercise 3.55 ;;
;;;;;;;;;;;;;;;;;;;

;; (define (partial-sums stream)
;;   (let ((result (stream-cons (stream-first stream)
;; 			     (add-streams result (cdr stream)))))
;;     result))

;; mejor opcion (sacada de http://community.schemewiki.org/?sicp-ex-3.55)
(define (partial-sums s)
  (define result (add-streams s (stream-cons 0 result)))
  result)

;; code needed
(define (merge s1 s2)
  (cond ((stream-empty? s1) s2)
	((stream-empty? s2) s1)
	(else
	 (let ((s1car (stream-first s1))
	       (s2car (stream-first s2)))
	   (cond ((< s1car s2car)
		  (stream-cons s1car
			       (merge (stream-rest s1) s2)))
		 ((> s1car s2car)
		  (stream-cons s2car
			       (merge s1 (stream-rest s2))))
		 (else
		  (stream-cons
		   s1car
		   (merge (stream-rest s1)
			  (stream-rest s2)))))))))

;; code needed
(define (scale-stream s n)
  (stream-map (lambda (x) (* x n)) s))
;;;;;;;;;;;;;;;;;;;
;; Exercise 3.56 ;;
;;;;;;;;;;;;;;;;;;;

(define S (stream-cons 1 (merge (scale-stream S 2)
				(merge (scale-stream S 3)
 				       (scale-stream S 5)))))

;;(1 2 3 4 5 6 8 9 10 12 ...)

;;;;;;;;;;;;;;;;;;;
;; Exercise 3.57 ;;
;;;;;;;;;;;;;;;;;;;

;; to compute the nth fibonacci number, n-2 additions are performed
;; (0 1 1 2 3 5 8 13 ...)

;; if we had implemented delay as noted (without the memoization)
;; the ammount of additions would increase exponentially since,
;; for each value, each delayed evaluation in the stream would
;; trigger the evaluation of the preceding ones, up to the beggining

;;;;;;;;;;;;;;;;;;;
;; Exercise 3.59 ;;
;;;;;;;;;;;;;;;;;;;

(define (integrate-series series)
  (stream-map (lambda (a i) (/ a i)) series integers))

(define exp-series
  (stream-cons 1 (integrate-series exp-series)))

(define cosine-series
  (stream-cons 1 (stream-map (lambda (a) (* -1 a)) (integrate-series sine-series))))

(define sine-series
  (stream-cons 0 (integrate-series cosine-series)))

;;;;;;;;;;;;;;;;;;;
;; Exercise 3.60 ;;
;;;;;;;;;;;;;;;;;;;

(define (mul-series s1 s2)
  (stream-cons (* (stream-first s1)
		  (stream-first s2))
	       (add-streams (scale-stream (stream-first s1) (stream-rest s2))
			    (mul-series (stream-rest s1) s2))))

;;;;;;;;;;;;;;;;;;;
;; Exercise 3.61 ;;
;;;;;;;;;;;;;;;;;;;

(define (invert-unit-series s)
  (define x (stream-cons
	     1 (scale-stream -1 (mul-series (stream-rest s) x))))
  x)

;;;;;;;;;;;;;;;;;;;
;; Exercise 3.62 ;;
;;;;;;;;;;;;;;;;;;;
						       
(define (div-series s1 s2)
  (cond ((= 0 (stream-first s2))
	 (error "cant divide by 0"))))
;; TODO

;;;;;;;;;;;;;;;;;;;
;; Exercise 3.63 ;;
;;;;;;;;;;;;;;;;;;;

;; each time a new value is required from the stream, (stream-rest),
;; a new (sqrt-stream x) is called, wich returns the first
;; element of the created stream 1.0 and it is passed by
;; the various (sqrt-improve quess x) on its way up of the
;; calling (sqrt-stream x), thus re-doing all the improvements
;; to each element.

;; In contrast, the correct implementation always improves only
;; the last constructed element of the same stream.

;; the two versions wil not differ in efficiency, since while one
;; re-evaluates the previous calculations, the other re-do
;; the calculations on the succesive streams it creates

;;;;;;;;;;;;;;;;;;;
;; Exercise 3.64 ;;
;;;;;;;;;;;;;;;;;;;

(define (stream-limit s tol)
  (let ((first (stream-first s))
	(second (stream-first (stream-rest s))))
    (cond ((<= (abs (- first second)) tol)
	   second)
	  (else
	   (stream-limit (stream-rest s) tol)))))

;;;;;;;;;;;;;;;;;;;
;; Exercise 3.65 ;;
;;;;;;;;;;;;;;;;;;;

;; #1
(define (log-2-summands n)
  (stream-cons
   (/ 1 n)
   (stream-map - (log-2-summands (+ n 1)))))
(define log-2-stream
  (partial-sums (log-2-summands 1)))

;; #2
;;(euler-transform log-2-stream)

;; #3
;;(accelerated-sequence euler-transform log-2-stream)

;;;;;;;;;;;;;;;;;;;
;; Exercise 3.66 ;;
;;;;;;;;;;;;;;;;;;;

;; the pair (1, 100) will have (98 x 2) + 1 pairs preceding it
;; computes the position in the
(define (place-in-row s t)
  (cond ((= (- t s) 0) 1)
	(else (* (- t s) 2))))

(define (place-in-upper-row n)
  (+ 1 (* 2 n)))

(define (final-place s t)
  (cond ((= 1 s)
	 (place-in-row s t))
	(else
	 (place-in-upper-row (final-place (- s 1) (- t 1))))))

;; code needed
(define (interleave s1 s2)
  (cond ((stream-empty? s1) s2)
	(else
	 (stream-cons (stream-first s1)
		      (interleave s2 (stream-rest s1))))))
;;;;;;;;;;;;;;;;;;;
;; Exercise 3.67 ;;
;;;;;;;;;;;;;;;;;;;

(define (pairs s t)
  (stream-cons
   (list (stream-first s) (stream-first t))
   (interleave
    (interleave (stream-map (lambda (x) (list (stream-first s) x))
			    (stream-rest t))
		(stream-map (lambda (x) (list x (stream-first t)))
			    (stream-rest s)))
    (pairs (stream-rest s) (stream-rest t)))))

;;;;;;;;;;;;;;;;;;;
;; Exercise 3.68 ;;
;;;;;;;;;;;;;;;;;;;

;; when we evaluate (pairs integers integers) using Louis's definition
;; there will be an infinite loop because the interleave evaluation will
;; cause (pairs (stream-rest s) (stream-rest t)) to be evlauated, which in
;; turn will cause a new interleave to be evaluated and so on...

;;;;;;;;;;;;;;;;;;;
;; Exercise 3.69 ;;
;;;;;;;;;;;;;;;;;;;
	  
(define (triples s t u)
  (let ((doubles (pairs s t)))
    (define (iter st u)
      (stream-cons 
       (list (car (stream-first st))
	     (cadr (stream-first st))
	     (stream-first u))
       (interleave
	(stream-map (lambda (x)
		      (list (car (stream-first st))
			    (cadr (stream-first st))
			    x))
		    (stream-rest u))
	(iter (stream-rest st)
	      (stream-rest u)))))
    (iter doubles u)))
      
(define pythagorean-triples
  (stream-filter
   (lambda (triple)
     (let ((i (car triple))
	   (j (cadr triple))
	   (k (caddr triple)))
       (= (+ (square i) (square j))
	  (square k))))
   (triples integers integers integers)))
	     
;;;;;;;;;;;;;;;;;;;
;; Exercise 3.70 ;;
;;;;;;;;;;;;;;;;;;;

(define (merge-weighted s1 s2 weight)
  (cond ((stream-empty? s1) s2)
	((stream-empty? s2) s1)
	(else
	 (let ((w1 (weight (stream-first s1)))
	       (w2 (weight (stream-first s2))))
	   (cond ((< w1 w2)
		  (stream-cons (stream-first s1)
			       (merge-weighted (stream-rest s1) s2 weight)))
		 ((> w1 w2)
		  (stream-cons (stream-first s2)
			       (merge-weighted s1 (stream-rest s2) weight)))
		 (else
		  (stream-cons
		   (stream-first s1)
		   (stream-cons
		    (stream-first s2)
		    (merge-weighted (stream-rest s1)
				    (stream-rest s2)
				    weight)))))))))

(define (weighted-pairs s t weight)
  (stream-cons
   (list (stream-first s) (stream-first t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-first s) x))
		(stream-rest t))
    (weighted-pairs (stream-rest s) (stream-rest t) weight)
    weight)))

(weighted-pairs integers integers
		(lambda (pair)
		  (+ (car pair) (cadr pair))))


(define (divisible-by-2-3-5 n)
  (or (= (remainder n 2) 0)
      (= (remainder n 3) 0)
      (= (remainder n 5) 0)))
	    
(define (not-divisible pair)
  (let ((i (car pair)) (j (cadr pair)))
    (and (not (divisible-by-2-3-5 i))
	 (not (divisible-by-2-3-5 j)))))

(stream-filter not-divisible
	       (weighted-pairs
		integers integers
		(lambda (pair)
		  (+ (* 2 (car pair))
		     (* 3 (cadr pair))
		     (* 5 (car pair) (cadr pair))))))

;;;;;;;;;;;;;;;;;;;
;; Exercise 3.71 ;;
;;;;;;;;;;;;;;;;;;;
	       
(define (ramanujan-weight pair)
  (+ (expt (car pair) 3)
     (expt (cadr pair) 3)))
  
(define (same-weight s weight)
  (let ((s1 (stream-first s))
	(s2 (stream-first (stream-rest s))))
    (if (= (weight s1)
	   (weight s2))
	(stream-cons (weight s1)
		     (same-weight (stream-rest s) weight))
	(same-weight (stream-rest s) weight))))

(define (stream-take s n)
  (cond ((= 0 n) null)
	(else (cons (stream-first s)
		    (stream-take (stream-rest s)
				 (- n 1))))))

(stream-take (same-weight
	      (weighted-pairs integers integers
			      ramanujan-weight)
	      ramanujan-weight)
	     5)

;;;;;;;;;;;;;;;;;;;
;; Exercise 3.72 ;;
;;;;;;;;;;;;;;;;;;;

(define (same-weight2 s weight)
  (let ((s1 (stream-first s))
	(s2 (stream-first (stream-rest s)))
	(s3 (stream-first (stream-rest (stream-rest s)))))
    (if (= (weight s1)
	   (weight s2)
	   (weight s3))
	(stream-cons (list (weight s1) s1 s2 s3)
		     (same-weight2 (stream-rest s) weight))
	(same-weight2 (stream-rest s) weight))))

(define (square-weight pair)
  (+ (square (car pair))
     (square (cadr pair))))

(stream-take (same-weight2
	      (weighted-pairs integers integers
			      square-weight)
	      square-weight)
	     5)

;;;;;;;;;;;;;;;;;;;
;; Exercise 3.73 ;;
;;;;;;;;;;;;;;;;;;;

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
		 (add-streams (scale-stream integrand dt)
			      int)))
  int)

(define (RC r c dt)
  (lambda (i v0)
    (add-streams (scale-stream r i)
		 (integral
		  (scale-stream (/ 1 c) i) v0) dt)))

;;;;;;;;;;;;;;;;;;;
;; Exercise 3.74 ;;
;;;;;;;;;;;;;;;;;;;
  
(define (make-zero-crossings input-stream last-value)
  (cons-stream
   (sign-change-detector (stream-car input-stream) last-value)
   (make-zero-crossings (stream-cdr input-stream)
			(stream-car input-stream))))

(define zero-crossings
  (stream-map sign-change-detector sense-data (cons-stream 0 sense-data)))

;;;;;;;;;;;;;;;;;;;
;; Exercise 3.75 ;;
;;;;;;;;;;;;;;;;;;;

(define (make-zero-crossings2 input-stream last-avpt-value last-orig-value)
  (let ((avpt (/ (+ (stream-car input-stream) last-orig-value) 2)))
    (cons-stream (sign-change-detector avpt last-avpt-value)
		 (make-zero-crossings2 (stream-cdr input-stream)
				       avpt
				       (stream-car input-stream)))))

;;;;;;;;;;;;;;;;;;;
;; Exercise 3.76 ;;
;;;;;;;;;;;;;;;;;;;

(define (smooth signal)
  (let ((avpt (/ (+ (stream-car signal)
		    (stream-car (stream-cdr signal)))
		 2)))
    (cons-stream avpt
		 (smooth (stream-cdr signal)))))

(define (make-zero-crossings3 input-stream)
  (let ((smoothed (smooth input-stream)))
    (cons-stream (sign-change-detector (stream-car smoothed)
				       (stream-cdr (stream-car smoothed)))
		 (make-zero-crossings3 (stream-cdr input-stream)))))
		 
;;;;;;;;;;;;;;;;;;;
;; Exercise 3.78 ;;
;;;;;;;;;;;;;;;;;;;

(define (solve-2nd a b dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams (scale-stream a dy)
			   (scale-stream b y)))
  y)


;;;;;;;;;;;;;;;;;;;
;; Exercise 3.80 ;;
;;;;;;;;;;;;;;;;;;;

(define (RLC R L C dt)
  (lambda (vc0 il0)
    (define il (integral (delay dil) il0 dt))
    (define vc (integral (delay dvc) vc0 dt))
    (define dvc (scale-stream (/ (- 1) C) il))
    (define dil (add-streams (scale-stream (/ (- R) L) il)
			     (scale-stream (/ 1 L) vc)))
    (stream-map (lambda (vcn iln) (cons vcn iln))
		il vc)))
			
		  
     
