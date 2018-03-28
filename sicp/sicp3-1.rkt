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
	    (else #f)))   
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
;; Exercise 3.28 ;;
;;;;;;;;;;;;;;;;;;;

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
				  
