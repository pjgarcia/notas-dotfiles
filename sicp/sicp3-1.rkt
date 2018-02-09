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

(define random-init "test")
(define rand-update "test")

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
   
