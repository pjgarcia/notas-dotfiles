#lang racket/base

;;;;;;;;;;;;;;;;;;
;; Exercise 2.1 ;;
;;;;;;;;;;;;;;;;;;

(define (make-rat n d)
  (let ((g ((if (< d 0) - +) (gcd n d))))
    (cons (/ n g)
	  (/ d g))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
	    (* (denom x) (numer y))))

(define (equal-rat x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))


(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))
  
;;;;;;;;;;;;;;;;;;
;; Exercise 2.2 ;;
;;;;;;;;;;;;;;;;;;

(define (make-segment point-a point-b)
  (cons point-a point-b))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (average x y)
  (/ (+ x y) 2))

(define (midpoint-segment segment)
  (let ((start (start-segment segment))
	(end (end-segment segment)))
    (make-point (average (x-point end)
			 (x-point start))
		(average (y-point end)
			 (x-point start)))))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

;;;;;;;;;;;;;;;;;;
;; Exercise 2.3 ;;
;;;;;;;;;;;;;;;;;;

(define (make-rect base height)
  (cons base height))

(define (base-rect rect)
  (car rect))

(define (height-rect rect)
  (cdr rect))

(define (perimeter-rect rect)
  (+ (* 2 (base-rect rect))
     (* 2 (height-rect rect))))

(define (area-rect rect)
  (* (base-rect rect)
     (height-rect rect)))


;; second definition of make-rect, base-rect & height-rect

;; (define (square x)
;;   (* x x))

;; (define (make-rect seg-a seg-b seg-c seg-d)
;;   (cons (cons seg-a seg-b)
;; 	(cons seg-c seg-d)))

;; (define (module-segment segment)
;;   (let ((distance-x (x-point (start-segment segment)))
;; 	(distance-y (y-point (start-segment segment)))
;; 	(end-seg (end-segment segment)))
;;     (let ((translated (make-segment (make-point 0 0)
;; 				    (make-point (- (x-point end-seg) distance-x)
;; 						(- (y-point end-seg) distance-y)))))
;;       (sqrt (+ (square (y-point (end-segment translated)))
;; 	       (square (x-point (end-segment translated))))))))

;; (define (base-rect rect)
;;   (let ((base-seg (car (car rect))))
;;     (module-segment base-seg)))

;; (define (height-rect rect)
;;   (let ((height-seg (cdr (car rect))))
;;     (module-segment height-seg)))

;;;;;;;;;;;;;;;;;;
;; Exercise 2.5 ;;
;;;;;;;;;;;;;;;;;;

;; (define (cons a b)
;;   (* (expt 2 a)
;;      (expt 3 b)))

;; (define (logn n x)
;;   (/ (log x)
;;      (log n)))

;; (define (car x)
;;   (if (= 0 (remainder x 3))
;;       (car (/ x 3))
;;       (logn 2 x)))

;; (define (cdr x)
;;   (if (= 0 (remainder x 2))
;;       (cdr (/ x 2))
;;       (logn 3 x)))

;;;;;;;;;;;;;;;;;;
;; Exercise 2.6 ;;
;;;;;;;;;;;;;;;;;;

(define (inc x)
  (+ x 1))

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add n m)
  (lambda (f) (lambda (x) ((n f) ((m f) x)))))

;;;;;;;;;;;;;;;;;;
;; Exercise 2.7 ;;
;;;;;;;;;;;;;;;;;;
  
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
		(make-interval (/ 1.0 (upper-bound y))
			       (/ 1.0 (lower-bound y)))))

(define (make-interval x y)
  (cons x y))

(define (lower-bound x)
  (car x))

(define (upper-bound x)
  (cdr x))

;;;;;;;;;;;;;;;;;;
;; Exercise 2.8 ;;
;;;;;;;;;;;;;;;;;;

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
		 (- (upper-bound x) (lower-bound y))))

;;;;;;;;;;;;;;;;;;
;; Exercise 2.9 ;;
;;;;;;;;;;;;;;;;;;

(define (width-interval x)
  (/ (abs (- (lower-bound x) (upper-bound x)))
     2))


;; racket@sicp2.rkt> (define a (make-interval 3 5))
;; racket@sicp2.rkt> (define b (make-interval -5 7))
;; racket@sicp2.rkt> (= (width-interval (add-interval a b)) (+ (width-interval a) (width-interval b)))
;; #t
;; racket@sicp2.rkt> (= (width-interval (sub-interval a b)) (+ (width-interval a) (width-interval b)))
;; #t

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.10 ;;
;;;;;;;;;;;;;;;;;;;

(define (div-interval x y)
  (if (and (< (lower-bound y) 0) (> (upper-bound y) 0))
      (error "Cant divide by an interval that spans zero.")
      (mul-interval x 
		    (make-interval (/ 1.0 (upper-bound y))
				   (/ 1.0 (lower-bound y))))))
