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
