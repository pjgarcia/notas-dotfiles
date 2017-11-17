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
;; Exercise 2.4 ;;
;;;;;;;;;;;;;;;;;;

;; (define (cons x y)
;;   (lambda (m) (m x y)))

;; (define (car x)
;;   (x (lambda (n d) n)))

;; (define (cdr x)
;;   (x (lambda (n d) d)))

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

;; (define (sub-interval x y)
;;   (make-interval (- (lower-bound x) (upper-bound y))
;; 		 (- (upper-bound x) (lower-bound y))))

(define (sub-interval x y)
  (add-interval x (make-interval (- (upper-bound y))
				 (- (lower-bound y)))))
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

;; (define (div-interval x y)
;;   (if (and (< (lower-bound y) 0) (> (upper-bound y) 0))
;;       (error "Cant divide by an interval that spans zero.")
;;       (mul-interval x 
;; 		    (make-interval (/ 1.0 (upper-bound y))
;; 				   (/ 1.0 (lower-bound y))))))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.11 ;;
;;;;;;;;;;;;;;;;;;;


;; racket@sicp2.rkt> (define both-pos-int (make-interval 2 4))
;; racket@sicp2.rkt> (define pos-pos-int (make-interval 2 4))
;; racket@sicp2.rkt> (define neg-pos-int (make-interval -2 4))
;; racket@sicp2.rkt> (define neg-neg-int (make-interval -4 -2))
;; racket@sicp2.rkt> (mul-interval pos-pos-int pos-pos-int)
;; '(4 . 16)
;; racket@sicp2.rkt> (mul-interval pos-pos-int neg-pos-int)
;; '(-8 . 16)
;; racket@sicp2.rkt> (mul-interval pos-pos-int neg-neg-int) 
;; '(-16 . -4)
;; racket@sicp2.rkt> (mul-interval neg-pos-int pos-pos-int)
;; '(-8 . 16)
;; racket@sicp2.rkt> (mul-interval neg-pos-int neg-pos-int)
;; '(-8 . 16)
;; racket@sicp2.rkt> (mul-interval neg-pos-int neg-neg-int)
;; '(-16 . 8)
;; racket@sicp2.rkt> (mul-interval neg-neg-int pos-pos-int)
;; '(-16 . -4)
;; racket@sicp2.rkt> (mul-interval neg-neg-int neg-pos-int)
;; '(-16 . 8)
;; racket@sicp2.rkt> (mul-interval neg-neg-int neg-neg-int)
;; '(4 . 16)

;; (define (mul-interval x y)
;;   (let ((a (lower-bound x))
;; 	(b (upper-bound x))
;; 	(c (lower-bound y))
;; 	(d (upper-bound y)))
;;     (cond
;;      ((and (and (positive? a) (positive? b)) (and (positive? c) (positive? d)))
;;       (make-interval (* a c) (* b d)))
;;      ((and (and (positive? a) (positive? b)) (and (negative? c) (positive? d)))
;;       (make-interval (* b c) (* b d)))
;;      ((and (and (positive? a) (positive? b)) (and (negative? c) (negative? d)))
;;       (make-interval (* b c) (* a d)))
;;      ((and (and (negative? a) (positive? b)) (and (positive? c) (positive? d)))
;;       (make-interval (* a d) (* b d)))
;;      ((and (and (negative? a) (positive? b)) (and (negative? c) (positive? d)))
;;       (let ((p (* a c))
;; 	    (q (* a d))
;; 	    (r (* b c))
;; 	    (s (* b d)))
;; 	(make-interval (min p q r s) (max p q r s))))
;;      ((and (and (negative? a) (positive? b)) (and (negative? c) (negative? d)))
;;       (make-interval (* b c) (* a c)))
;;      ((and (and (negative? a) (negative? b)) (and (positive? c) (positive? d)))
;;       (make-interval (* a d) (* b c)))
;;      ((and (and (negative? a) (negative? b)) (and (negative? c) (positive? d)))
;;       (make-interval (* a d) (* a c)))
;;      ((and (and (negative? a) (negative? b)) (and (negative? c) (negative? d)))
;;       (make-interval (* b d) (* a c))))))


;; some extra definitions from the book (page 95)

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (average (lower-bound i) (upper-bound i)))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.12 ;;
;;;;;;;;;;;;;;;;;;;

(define (make-center-percent c p)
  (let ((width (abs (* c (/ p 100)))))
    (make-center-width c width)))

(define (percent i)
  (abs (/ (* 100 (width i))
	  (center i))))


;; some extra definitions from the book (page 96)

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
		(add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
		  (add-interval (div-interval one r1)
				(div-interval one r2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FALTAN 13, 14, 15 & 16 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.17 ;;
;;;;;;;;;;;;;;;;;;;

(define (last-pair l)
  (if (= 1 (length l))
      l
      (last-pair (cdr l))))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.18 ;;
;;;;;;;;;;;;;;;;;;;

(define (reverse l)
  (cond ((null? l) l)
	((= 1 (length l)) l)
	(else (append (reverse (cdr l)) (list (car l))))))

(define (reverse-iter l)
  (define (iter list res)
    (if (null? list)
	res
	(iter (cdr list) (cons (car list) res))))
  (iter l null))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.19 ;;
;;;;;;;;;;;;;;;;;;;

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
	((or (< amount 0) (no-more? coin-values)) 0)
	(else (+ (cc amount
		     (except-first-denomination coin-values))
		 (cc (- amount
			(fist-denomination coin-values))
		     coin-values)))))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (no-more? coins)
  (= 0 (length coins)))

(define (fist-denomination kinds-of-coins)
  (car kinds-of-coins))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define us-coins (list 1 5 10 25 50))
(define (count-change amount)
  (cc amount us-coins))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.20 ;;
;;;;;;;;;;;;;;;;;;;

(define (same-parity first . rest)
  (define (iter items res)
    (if (= 0 (length items))
	res
	(let ((head (car items)))
	  (if (even? (+ head first))
	      (iter (cdr items) (append res (list (car items))))
	      (iter (cdr items) res)))))
  (iter (cons first rest) null))
	  

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.21 ;;
;;;;;;;;;;;;;;;;;;;

(define (square x)
  (* x x))

(define (square-list items)
  (if (null? items)
      null
      (cons (square (car items))
	    (square-list (cdr items)))))

(define (square-list items)
  (map square items))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.22 ;;
;;;;;;;;;;;;;;;;;;;

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (append answer (list (square (car things)))))))
  (iter items null))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.23 ;;
;;;;;;;;;;;;;;;;;;;

(define (for-each f items)
  (map f items)
  #t)

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.24 ;;
;;;;;;;;;;;;;;;;;;;

;; hecho en papel :)

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.25 ;;
;;;;;;;;;;;;;;;;;;;

(car (cdr (car (cdr (cdr (list 1 3 (list 5 7) 9))))))

(car (car (list (list 7))))

(cadr (cadr (cadr (cadr (cadr (cadr (list 1
					  (list 2
						(list 3
						      (list 4
							    (list 5
								  (list 6 7))))))))))))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.26 ;;
;;;;;;;;;;;;;;;;;;;

(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y) ;; '(1 2 3 4 5 6)
(cons x y) ;; '((1 2 3) 4 5 6)
(list x y) ;; '((1 2 3) (4 5 6))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.27 ;;
;;;;;;;;;;;;;;;;;;;

(define (deep-reverse l)
  (cond ((null? l) l)
	((pair? l) (append (deep-reverse (cdr l))
			   (list (deep-reverse (car l)))))
	(else l)))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.28 ;;
;;;;;;;;;;;;;;;;;;;

(define (fringe tree)
  (cond ((null? tree) tree)
	((pair? (car tree))
	 (append (fringe (car tree)) (fringe (cdr tree))))
	(else (cons (car tree) (fringe (cdr tree))))))
