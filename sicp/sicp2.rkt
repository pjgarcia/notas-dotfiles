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

;;(define us-coins (list 1 5 10 25 50))
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

;; (define (square-list items)
;;   (map square items))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.22 ;;
;;;;;;;;;;;;;;;;;;;

;; (define (square-list items)
;;   (define (iter things answer)
;;     (if (null? things)
;; 	answer
;; 	(iter (cdr things)
;; 	      (append answer (list (square (car things)))))))
;;   (iter items null))

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

(define (fring tree)
  (cond ((null? tree) tree)
	((not (pair? tree)) (list tree))
	(else (append (fring (car tree))
		      (fring (cdr tree))))))
	 
	      

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.29 ;;
;;;;;;;;;;;;;;;;;;;

;; Part 1

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;; (define (left-branch mobile)
;;   (car mobile))

;; (define (right-branch mobile)
;;   (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))
 
;; Part 2

(define (total-weight mobile)
  (define (structure-weight structure)
    (if (not (pair? structure))
	structure
	(+ (structure-weight (branch-structure (left-branch structure)))
	   (structure-weight (branch-structure (right-branch structure))))))
  (structure-weight mobile))

;; Part 3

(define (balanced? mobile)
  (define (torque branch)
    (* (total-weight (branch-structure branch))
       (branch-length branch)))
  (define (structure-balanced? structure)
    (if (not (pair? structure))
	#t
	(let ((left (left-branch structure))
	      (right (right-branch structure)))
	  (and (structure-balanced? (branch-structure left))
	       (structure-balanced? (branch-structure right))
	       (= (torque left) (torque right))))))
  (structure-balanced? mobile))

;; Part 4

;; (define (make-mobile left right)
;;   (cons left right))

;; (define (make-branch leng structure)
;;   (cons length structure))

;; (define (left-branch mobile)
;;   (car mobile))
;; (define (right-branch mobile)
;;   (cdr mobile))

;; (define (branch-length branch)
;;   (car branch))
;; (define (branch-structure branch)
;;   (cdr branch))
  

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.30 ;;
;;;;;;;;;;;;;;;;;;;

(define (square-tree tree)
  (map (lambda (subtree)
	 (if (pair? subtree)
	     (square-tree subtree)
	     (square subtree)))
       tree))

;; (define (square-tree tree)
;;   (cond ((null? tree) null)
;; 	((pair? tree) (cons (square-tree (car tree))
;; 			    (square-tree (cdr tree))))
;; 	(else (square tree))))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.31 ;;
;;;;;;;;;;;;;;;;;;;
	 
(define (tree-map f tree)
  (map (lambda (subtree)
	 (if (pair? subtree)
	     (tree-map f subtree)
	     (f subtree)))
       tree))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.32 ;;
;;;;;;;;;;;;;;;;;;;
	 
(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
	(append rest (map (lambda (subset)
			    (append subset (list (car s))))
			  rest)))))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.33 ;;
;;;;;;;;;;;;;;;;;;;

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

;; (define (map p sequence)
;;   (accumulate (lambda (x y)
;; 		(cons (p x) y))
;; 	      null
;; 	      sequence))

;; (define (append seq1 seq2)
;;   (accumulate cons seq2 seq1))

;; (define (length sequence)
;;   (accumulate (lambda (x y)
;; 		(+ 1 y))
;; 	      0
;; 	      sequence))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.34 ;;
;;;;;;;;;;;;;;;;;;;

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
		(+ (* higher-terms x)
		   this-coeff))
	      0
	      coefficient-sequence))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.35 ;;
;;;;;;;;;;;;;;;;;;;

(define (count-leaves t)
  (accumulate + 0 (map (lambda (subtree)
			 (if (pair? subtree)
			     (count-leaves subtree)
			     1))
		       t)))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.36 ;;
;;;;;;;;;;;;;;;;;;;

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map car seqs))
	    (accumulate-n op init (map cdr seqs)))))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.37 ;;
;;;;;;;;;;;;;;;;;;;

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))


(define (transpose mat)
  (accumulate-n cons null mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.38 ;;
;;;;;;;;;;;;;;;;;;;

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest))
	      (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)

;; op should be commutative

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.39 ;;
;;;;;;;;;;;;;;;;;;;

;; (define (reverse sequence)
;;   (fold-right (lambda (x y)
;; 		(append y (list x)))
;; 	      null
;; 	      sequence))

;; (define (reverse sequence)
;;   (fold-left (lambda (x y)
;; 	       (cons y x))
;; 	     null
;; 	     sequence))
	      
;;;;;;;;;;;;;;;;;;;
;; Exercise 2.40 ;;
;;;;;;;;;;;;;;;;;;;

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

;; generates the sequence of pairs (i,j) with 1 <= j < i <= n
(define (unique-pairs n)
  (flatmap (lambda (i)
	     (map (lambda (j)
		    (list i j))
		  (enumerate-interval 1 (- i 1))))
	   (enumerate-interval 1 n)))

(define (make-pair-sum pair)
  (list (car pair)
	(cadr pair)
	(+ (car pair) (cadr pair))))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
	       (unique-pairs n))))

(define (smallest-divisor-next n)
  (define (next n)
    (if (= n 2)
	3
	(+ n 2)))
  (define (divides? a b)
    (= (remainder b a) 0))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
	  ((divides? test-divisor n) test-divisor)
	  (else (find-divisor n (next test-divisor)))))  
  (find-divisor n 2))

(define (prime? n)
  (= (smallest-divisor-next n) n))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.41 ;;
;;;;;;;;;;;;;;;;;;;

(define (make-triplets n)
  (flatmap (lambda (i)
	     (flatmap (lambda (j)
			(map (lambda (k)
			       (list i j k))
			     (enumerate-interval 1 n)))
		      (enumerate-interval 1 n)))
	   (enumerate-interval 1 n)))

(define (filter-dif-numbers triplets)
  (filter (lambda (t)
	    (not (or (= (car t) (cadr t))
		     (= (car t) (caddr t))
		     (= (cadr t) (caddr t)))))
	  triplets))

(define (triplet-sum triplet)
  (accumulate + 0 triplet))
		    
(define (triplets-sum n s)
  (filter (lambda (t)
	    (= (triplet-sum t) s))
	  (filter-dif-numbers (make-triplets n))))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.42 ;;
;;;;;;;;;;;;;;;;;;;

;; (define (flatmap mapper seq)
;;   (accumulate append null (map mapper seq)))

;; (define (enumerate-interval start end)
;;   (if (> start end)
;;       null
;;       (cons start (enumerate-interval (+ start 1) end))))

(define empty-board null)

(define (adjoin-position row col positions)
  (append positions (list (list row col))))

(define (pos-row position)
  (car position))

(define (pos-col position)
  (cadr position))

(define (upper-left-diagonal pos)
  (if (or (= (pos-row pos) 1) (= (pos-col pos) 1))
      null
      (let ((diagonal (list (- (pos-row pos) 1) (- (pos-col pos) 1))))
	(append (upper-left-diagonal diagonal) (list diagonal)))))

(define (lower-left-diagonal pos)
  (if (= (pos-col pos) 1)
      null
      (let ((diagonal (list (+ (pos-row pos) 1) (- (pos-col pos) 1))))
	(append (lower-left-diagonal diagonal) (list diagonal)))))
;; determines, for a set of positions, wether the queen
;; in the kth column is safe with respect to the others
(define (safe? col queens)
  (let ((last-queen-pos (car (filter (lambda (pos) (= (pos-col pos) col))
				     queens))))
    (and (= (length (filter (lambda (pos)
			     (= (pos-row pos) (pos-row last-queen-pos)))
			   queens))
	   1)
	(= (length (filter (lambda (pos)
			     (= (pos-col pos) (pos-col last-queen-pos)))
			   queens))
	   1)
	(let ((diagonals (append (upper-left-diagonal last-queen-pos)
				 (lower-left-diagonal last-queen-pos))))
	  (let ((diag-queens-intersection (flatmap (lambda (diagonal)
						     (filter (lambda (previous-queen)
							       (and (= (pos-row diagonal) (pos-row previous-queen))
								    (= (pos-col diagonal) (pos-col previous-queen))))
							     queens))
						   diagonals)))
	    (= (length diag-queens-intersection) 0))))))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
	(list empty-board)
	(filter (lambda (positions) (safe? k positions))
		(flatmap (lambda (rest-of-queens)
			   (map (lambda (new-row)
				  (adjoin-position new-row k rest-of-queens))
				(enumerate-interval 1 board-size)))
			 (queen-cols (- k 1))))))
  (queen-cols board-size))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.43 ;;
;;;;;;;;;;;;;;;;;;;
			   
;; If the order of mappings is inverted, (queen-cols (- k 1)) is re-evaluated
;; for (length (enumerate-interval 1 board-size)) times. If the puzzle is solved
;; in time T (4.42), then with this change it should take approx T * (board-size ^ board-size)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 2.2.4 Example: A Picture Language ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (define wave2 (beside wave (flip-vert wave)))
;; (define wave4 (below wave2 wave2))

(define beside '())
(define flip-vert '())
(define draw-line '())
(define frame-coord-map '())
(define transform-map '())
(define transform-painter '())

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

;; (define wave4 (flipped-pairs wave))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
	(beside painter
		(below smaller smaller)))))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.44 ;;
;;;;;;;;;;;;;;;;;;;

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
	(below painter
	       (beside smaller smaller)))))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.45 ;;
;;;;;;;;;;;;;;;;;;;

(define (split main second)
  (lambda (painter n)
    (if (= n 0)
	painter
	(let ((smaller ((split main second) painter (- n 1))))
	  (main painter
		(second smaller smaller))))))


;;;;;;;;;;;;;;;;;;;
;; Exercise 2.46 ;;
;;;;;;;;;;;;;;;;;;;

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))
(define (ycor-vect v)
  (cdr v))

(define (add-vect va vb)
  (make-vect (+ (xcor-vect va)
		(xcor-vect vb))
	     (+ (ycor-vect va)
		(ycor-vect vb))))

(define (sub-vect va vb)
  (add-vect va (make-vect (- (xcor-vect vb))
			    (- (ycor-vect vb)))))

(define (scale-vect v factor)
  (make-vect (* (xcor-vect v) factor)
	     (* (ycor-vect v) factor)))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.47 ;;
;;;;;;;;;;;;;;;;;;;
  
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frame f)
  (car f))
(define (edge1-frame f)
  (cadr f))
(define (edge2-frame f)
  (caddr f))

;; (define (make-frame origin edge1 edge2)
;;   (cons origin (cons edge1 edge2)))
;; (define (origin-frame f)
;;   (car f))
;; (define (edge1-frame f)
;;   (cadr f))
;; (define (edge2-frame f)
;;   (cddr f))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.48 ;;
;;;;;;;;;;;;;;;;;;;

;; (define (make-segment start end)
;;   (cons start end))

;; (define (start-segment seg)
;;   (car seg))
;; (define (end-segment seg)
;;   (cdr seg))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.49 ;;
;;;;;;;;;;;;;;;;;;;

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each (lambda (segment)
		(draw-line
		 ((frame-coord-map frame) (start-segment segment))
		 ((frame-coord-map frame) (end-segment segment))))
	      segment-list)))

(segments->painter (list (make-segment (make-vect 0 0) (make-vect 1 0))
			 (make-segment (make-vect 1 0) (make-vect 1 1))
			 (make-segment (make-vect 1 1) (make-vect 0 1))
			 (make-segment (make-vect 0 1) (make-vect 0 0))))

(segments->painter (list (make-segment (make-vect 0 0) (make-vect 1 1))
			 (make-segment (make-vect 0 1) (make-vect 1 0))))

(segments->painter (list (make-segment (make-vect 0.5 0) (make-vect 1 0.5))
			 (make-segment (make-vect 1 0.5) (make-vect 0.5 1))
			 (make-segment (make-vect 0.5 1) (make-vect 0 0.5))
			 (make-segment (make-vect 0 0.5) (make-vect 0.5 0))))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.50 ;;
;;;;;;;;;;;;;;;;;;;

(define (flip-horiz painter)
  (transform-painter painter
		     (make-vect 1.0 0)
		     (make-vect 0 0)
		     (make-vect 1.0 1.0)))

(define (rot-counter-180 painter)
  (transform-painter painter
		     (make-vect 1.0 1.0)
		     (make-vect 0 1.0)
		     (make-vect 1.0 0)))

(define (rot-counter-270 painter)
  (transform-painter painter
		     (make-vect 0 1.0)
		     (make-vect 0 0)
		     (make-vect 1.0 1.0)))
				
		     
;;;;;;;;;;;;;;;;;;;
;; Exercise 2.51 ;;
;;;;;;;;;;;;;;;;;;;

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-below (transform-painter painter1
					  (make-vect 0.0 0.0)
					  (make-vect 1.0 0.0)
					  split-point))
	  (paint-above (transform-painter painter2
					  split-point
					  (add-vect split-point (make-vect 0.5 0.0))
					  (add-vect split-point (make-vect 0.0 0.5)))))
      (lambda (frame)
	(paint-below frame)
	(paint-above frame)))))

;; (define (below painter1 painter2)
;;   (rot-counter-270
;;    (rot-counter-180
;;     (beside (rot-counter-270 painter1)
;; 	    (rot-counter-270 painter2)))))


;;;;;;;;;;;;;;;;;;;
;; Exercise 2.53 ;;
;;;;;;;;;;;;;;;;;;;

(list 'a 'b 'c) ;; (a b c)
(list (list 'george)) ;; ((george))
(cdr '((x1 x2) (y1 y2))) ;; ((y1 y2))
(cadr '((x1 x2) (y1 y2))) ;; (y1 y2)
(pair? (car '(a short lst))) ;; false
(memq 'red '((red shoes) (blue socks))) ;; false
(memq 'red '(red shoes blue socks)) ;; (red shoes blue socks)

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.54 ;;
;;;;;;;;;;;;;;;;;;;

(define (equal? a b)
  (cond ((and (not (pair? a)) (not (pair? b)))
	 (eq? a b))
	((and (pair? a) (pair? b))
	 (and (equal? (car a) (car b))
	      (equal? (cdr a) (cdr b))))
	(else #f)))
  

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.55 ;;
;;;;;;;;;;;;;;;;;;;

(car ''abracadabra) ;; (car (quote (quote abracadabra)))
;; quote

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.56 ;;
;;;;;;;;;;;;;;;;;;;

(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp)
	 (if (same-variable? exp var) 1 0))
	((sum? exp)
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var)))
	((product? exp)
	 (make-sum (make-product (multiplier exp)
				 (deriv (multiplicand exp) var))
		   (make-product (multiplicand exp)
				 (deriv (multiplier exp) var))))
	((exponentiation? exp)
	 (make-product
	  (make-product (exponent exp)
			(make-exponentiation (base exp)
					     (- (exponent exp) 1)))
	  (deriv (base exp) var)))
	(else (error "unknown expression type: DERIV" exp))))
	     
(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2)) (+ a1 a2))
	(else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((=number? m1 1) m2)
	((=number? m2 1) m1)
	((or (=number? m1 0) (=number? m2 0)) 0)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (list '* m1 m2))))

(define (sum? x)
  (and (pair? x) (eq? '+ (car x))))
(define (addend s) (cadr s))
(define (augend s)
  (caddr s))

(define (product? x)
  (and (pair? x) (eq? '* (car x))))
(define (multiplier p) (cadr p))
(define (multiplicand p)
  (caddr p))

(define (exponentiation? exp)
  (and (pair? exp) (eq? '** (car exp))))
(define (base exp)
  (cadr exp))
(define (exponent exp)
  (caddr exp))
(define (make-exponentiation base exponent)
  (cond ((= exponent 0) 1)
	((= exponent 1) base)
	(else (list '** base exponent))))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.57 ;;
;;;;;;;;;;;;;;;;;;;

;; (define (augend s)
;;   (cond ((= 2 (length (cdr s))) (caddr s))
;; 	(else (cons '+ (cddr s)))))

;; (define (multiplicand p)
;;   (cond ((= 2 (length (cdr p))) (caddr p))
;; 	(else (cons '* (cddr p)))))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.58 ;;
;;;;;;;;;;;;;;;;;;;

;; part a)
;; (define (make-sum a1 a2)
;;   (cond ((=number? a1 0) a2)
;; 	((=number? a2 0) a1)
;; 	((and (number? a1) (number? a2)) (+ a1 a2))
;; 	(else (list a1 '+ a2))))

;; (define (make-product m1 m2)
;;   (cond ((=number? m1 1) m2)
;; 	((=number? m2 1) m1)
;; 	((or (=number? m1 0) (=number? m2 0)) 0)
;; 	((and (number? m1) (number? m2)) (* m1 m2))
;; 	(else (list m1 '* m2))))

;; (define (sum? x)
;;   (and (pair? x) (eq? '+ (cadr x))))
;; (define (addend s) (car s))
;; (define (augend s) (caddr s))

;; (define (product? x)
;;   (and (pair? x) (eq? '* (cadr x))))
;; (define (multiplier p) (car p))
;; (define (multiplicand p) (caddr p))

;; part b)

;; (x + 3 * (x + y + 2))
;; (define (sum? x)
;;   (and (pair? x)
;;        (> (length (filter (lambda (e) (eq? e '+)) x))
;; 	  0)))
;; (define (addend s)
;;   (define (iter addend rest)
;;     (if (eq? (car rest) '+)
;; 	(if (= (length addend) 1)
;; 	    (car addend)
;; 	    addend)
;; 	(iter (append addend (list (car rest)))
;; 	      (cdr rest))))
;;   (iter '() s))
;; (define (augend s)
;;   (let ((aug (cdr (memq '+ s))))
;;     (if (= 1 (length aug))
;; 	(car aug)
;; 	aug)))
  
;; (define (product? x)
;;   (and (pair? x)
;;        (let ((operators (filter symbol? x)))
;; 	 (= (length operators)
;; 	    (length (filter (lambda (e)
;; 			      (eq? e '*))
;; 			    x))))))
;; (define (multiplier p)
;;   (define (iter multip rest)
;;     (if (eq? (car rest) '*)
;; 	(if (= (length multip) 1)
;; 	    (car multip)
;; 	    multip)
;; 	(iter (append multip (list (car rest)))
;; 	      (cdr rest))))
;;   (iter '() p))
;; (define (multiplicand p)
;;   (let ((multip (cdr (memq '* p))))
;;     (if (= 1 (length multip))
;; 	(car multip)
;; 	multip)))
	   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sets as unordered lists ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; O(n)
;; (define (element-of-set? x set)
;;   (cond ((null? set) #f)
;; 	((equal? x (car set)) #t)
;; 	(else (element-of-set? x (cdr set)))))

;; O(n)
;; (define (adjoin-set x set)
;;   (if (element-of-set? x set)
;;       set
;;       (cons x set)))

;; O(n^2)
;; (define (intersection-set set1 set2)
;;   (cond ((null? set1) '())
;; 	((element-of-set? (car set1) set2)
;; 	 (cons (car set1)
;; 	       (intersection-set (cdr set1) set2)))
;; 	(else (intersection-set (cdr set1) set2))))

;;;;;;;;;;;;;;;;;;;;
;; Excercise 2.59 ;;
;;;;;;;;;;;;;;;;;;;;

;; O(n^2)
;; (define (union-set set1 set2)
;;   (cond ((null? set1) set2)
;; 	(else (adjoin-set (car set1)
;; 			  (union-set (cdr set1) set2)))))

;; (define (union-set set1 set2)
;;   (accumulate adjoin-set set2 set1))
;;;;;;;;;;;;;;;;;;;
;; Exercise 2.60 ;;
;;;;;;;;;;;;;;;;;;;

;; O(1)
;; (define (adjoin-set x set)
;;   (cons x set))

;; (define (union-set set1 set2)
;;   (append set1 set2))

;; el resto son iguales

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sets as ordered lists ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; S={1,2,3,4} => '(1 2 3 4)
;; O(n)
;; (define (element-of-set? x set)
;;   (cond ((null? set) #t)
;; 	((< x (car set)) #f)
;; 	((= x (car set)) #t)
;; 	(else (element-of-set? x (cdr set)))))

;; O(n)
;; each step removes one element of set1, set2 or both
;; at most, if n1=(size set1) && n2=(size set2)
;; n1+n2 steps are needed ( O(n) ), instead of n1*n2 ( O(n^2) )
;; as with unordered lists
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
	(else (let ((x1 (car set1))
		    (x2 (car set2)))
		(cond ((< x1 x2)
		       (intersection-set (cdr set1) set2))
		      ((< x2 x1)
		       (intersection-set set1 (cdr set2)))
		      (else
		       (cons x1 (intersection-set (cdr set1)
						  (cdr set2)))))))))
  
;;;;;;;;;;;;;;;;;;;
;; Exercise 2.61 ;;
;;;;;;;;;;;;;;;;;;;

;; (define (adjoin-set x set)
;;   (cond ((null? set) (list x))
;; 	((< x (car set)) (cons x set))
;; 	((= x (car set)) set)
;; 	(else (cons (car set) (adjoin-set x (cdr set))))))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.62 ;;
;;;;;;;;;;;;;;;;;;;

(define (union-set set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	(else (let ((x1 (car set1))
		    (x2 (car set2)))
		(cond ((< x1 x2)
		       (cons x1 (union-set (cdr set1) set2)))
		      ((= x1 x2)
		       (cons x1 (union-set (cdr set1) (cdr set2))))
		      (else ;; (> x1 x2)
		       (cons x2 (union-set set1 (cdr set2)))))))))
					   
;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sets as binary trees ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

;; abstraction barrier :)

(define (element-of-set? x set)
  (cond ((null? set) #f)
	((= x (entry set)) #t)
	((< x (entry set)) (element-of-set? x (left-branch set)))
	(else (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
	((= x (entry set)) set)
	((< x (entry set))
	 (make-tree (entry set)
		    (adjoin-set x (left-branch set))
		    (right-branch set)))
	(else
	 (make-tree (entry set)
		    (left-branch set)
		    (adjoin-set x (right-branch set))))))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.63 ;;
;;;;;;;;;;;;;;;;;;;

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
	      (list (entry tree))
	      (tree->list-1 (right-branch tree)))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
	result-list
	(copy-to-list (left-branch tree)
		      (cons (entry tree)
			    (copy-to-list (right-branch tree)
					  result-list)))))
  (copy-to-list tree '()))

;; a) yes, both procedures generate the same answer
;; b) tree->list-1: T(n) = 2*T(n/2) + O(n/2) (por append)
;;                  T(n) = O(n * log n)
;;    tree->list-2: T(n) = 2*T(n/2) + O(1) (por cons)
;;                  T(n) = O(n)

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.64 ;;
;;;;;;;;;;;;;;;;;;;

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
	(let ((left-result (partial-tree elts left-size)))
	  (let ((left-branch (car left-result))
		(non-left-elts (cdr left-result))
		(right-size (- n (+ left-size 1))))
	    (let ((this-entry (car non-left-elts))
		  (right-result (partial-tree (cdr non-left-elts)
					      right-size)))
	      (let ((right-branch (car right-result))
		    (remaining-elts (cdr right-result)))
		(cons (make-tree this-entry left-branch right-branch)
		      remaining-elts))))))))
		  
;; a)
;; partial-tree takes a number N and a list of elements of at
;; least N elements ELTS.
;; it computes the number of elements corresponding to each halve
;; of the tree (each branch), and recursively calls itself to
;; produce the partial trees corresponding to those elements.
;; each time a subtree is made, the first item of the list is taken

;; b) T(n) = 2*T(n/2) + O(1) (por cons) = O(n)

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.65 ;;
;;;;;;;;;;;;;;;;;;;

(define (union-tree tree-1 tree-2)
  (let ((list-1 (tree->list-2 tree-1))
	(list-2 (tree->list-2 tree-2)))
    (list->tree (union-set list-1 list-2))))
;;            tree->list-2   union-set (ordered list)  list->tree
;; T(n) = 2 *    O(n)      +     O(n)                +   O(n)
;; T(n) = O(n)

(define (intersection-tree tree-1 tree-2)
  (let ((list-1 (tree->list-2 tree-1))
	(list-2 (tree->list-2 tree-2)))
    (list->tree (intersection-set list-1 list-2))))
	
;;;;;;;;;;;;;;;;;;;
;; Exercise 2.66 ;;
;;;;;;;;;;;;;;;;;;;

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
	((= given-key (key (entry set-of-records)))
	 (entry set-of-records))
	((< given-key (key (entry set-of-records)))
	 (lookup given-key (left-branch set-of-records)))
	(else
	 (lookup given-key (right-branch set-of-records)))))
