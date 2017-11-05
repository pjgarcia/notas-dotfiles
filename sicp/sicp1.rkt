#lang racket/base

;;;;;;;;;;;;;;;;;;
;; Exercise 1.2 ;;
;;;;;;;;;;;;;;;;;;

(/ (+ 5 4 (- 2 (- 3 (+ 6 4/5))))
   (* 3 (- 6 2) (- 2 7)))


;;;;;;;;;;;;;;;;;;;
;; Excercise 1.3 ;;
;;;;;;;;;;;;;;;;;;;

(define (sum-square-largers a b c)
  (define (square n)
    (* n n))
  (cond ((and (<= c a) (<= c b))
	 (+ (square a) (square b)))
	((and (<= b a) (<= b c))
	 (+ (square a) (square c)))
	(else
	;;((and (< a b) (< a c))
	 (+ (square b) (square c)))))

;;;;;;;;;;;;;;;;;;
;; Exercise 1.4 ;;
;;;;;;;;;;;;;;;;;;
	 
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;; si b > 0, se utiliza la funcion suma, si no, resta


;;;;;;;;;;;;;;;;;;
;; Exercise 1.5 ;;
;;;;;;;;;;;;;;;;;;

;; applicative-order
;;(test 0 (p))
;;(if (= 0 0) 0 (p))
;; en el if se va a evaluar primero el predicado
;; como es verdadero, el valor de if va a ser 0

;; normal-order
;; cuando se evaluan los operandos, 0 y (p), se va a
;; llamar continuamente a (p) y la ejecucion va a
;; quedar ahi loopeando.


	     
;;;;;;;;;;;;;;;;;;
;; Exercise 1.6 ;;
;;;;;;;;;;;;;;;;;;

(define (square n)
  (* n n))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
		 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

;; (define (good-enough? guess x)
;;   (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
	(else else-clause)))

;; (define (sqrt-iter guess x)
;;   (new-if (good-enough? guess x)
;; 	  x
;; 	  (sqrt-iter (improve guess x)
;; 		     x)))

;; cuando Alyssa usa "new-if" para calcular "sqrt-iter",
;; sucede que, como "new-if" es una combination normal y
;; por lo tanto esta sujeta a la forma normal de evaluacion.
;; Esto hace que LAS DOS CLAUSULAS VAN A SER EVALUADAS,
;; aunque el "new-if" solo va a retornar el valor de la
;; que corresponda.


;;;;;;;;;;;;;;;;;;
;; Exercise 1.7 ;;
;;;;;;;;;;;;;;;;;;

;;(sqrt 0.00001) ;; => 0.03135649010771716
;; deberia ser       0.001

;;(sqrt 12345678901234567890) ;; => 3513641828.820144
;; (sqrt 123456789012345678900) (un digito mas);; => no termina

(define (good-enough? guess x)
  (< (abs (- (improve guess x) guess))
     (* guess 0.001)))

;;;;;;;;;;;;;;;;;;
;; Exercise 1.8 ;;
;;;;;;;;;;;;;;;;;;

(define (cuberoot-iter guess x)  
  (define (improve guess x)
    (/ (+ (/ x (square guess))
	  (* 2 guess))
       3))
  (define (good-enough? guess x)
    (< (abs (- (improve guess x) guess))
       (* guess 0.001)))
  (if (good-enough? guess x)
      guess
      (cuberoot-iter (improve guess x)
		     x)))

(define (cuberoot x)
  (cuberoot-iter 1.0 x))

;;;;;;;;;;;;;;;;;;
;; Exercise 1.9 ;;
;;;;;;;;;;;;;;;;;;

(define (inc n)
  (+ n 1))

(define (dec n)
  (- n 1))

(define (suma-a a b)
  (if (= a 0)
      b
      (inc (suma-a (dec a) b))))

;; (suma-a 4 5)
;; (inc (suma-a (dec 4) 5))
;; (inc (suma-a 3 5))
;; (inc (inc (suma-a 2 5)))
;; (inc (inc (inc (suma-a 1 5))))
;; (inc (inc (inc (inc (suma-a 0 5)))))
;; (inc (inc (inc (inc 5))))
;; (inc (inc (inc 6)))
;; (inc (inc 7))
;; (inc 8)
;; 9
;; suma-a is linear recursive

(define (suma-b a b)
  (if (= a 0)
      b
      (suma-b (dec a) (inc b))))

;; (suma-b 4 5)
;; (suma-b (dec 4) (inc 5))
;; (suma-b 3 6)
;; (suma-b 2 7)
;; (suma-b 1 8)
;; (suma-b 0 9)
;; 9
;; suma-b is linear iterative

;;;;;;;;;;;;;;;;;;;
;; Exercise 1.10 ;;
;;;;;;;;;;;;;;;;;;;

(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1)
		 (A x (- y 1))))))

(define (f n) (A 0 n)) ;; double
(define (g n) (A 1 n)) ;; potencias de dos para n > 1
(define (h n) (A 2 n)) ;; exponencian enorme
(define (k n) (* 5 n n)) ;; 5*n^2
	 
;;;;;;;;;;;;;;;;;;;;
;; Excercise 1.11 ;;
;;;;;;;;;;;;;;;;;;;;

(define (f-recur n)
  (if (< n 3)
      n
      (+ (f-recur (- n 1))
	 (* 2 (f-recur (- n 2)))
	 (* 3 (f-recur (- n 3))))))

(define (f-iter n)
  (define (iterate a b c count)
    (if (= 0 count)
	c
	(iterate (+ a
		    (* 2 b)
		    (* 3 c))
		 a
		 b
		 (- count 1))))
  (iterate 2 1 0 n))

;;;;;;;;;;;;;;;;;;;
;; Exercise 1.12 ;;
;;;;;;;;;;;;;;;;;;;

(define (pascal row col)
  (cond ((or (= col 1) (= col row)) 1)
	(else (+ (pascal (- row 1) (- col 1))
		 (pascal (- row 1) col)))))

;;;;;;;;;;;;;;;;;;;
;; Exercise 1.15 ;;
;;;;;;;;;;;;;;;;;;;

(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

;; (sine 12.15)
;; (p (sine 4.05))
;; (p (p (sine 1.34)))
;; (p (p (p (sine 0.45))))
;; (p (p (p (p (sine 0.15)))))
;; (p (p (p (p (p (sine 0.05))))))
;; (p (p (p (p (p 0.05)))))
;; (p (p (p (p 0.15))))
;; (p (p (p 0.44)))
;; (p (p 0.98))
;; (p -0.79)
;; -0.39

;; a) p is applied 5 times
;; b) space complexity: log3
;;    time complexity: log3 

;;;;;;;;;;;;;;;;;;;
;; Exercise 1.16 ;;
;;;;;;;;;;;;;;;;;;;

(define (exponential b n)
  (define (iter a b n)
    (if (= n 0)
	a
	(if (even? n)
	    (iter a (square b) (/ n 2))
	    (iter (* a b) b (- n 1)))))
  (iter 1 b n))

;;;;;;;;;;;;;;;;;;;
;; Exercise 1.17 ;;
;;;;;;;;;;;;;;;;;;;
(define (double n)
  (+ n n))
(define (halve n)
  (/ n 2))
  
(define (mul a b)
  (cond ((= b 1) a)
	((even? b) (mul (double a) (halve b)))
	(else (+ a (mul a (- b 1))))))

;;;;;;;;;;;;;;;;;;;
;; Exercise 1.18 ;;
;;;;;;;;;;;;;;;;;;;

(define (multiplicate a b)
  (define (iter a b c)
    (cond ((= b 0) c)
	  ((even? b) (iter (double a) (halve b) c))
	  (else (iter a (- b 1) (+ c a)))))
  (iter a b 0))

;;;;;;;;;;;;;;;;;;;
;; Exercise 1.19 ;;
;;;;;;;;;;;;;;;;;;;

(define (fib-t n)
  (define (fib-iter a b p q count)
    (cond ((= count 0) b)
	  ((even? count)
	   (fib-iter a
		     b
		     (+ (square q) (square p)) ;; p'
		     (+ (* 2 p q) (square q)) ;; q'
		     (/ count 2)))
	  (else (fib-iter (+ (* b q) (* a q) (* a p))
			  (+ (* b p) (* a q))
			  p
			  q
			  (- count 1)))))		     
  (fib-iter 1 0 0 1 n))

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

;;;;;;;;;;;;;;;;;;;
;; Exercise 1.20 ;;
;;;;;;;;;;;;;;;;;;;

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; applicative-order 
(gcd 206 40)
(gcd 40 6)
(gcd 6 4)
(gcd 4 2)
(gcd 2 0)
2

;;;;;;;;;;;;;;;;;;;
;; Exercise 1.21 ;;
;;;;;;;;;;;;;;;;;;;

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

;; (smallest-divisor 199) => 199
;; (smallest-divisor 1999) => 1999
;; (smallest-divisor 19999) => 7

;;;;;;;;;;;;;;;;;;;
;; Exercise 1.22 ;;
;;;;;;;;;;;;;;;;;;;

(define (timed-prime-test n)
  (start-prime-test n (current-milliseconds)))

(define (start-prime-test n start-time)
  (cond ((fast-prime? n)
	 (report-prime (- (current-milliseconds) start-time)
		       n))))

(define (report-prime elapsed-time n)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time))

;; O(sqrt(n))
(define (prime? n)
  (= (smallest-divisor-next n) n))

(define (search-for-primes start end)
  (cond ((even? start) (search-for-primes (+ start 1) end))
	((not (> start end))
	 (timed-prime-test start)
	 (search-for-primes (+ start 2) end))))
;; first 3 primes from:
;; a) 10^9: 2 seconds
;; b) 10^12: 60 seconds
;; c) 10^13: 185 seconds

;; (b) should take:
(* (sqrt 100) 2) ;; 20 seconds <== does not...
(* (sqrt 10) 60) ;; 189 seconds <== satisfies the prediction

;;;;;;;;;;;;;;;;;;;
;; Exercise 1.23 ;;
;;;;;;;;;;;;;;;;;;;

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

;; (search-for-primes (expt 10 13) (+ (expt 10 13) 100))
;; 10000000000037 *** 68
;; 10000000000051 *** 62

;; for the primes around (expt 10 13) the time consumption drops from ~180 to ~65

;;;;;;;;;;;;;;;;;;;
;; Exercise 1.24 ;;
;;;;;;;;;;;;;;;;;;;

(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m))
		    m))
	(else
	 (remainder (* base (expmod base (- exp 1) m))
		    m))))

;; O(log n)
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (min 4294967087 (- n 1))))))

(define (fast-prime? n)
  (define (iter times)
    (cond ((= times 0) #t)
	  ((fermat-test n) (iter (- times 1)))
	  (else #f)))
  (iter 100))
	 
;; since: 10^19 = 10^3 * 10^16
;; log(10^19) = log(10^3 * 10^16)
;; log(10^19) = log(10^3) + log(10^16)

;; therefore, for an input 10^3 times larger than before,
;; the algorithm should take log(10^3)=3 more. (not times!)

;;;;;;;;;;;;;;;;;;;
;; Exercise 1.25 ;;
;;;;;;;;;;;;;;;;;;;

;; This procedure will not serve as well as our expmod because,
;; when used with big exp, the dividend will become exponentially
;; large. Our expmod uses properties of modular exponentiation
;; to work with numbers not much larger than m (not > m than m when squaring)
;; Very large numbers imposes an overhead in the lisp evaluator.

;; (define (square n)
;;   (display "square ")(display n)(newline)
;;   (* n n))

;;;;;;;;;;;;;;;;;;;
;; Exercise 1.26 ;;
;;;;;;;;;;;;;;;;;;;

;; In the original expmod, when we double the size of the input (exponent)
;; the algorightm only needs one more step to compute te result.
;; Meanwhile, the transformed expmod needs to double the steps:
;; steps(n) = 2 * steps(n/2)
;; Therefore, the new methods behaves like O(n)

;;;;;;;;;;;;;;;;;;;
;; Exercise 1.27 ;;
;;;;;;;;;;;;;;;;;;;

(define (congruent-mod-n-lessers n)
  (define (congruent-mod-n a n)
    (= (expmod a n n) a))
  (define (iter a)
    (cond ((= a n) #t)
	  ((congruent-mod-n a n) (iter (+ a 1)))
	  (else #f)))
  (iter 1))

;;;;;;;;;;;;;;;;;;;
;; Exercise 1.28 ;;
;;;;;;;;;;;;;;;;;;;

(define (miller-rabint-test n)

  (define (mr-expmod a n m)
    (define (even-part x)
      (if (and (=  1 (remainder (square x) m))
	       (not (= 1 x))
	       (not (= (- m 1) x)))
	  0
	  (remainder (square x) m)))
    (cond ((= n 0) 1)
	  ((even? n)
	   (even-part (mr-expmod a (/ n 2) m)))
	  (else
	   (remainder (* a (mr-expmod a (- n 1) m))
		      m))))

  
  (define (iter i)
    (cond ((> i (/ n 2)) #t)
	  ((= 0 (mr-expmod i (- n 1) n)) #f)
	  (else (iter (+ i 1)))))
  (iter 1))
    


	  
(define (mr-expmod a n m)
  (define (even-part x)
    (if (and (=  1 (remainder (square x) m))
	     (not (= 1 x))
	     (not (= (- m 1) x)))
	0
	(remainder (square x) m)))
  (cond ((= n 0) 1)
	((even? n)
	 (even-part (mr-expmod a (/ n 2) m)))
	(else
	 (remainder (* a (mr-expmod a (- n 1) m))
		    m))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 1.3 functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (identity n) n)

;;;;;;;;;;;;;;;;;;;
;; Exercise 1.29 ;;
;;;;;;;;;;;;;;;;;;;

(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (term k)
    (* (f (+ a (* k h)))
       (cond ((or (= k 0) (= k n)) 1)
	     ((even? k) 2)
	     (else 4))))
  (* (/ h 3)
     (sum term 0 inc n)))


;;;;;;;;;;;;;;;;;;;
;; Exercise 1.30 ;;
;;;;;;;;;;;;;;;;;;;

;; Linear recursive version of sum
;; (define (sum term a next b)
;;   (if (> a b)
;;       0
;;       (+ (term a)
;; 	 (sum term (next a) next b))))

(define (sum-i term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ result (term a)))))
  (iter a 0))
    

;;;;;;;;;;;;;;;;;;;
;; Exercise 1.31 ;;
;;;;;;;;;;;;;;;;;;;

(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(define (fact-product n)
  (product identity 1 inc n))

(define (product-i term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* result (term a)))))
  (iter a 1))

(define (wallis-pi n)
  (define (term k)
    (/ (* (* 2.0 k)
	  (* 2.0 (+ k 1)))
       (square (+ 1 (* k 2.0)))))
  (product-i term 1.0 inc n))

;;;;;;;;;;;;;;;;;;;
;; Exercise 1.32 ;;
;;;;;;;;;;;;;;;;;;;

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner
       (term a)
       (accumulate combiner null-value term (next a) next b))))

(define (new-sum term a next b)
  (accumulate + 0 term a next b))

(define (new-product term a next b)
  (accumulate * 1 term a next b))

(define (accumulate-i combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (combiner result (term a)))))
  (iter a null-value))

;;;;;;;;;;;;;;;;;;;
;; Exercise 1.33 ;;
;;;;;;;;;;;;;;;;;;;

(define (filtered-accum combiner null-value term a next b filter)
  (cond ((> a b) null-value)
	((filter a)
	 (combiner (term a)
		   (filtered-accum combiner null-value term (next a) next b filter)))
	(else
	 (filtered-accum combiner null-value term (next a) next b filter))))

;; racket@sicp.rkt> (filtered-accum + 0 square 2 inc 6 prime?)
;; 38

(define (relative-prime i)
  (= (gcd 10 i) 1))

;; racket@sicp.rkt> (filtered-accum * 1 identity 1 inc 9 relative-prime)
;; 189

(define (filtered-accum-i combiner null-value term a next b filter)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (combiner (if (filter a) (term a) null-value) result))))
  (iter a null-value))
				 

;;;;;;;;;;;;;;;;;;;
;; Exercise 1.34 ;;
;;;;;;;;;;;;;;;;;;;

;;(define (f g) (g 2))

;; (f square) => 4
;; (f (lambda (z) (* z (+ z 1)))) => 6

;; (f f)
;; (f 2)
;; (2 2) => error!

;;;;;;;;;;;;;;;;;;;
;; Exercise 1.35 ;;
;;;;;;;;;;;;;;;;;;;

(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  guess
	  (try next))))
  (try first-guess))

;; phi es P.F. de f(x) = 1 + 1/x si:
;; (= (f phi)
;;    (+ 1 (/ 1 phi))
;;    phi)
;; reemplazando phi por su valor
;; (= (/ (+ 1 (sqrt 5)) 2)
;;    (+ 1 (/ 1
;; 	   (/ (+ 1 (sqrt 5)) 2))))
;; multiplicando por 2 y restando 2 
;; (= (+ (- 1) (sqrt 5))
;;    (/ 4 (+ 1 (sqrt 5))))
;; multiplicando por 1 + sqrt(5)
;; (= 4 4)

;;;;;;;;;;;;;;;;;;;
;; Exercise 1.36 ;;
;;;;;;;;;;;;;;;;;;;

(define (fixed-point-print f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess step)
    (let ((next (f guess)))
      (display step) (display " ")
      (display next)
      (newline)
      (if (close-enough? guess next)
	  guess
	  (try next (+ step 1)))))
  (try first-guess 1))

;; (fixed-point-print 
;;  (lambda (x) (average x (/ (log 1000) (log x))))
;;  2)

;; toma 34 iteraciones

;; (fixed-point-print
;;  (lambda (x) (/ (log 1000) (log x)))
;;  2)

;; toma 9 iteraciones

;;;;;;;;;;;;;;;;;;;
;; Exercise 1.37 ;;
;;;;;;;;;;;;;;;;;;;

(define (cont-frac n d k)
  (define (recur i)
    (if (> i k)
	0
	(/ (n i)
	   (+ (d i) (recur (+ i 1))))))
  (recur 1))

;; k = 11 gives at least 4 decimal accurate places

(define (cont-frac-i n d k)
  (define (iter i accum)
    (if (= i 0)
	accum
	(iter (- i 1)
	      (/ (n i)
		 (+ (d i) accum)))))
  (iter k 0))

;;;;;;;;;;;;;;;;;;;
;; Exercise 1.38 ;;
;;;;;;;;;;;;;;;;;;;

(define (d i)
  (let ((part (remainder i 3)))
    (if (or (= part 0) (= part 1))
	1
	(* 2 (+ 1 (quotient i 3))))))
    
;;;;;;;;;;;;;;;;;;;
;; Exercise 1.39 ;;
;;;;;;;;;;;;;;;;;;;

(define (tan-cf x k)
  (define (n i)
    (if (= i 1)
	x
	(- 0 (square x))))
  (define (d i)
    (- (* 2 i) 1))
  (cont-frac n d k))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.3.4 Procedures as Returned Values ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (average-damp f)
  (lambda (x) (average x (f x))))

;; (define (sqrt x)
;;   (fixed-point (average-damp (lambda (y) (/ x y)))
;; 	       1.0))

;; compare the previous definition of sqrt with
;; the next
;; the former makes explicit the three ideas of
;; fixed-point search, average damping, and the
;; function y -> x/y
;; the general idea of the process becomes clearer
;; as we use those abstractions

;; (define (sqrt-iter guess x)
;;   (if (good-enough? guess x)
;;       guess
;;       (sqrt-iter (improve guess x)
;; 		 x)))

;; (define (improve guess x)
;;   (average guess (/ x guess)))

;; (define (sqrt x)
;;   (sqrt-iter 1.0 x))


;;;;;;;;;;;;;;;;;;;;;
;; Newton's Method ;;
;;;;;;;;;;;;;;;;;;;;;

;; if dx is a small number
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x)
	    ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

;; (define (sqrt x)
;;   (newtons-method (lambda (y) (- (square y) x)) 1.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Abstraction and first-class procedures ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

;; (define (sqrt x)
;;   (fixed-point-of-transform
;;    (lambda (y) (/ x y))
;;    average-damp
;;    1.0))

;; (define (sqrt x)
;;   (fixed-point-of-transform
;;    (lambda (y) (- (square y) x))
;;    newton-transform
;;    1.0))

;;;;;;;;;;;;;;;;;;;
;; Exercise 1.40 ;;
;;;;;;;;;;;;;;;;;;;

(define (cubic a b c)
  (lambda (x) (+ (cube x)
		 (* a (square x))
		 (* b x)
		 c)))

;;;;;;;;;;;;;;;;;;;
;; Exercise 1.41 ;;
;;;;;;;;;;;;;;;;;;;

(define (double-f f)
  (lambda (x) (f (f x))))

(((double-f (double-f double-f)) inc) 5) ;; 21

;; ((double (double (double (double inc)))) 5)
;; ((inc1 (inc2 ... (inc 16)...)) 5)

;;;;;;;;;;;;;;;;;;;
;; Exercise 1.42 ;;
;;;;;;;;;;;;;;;;;;;

(define (compose f g)
  (lambda (x)
    (f (g x))))

;;;;;;;;;;;;;;;;;;;
;; Exercise 1.43 ;;
;;;;;;;;;;;;;;;;;;;

(define (repeated f n)
  (cond ((= n 1) f)
	((even? n) (repeated (double-f f) (/ n 2)))
	(else (compose f (repeated f (- n 1))))))

;;;;;;;;;;;;;;;;;;;
;; Exercise 1.44 ;;
;;;;;;;;;;;;;;;;;;;

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx))
		    (f x)
		    (f (+ x dx)))
		 3)))

(define (repeated-smooth f n)
  ((repeated smooth n) f))
  
;;;;;;;;;;;;;;;;;;;
;; Exercise 1.45 ;;
;;;;;;;;;;;;;;;;;;;

(define (nth-root n x)
  (define (log2 x)
    (/ (log x)
       (log 2)))
  (fixed-point-of-transform
   (lambda (y)
     (/ x (expt y (- n 1))))
   (repeated average-damp (floor (log2 n)))
   1.0))

;;;;;;;;;;;;;;;;;;;
;; Exercise 1.46 ;;
;;;;;;;;;;;;;;;;;;;

(define (iterative-improve good-enough? improve)
  (lambda (x)
    (define (iter n)
      (if (good-enough? n)
	  n
	  (iter (improve n))))
    (iter x)))

(define (fixed-point f first-guess)
  (define tolerance 0.000001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))

  ((iterative-improve
    (lambda (x)
      (close-enough? x (f x)))
    (lambda (x)
      (f x)))
   first-guess))

(define (sqrt x)
  ((iterative-improve
    (lambda (y)
      (< (abs (- (square y) x)) 0.0001))
    (lambda (y)
      (average y (/ x y))))
   1.0))
