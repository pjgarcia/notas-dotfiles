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