;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 11. Welcome Back to the Show ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define two-in-a-row-b?
  (lambda (preceding lat)
    (cond
     ((null? lat) #f)
     (else
      (or (eq? preceding (car lat))
	  (two-in-a-row-b?
	   (car lat)
	   (cdr lat)))))))
    
(define two-in-a-row?
  (lambda (lat)
    (cond
     ((null? lat) #f)
     (else
      (two-in-a-row-b?
       (car lat)
       (cdr lat))))))

(define sum-of-prefixes-b
  (lambda (sonssf tup)
    (cond
     ((null? tup) (quote ()))
     (else
      (cons (+ sonssf (car tup))
	    (sum-of-prefixes-b
	     (+ sonssf (car tup))
	     (cdr tup)))))))

(define sum-of-prefixes
  (lambda (tup)
    (sum-of-prefixes-b 0 tup)))

(define pick
  (lambda (n lat)
    (cond
     ((eq? 1 n) (car lat))
     (else (pick (- n 1) (cdr lat))))))

(define scramble-b
  (lambda (tup rev-pre)
    (cond
     ((null? tup) (quote ()))
     (else
      (cons (pick (car tup)
		  (cons (car tup) rev-pre))
	    (scramble-b
	     (cdr tup)
	     (cons (car tup) rev-pre)))))))

(define scramble
  (lambda (tup)
    (scramble-b tup (quote ()))))
    
;;;;;;;;;;;;;;;;;;;;
;; 12. Take Cover ;;
;;;;;;;;;;;;;;;;;;;;

(define multirember
  (lambda (a lat)
    (letrec
	((mr (lambda (lat)
	       (cond
		((null? lat) (quote ()))
		((eq? a (car lat))
		 (mr (cdr lat)))
		(else
		 (cons (car lat)
		       (mr (cdr lat))))))))
      (mr lat))))

(define union
  (lambda (set1 set2)
    (letrec
	((M? (lambda (a lat)
	       (letrec
		   ((m? (lambda (lat)			  
			  (cond
			   ((null? lat) #f)
			   ((eq? a (car lat)) #t)
			   (else
			    (m? (cdr lat)))))))
		 (m? lat))))
	 (U (lambda (set1)
	      (cond
	       ((null? set1) set2)
	       ((M? (car set1) set2)
		(U (cdr set1)))
	       (else
		(cons (car set1)
		      (U (cdr set1))))))))
      (U set1))))

(define two-in-a-row-b?
  (lambda (preceding lat)
    (cond
     ((null? lat) #f)
     (else
      (or (eq? preceding (car lat))
	  (two-in-a-row-b?
	   (car lat)
	   (cdr lat)))))))
    
;; es two-in-a-row? pero protegiendo la definicion de two-in-a-row-b
(define new-two-in-a-row?
  (letrec
      ((W (lambda (preceding lat)
	    (cond
	     ((null? lat) #f)
	     (else
	      (or (eq? preceding (car lat))
		  (W
		   (car lat)
		   (cdr lat))))))))
    (lambda (lat)
      (cond
       ((null? lat) #f)
       (else
	(W (car lat) (cdr lat))))))

(define sum-of-prefixes-b
  (lambda (sonssf tup)
    (cond
     ((null? tup) (quote ()))
     (else
      (cons (+ sonssf (car tup))
	    (sum-of-prefixes-b
	     (+ sonssf (car tup))
	     (cdr tup)))))))

;; also protected
(define new-sum-of-prefixes
  (letrec
      ((S (lambda (sonssf tup)
	    (cond
	     ((null? tup) (quote ()))
	     (else
	      (cons (+ sonssf (car tup))
		    (S (+ sonssf (car tup))
		       (cdr tup))))))))
    (lambda (tup)
      (S 0 tup))))


;; also protected
(define scramble
  (letrec
      ((S (lambda (tup rev-pre)
	    (cond
	     ((null? tup) (quote ()))
	     (else
	      (cons (pick (car tup)
			  (cons (car tup) rev-pre))
		    (S (cdr tup)
		       (cons (car tup) rev-pre))))))))
    (lambda (tup)
      (S tup (quote ())))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 13. Hop, Skip and Jump ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define instersectall
  (lambda (lset)
    (letcc hop
	   (letrec
	       ((A (lambda (lset)
		     (cond
		      ((null? (car lset))
		       (hop (quote ())))
		      ((null? (cdr lset))
		       (car lset))
		      (else
		       (I (car lset)
			  (A (cdr lset)))))))
		(I (lambda (s1 s2)
		     (letrec
			 ((M? (lambda (s)
				(cond				  
				 ((null? s) (quote ()))
				 ((member? (car s))
				  (cons (car s)
					(I (cdr s))))
				 (else
				  (I (cdr s)))))))
		       (cond
			((null? s2) (hop (quote ())))
			(else (M? s))))))
		(cond
		 ((null? lset) (quote ()))
		 (else (A lset))))))))
  

(define rember
  (lambda (a lat)
    (letrec
	((R (lambda (lat)
	      (cond
	       ((null? lat) (quote ()))
	       ((eq? a (car lat))
		(cdr lat))
	       (else
		(cons (car lat)
		      (R (cdr lat))))))))
      (R lat))))
		      
(define rember-upto-last
  (lambda (a lat)
    (letcc skip
	   (letrec
	       ((R (lambda (lat)
		     (cond
		      ((null? lat) (quote ()))
		      ((eq? a (car lat))
		       (skip (R (cdr lat))))
		      (else
		       (cons (car lat)
			     (R (cdr lat))))))))
	     (R lat)))))
