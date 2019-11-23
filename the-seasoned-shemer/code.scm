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
    (sum-of-prefixes-b 0 tup)))++

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
    
