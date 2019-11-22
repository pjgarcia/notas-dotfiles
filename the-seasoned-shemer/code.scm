(define is-first?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (eq? (car lat) a)))))

(define two-in-a-row?
  (lambda (lat)
    (cond
     ((null? lat) #f)
     (else
      (or (is-first? (car lat)
		     (cdr lat))
	  (two-in-a-row? (cdr lat)))))))

(define two-in-a-row-b?
  (lambda (lat)
    (cond
     ((null? lat) #f)
     (else
      (is-first-b? (car lat)
		   (cdr lat))))))

(define is-first-b?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     ((eq? a (car lat)) #t)
     (else
      (two-in-a-row-b? lat)))))
