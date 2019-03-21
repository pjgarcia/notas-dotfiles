(define atom?
  (lambda (x)
    (and (not (pair? x))
	 (not (null? x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2. Do it Again, and Again, And Again... ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define lat?
  (lambda (l)
    (cond ((null? l) #t)
	  ((atom? (car l))
	   (lat? (cdr l)))
	  (else #f))))

(define member?
  (lambda (a lat)
    (cond ((null? lat) #f)
	  (else
	   (or ((equal? (car lat) a) #f)	       
	       (member? a (cdr lat)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. Cons the Magnificent ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define rember
  (lambda (a lat)
    (cond ((null? lat) '())
	  ((eq? a (car lat))
	   (cdr lat))
	  (else
	   (cons (car lat)
		 (rember a (cdr lat)))))))
	 
(define firsts
  (lambda (l)
    (cond ((null? l) '())
	  (else
	   (cons (car (car l))
		 (firsts (cdr l)))))))

(define insertR
  (lambda (new old lat)
    (cond ((null? lat) '())
	  (else
	   (cond ((eq? old (car lat))
		  (cons old
			(cons new
			      (cdr lat))))
		 (else
		  (cons (car lat)
			(insertR new old (cdr lat)))))))))

(define insertL
  (lambda (new old lat)
    (cond ((null? lat) '())
	  ((eq? old (car lat))
	   (cons new lat))
	  (else
	   (cons (car lat)
		 (insertL new old (cdr lat)))))))

(define subst
  (lambda (new old lat)
    (cond ((null? lat) '())
	  ((eq? old (car lat))
	   (cons new (cdr lat)))
	  (else
	   (cons (car lat)
		 (subst new old (cdr lat)))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond ((null? lat) '())
	  ((or (eq? o1 (car lat))
	       (eq? o2 (car lat)))
	   (cons new (cdr lat)))
	  (else
	   (cons (car lat)
		 (subst2 new o1 o2 (cdr lat)))))))

(define multirember
  (lambda (a lat)
    (cond ((null? lat) '())
	  (else
	   (cond ((eq? a (car lat))
		  (multirember a (cdr lat)))
		 (else
		  (cons (car lat)
			(multirember a (cdr lat)))))))))

(define multiinsertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((eq? old (car lat))
	(cons old (cons new
			(multiinsertR new old (cdr lat)))))
       (else
	(cons (car lat)
	      (multiinsertR new old (cdr lat)))))))))
    
(define multisubst
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((eq? (car lat) old)
	(cons new (multisubst new old (cdr lat))))
       (else
	(cons (car lat)
	      (multisubst new old (cdr lat)))))))))


;;;;;;;;;;;;;;;;;;;;;
;; 4. Number Games ;;
;;;;;;;;;;;;;;;;;;;;;

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define o+
  (lambda (a b)
    (cond ((zero? b) a)
	  (else
	   (add1 (o+ a (sub1 b)))))))

(define o-
  (lambda (a b)
    (cond ((zero? b) a)
	  (else
	   (o- (sub1 a)
	       (sub1 b))))))

(define addtup
  (lambda (tup)
    (cond ((null? tup) 0)
	  (else
	   (o+ (car tup)
	       (addtup (cdr tup)))))))

(define x
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else
      (o+ n (x n (sub1 m)))))))

(define tup+
  (lambda (tup1 tup2)
    (cond
     ((null? tup1) tup2)
     ((null? tup2) tup1)
     (else
      (cons (o+ (car tup1) (car tup2))
	    (tup+ (cdr tup1)
		  (cdr tup2)))))))

(define >
  (lambda (n m)
    (cond ((zero? n) #f)
	  ((zero? m) #t)
	  (else
	   (> (sub1 n) (sub1 m))))))

(define <
  (lambda (n m)
    (cond ((zero? m) #f)
	  ((zero? n) #t)
	  (else
	   (< (sub1 n) (sub1 m))))))

(define =
  (lambda (n m)
    (cond ((zero? m) (zero? n))
	  ((zero? n) #f)
	  (else
	   (= (sub1 n) (sub1 m))))))

(define =2
  (lambda (n m)
    (cond ((> n m) #f)
	  ((< n m) #f)
	  (else #t))))

(define ^
  (lambda (n m)
    (cond ((zero? m) 1)
	  (else
	   (* n (^ n (sub1 m)))))))

(define length
  (lambda (lat)
    (cond ((null? lat) 0)
	  (else
	   (add1 (length (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond ((= n 1) (car lat))
	  (else
	   (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond ((zero? (sub1 n)) (cdr lat))
	  (else
	   (cons (car lat)
		 (rempick (sub1 n)
			  (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond ((null? lat) '())
	  (else
	   (cond ((number? (car lat))
		  (no-nums (cdr lat)))
		 (else
		  (cons (car lat)
			(no-nums (cdr lat)))))))))

(define all-nums
  (lambda (lat)
    (cond ((null? lat) '())
	  (else
	   (cond ((number? (car lat))
		  (cons (car lat)
			(all-nums (cdr lat))))
		 (else
		  (all-nums (cdr lat))))))))

;; = solo para numeros
;; eq? para el resto de atoms
(define eqan?
  (lambda (a1 a2)
    (cond ((and (number? a1) (number? a2))
	   (= a1 a2))
	  ((or (number? a1) (number? a2))
	   #f)
	  (else
	   (eq? a1 a2)))))

(define occur
  (lambda (a lat)
    (cond ((null? lat) 0)
	  (else
	   (cond ((eqan? (car lat) a)
		  (add1 (occur a (cdr lat))))
		 (else
		  (occur a (cdr lat))))))))

(define one?
  (lambda (n)
    (eqan? 1 n)))

(define rempick2
  (lambda (n lat)
    (cond ((one? n) (cdr lat))
	  (else
	   (cons (car lat)
		 (rempick2 (sub1 n)
			  (cdr lat)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 5. *Oh My Gawd*: It's full of Stars ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define rember*
  (lambda (a l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond ((eq? a (car l))
	     (rember* a (cdr l)))
	    (else
	     (cons (car l) (rember* a (cdr l))))))
     (else (cons (rember* a (car l))
		 (rember* a (cdr l)))))))

(define insertR*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond ((eq? (car l) old)
	     (cons old
		   (cons new
			 (insertR* new old (cdr l)))))
	    (else
	     (cons (car l)
		   (insertR* new old (cdr l))))))
     (else
      (cons (insertR* new old (car l))
	    (insertR* new old (cdr l)))))))

(define occur*
  (lambda (a l)
    (cond ((null? l) 0)
	  ((atom? (car l))
	   (cond ((eq? a (car l))
		  (add1 (occur* a (cdr l))))
		 (else
		  (occur* a (cdr l)))))
	  (else
	   (o+ (occur* a (car l))
	       (occur* a (cdr l)))))))

(define subst*
  (lambda (new old l)
    (cond ((null? l) '())
	  ((atom? (car l))
	   (cond ((eq? (car l) old)
		  (cons new
			(subst* new old (cdr l))))
		 (else
		  (cons (car l)
			(subst* new old (cdr l))))))
	  (else
	   (cons (subst* new old (car l))
		 (subst* new old (cdr l)))))))

(define insertL*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond ((eq? (car l) old)
	     (cons new
		   (cons old
			 (insertL* new old (cdr l)))))
	    (else
	     (cons (car l)
		   (insertL* new old (cdr l))))))
     (else
      (cons (insertL* new old (car l))
	    (insertL* new old (cdr l)))))))

(define member*
  (lambda (a l)
    (cond ((null? l) #f)
	  ((atom? (car l))
	   (cond ((eq? a (car l)) #t)
		 (else
		  (member* a (cdr l)))))
	  (else
	   (or (member* a (car l))
	       (member* a (cdr l)))))))
	   
(define leftmost
  (lambda (l)
    (cond ((atom? (car l)) (car l))
	  (else (leftmost (car l))))))

(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
     ((and (atom? (car l1)) (atom? (car l2)))
      (and (eqan? (car l1) (car l2))
	   (eqlist? (cdr l1) (cdr l2))))
     ((or (atom? (car l1)) (atom? (car l2))) #f)
     (else
      (and (eqlist? (car l1) (car l2))
	   (eqlist? (cdr l1) (cdr l2)))))))

(define equal?
  (lambda (s1 s2)
    (cond ((and (atom? s1) (atom? s2))
	   (eqan? s1 s2))
	  ((or (atom? s1) (atom? s2)) #f)
	  (else
	   (eqlist?2 s1 s2)))))

(define eqlist?2
  (lambda (l1 l2)
    (cond ((and (null? l1) (null? l2)) #t)
	  ((or (null? l1) (null? l2)) #f)
	  (else
	   (and (equal? (car l1) (car l2))
		(eqlist?2 (cdr l1) (cdr l2)))))))

(define rember2
  (lambda (s l)
    (cond ((null? l) '())
	  ((equal? s (car l))
	   (rember2 s (cdr l)))
	  (else
	   (cons (car l)
		 (rember2 s (cdr l)))))))


;;;;;;;;;;;;;;;;
;; 6. Shadows ;;
;;;;;;;;;;;;;;;;

(define numbered?
  (lambda (aexp)
    (cond ((atom? aexp) (number? aexp))
	  (else
	   (and (numbered? (car aexp))
		(numbered? (car (cdr (cdr aexp)))))))))

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (operator nexp) '+)
      (o+ (1st-sub-exp nexp) (2nd-sub-exp nexp)))
     ((eq? (operator nexp) '*)
      (x (1st-sub-exp nexp) (2nd-sub-exp nexp)))
     (else
      (expt (1st-sub-exp nexp) (2nd-sub-exp nexp))))))

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))


(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons '() n)))

(define zub1
  (lambda (n)
    (cdr n)))

(define oo+
  (lambda (n m)
    (cond
     ((sero? m) n)
     (else
      (edd1 (oo+ n (zub1 m)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7. Friends and Relations ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	   
(define set?
  (lambda (lat)
    (cond
     ((null? lat) #t)
     (else
      (cond
       ((zero? (occur (car lat) (cdr lat)))
	(set? (cdr lat)))
       (else #f))))))

(define set?
  (lambda (lat)
    (cond
     ((null? lat) #t)
     ((member? (car lat) (cdr lat)) #f)
     (else
      (set? (cdr lat))))))

(define makeset1
  (lambda (lat)
    (cond ((null? lat) '())
	  ((member? (car lat) (cdr lat))
	   (makeset (cdr lat)))
	  (else
	   (cons (car lat)
		 (makeset (cdr lat)))))))

(define makeset
  (lambda (lat)
    (cond ((null? lat) '())
	  (else
	   (cons (car lat)
		 (rember (car lat)
			 (makeset (cdr lat))))))))

(define subset?
  (lambda (set1 set2)
    (cond ((null? set1) #t)
	  (else (and (member? (car set1) set2)
		     (subset? (cdr set1) set2))))))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
	 (subset? set2 set1))))

(define intersetct?
  (lambda (set1 set2)
    (cond ((null? set1) #f)
	  (else
	   (or (member? (car set1) set2)
	       (intersect? (cdr set1) set2))))))

(define intersect
  (lambda (set1 set2)
    (cond ((null? set1) '())
	  ((member? (car set1) set2)
	   (cons (car set1)
		 (intersect (cdr set1) set2)))
	  (else
	   (intersect (cdr set1) set2)))))

(define union
  (lambda (set1 set2)
    (cond ((null? set1) set2)
	  ((member? (car set1) set2)
	   (union (cdr set1) set2))
	  (else
	   (cons (car set1)
		 (union (cdr set1) set2))))))

(define intersectall
  (lambda (l-set)
    (cond
     ((null? (cdr l-set)) (car l-set))
     (else
      (intersect (car l-set)
		 (intersectall (cdr l-set)))))))

(define a-pair?
  (lambda (x)
    (cond ((atom? x) #f)
	  ((or (null? x)
	       (null? (cdr x))) #f)
	  (else
	   (null? (cdr (cdr x)))))))

(define first
  (lambda (x)
    (car x)))

(define second
  (lambda (x)
    (car (cdr x))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define third
  (lambda (x)
    (car (cdr (cdr x)))))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define revrel
  (lambda (rel)
    (cond
     ((null? rel) '())
     (else
      (cons (build (second (car rel))
		   (first (car rel)))
	    (revrel (cdr rel)))))))

(define fullfun?
  (lambda (fun)
    (fun? (revrel fun)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 8. Lambda the Ultimate ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
