;;;;;;;;;;;;;;;;;;;
;; Exercise 4.25 ;;
;;;;;;;;;;;;;;;;;;;

;; with applicative-order, if we attempt to evaluate (factorial 5),
;; it will hang forever, because on every application of unless the
;; arguments will be evaluated, and therefore (factorial (- n 1))
;; will continue running forever

;; with normal-order the combination (* n (factorial (- n 1))) will
;; not be evaluated when (= n 1), so the definition will work

;;;;;;;;;;;;;;;;;;;
;; Exercise 4.26 ;;
;;;;;;;;;;;;;;;;;;;

;; (unless (> age 18)
;;   (error "Only adult people can drive")
;;   'drive)
;; unless as syntax
(define (unless->if exp)
  (list 'if
	(list 'not (if-predicate exp))
	(if-consequent exp)
	(if-alternative exp)))

;; unless as normal procedure
(map unless conditions errors success)
