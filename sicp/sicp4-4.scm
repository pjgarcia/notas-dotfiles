;;;;;;;;;;;;;;;;;;;
;; Exercise 4.55 ;;
;;;;;;;;;;;;;;;;;;;

;; a) (supervisor ?x (Bitdiddle Ben))
;; b) (job ?x (accounting . ?y))
;; c) (address ?x  (Slummerville . ?y))

;;;;;;;;;;;;;;;;;;;
;; Exercise 4.56 ;;
;;;;;;;;;;;;;;;;;;;

;; a) (and (supervisor ?name (Bitdiddle))
;;         (address ?name ?address))
;; b) (and (salary ?person ?amount)
;;         (salary (Bitdiddle Ben) ?ben-amount)
;;         (lisp-value < ?amount ?ben-amount))
;; c) (and (supervisor ?name ?boss)
;;         (not (job ?boss (computer . ?supervisor-job)))
;;         (job ?boss ?job))

;;;;;;;;;;;;;;;;;;;
;; Exercise 4.57 ;;
;;;;;;;;;;;;;;;;;;;

(rule (can-replace ?person-1 ?person-2)
      (and (and (job ?person-1 ?job)
		(job ?person-2 ?job))
	   (not (same ?person-1 ?person-2))))

(can-replace ?person (Fect Cy D))

(and (can-replace ?person-1 ?person-2)
     (salary ?person-1 ?salary-1)
     (salary ?person-2 ?salary-2)
     (lisp-value > salary-2 salary-1))

;;;;;;;;;;;;;;;;;;;
;; Exercise 4.58 ;;
;;;;;;;;;;;;;;;;;;;

(rule (big-shot ?person ?division)
      (and (job ?person ?division)
	   (not (supervisor ?person ?supervisor)
		(job ?supervisor ?division))))


		
