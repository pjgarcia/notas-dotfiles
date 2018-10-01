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



