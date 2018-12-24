
;; Structure of the Compiler
(define (compile exp target linkage)
  (cond ((self-evaluating? exp)
	 (compile-self-evaluating exp target linkage))
	((quoted? exp) (compile-quoted exp target linkage))
	((variable? exp)
	 (compile-variable exp target linkage))
	((assignment? exp)
	 (compile-assignment exp target linkage))
	((definition? exp)
	 (compile-definition exp target linkage))
	((if? exp) (compile-if exp target linkage))
	((lambda? exp) (compile-lambda exp target linkage))
	((begin? exp)
	 (compile-sequence (begin-actions exp)
			   target
			   linkage))
	((cond? exp) (compile (cond->if exp) target linkage))
	((application? exp)
	 (compile-application exp target linkage))
	(else
	 (error "Unknown expression type -- COMPILE" exp))))

;; Instructoin sequences and stack usage
(define (make-instruction-sequence needs modifies statements)
  (list needs modifies statements))

(define (empty-instruction-sequence)
  (list '() '() '()))

;; Compiling Expressions
;; Compilink linkage code
(define (compile-linkage linkage)
  (cond ((eq? linkage 'return)
	 (make-instruction-sequence '(continue) '()
				    '((goto (reg continue)))))
	((eq? linkage 'next)
	 (empty-instruction-sequence))
	(else
	 (make-instruction-sequence '() '()
				    `((goto (reg ,linkage)))))))

(define (end-with-linkage linkage instruction-sequence)
  (preserving '(continue) instruction-sequence
	      (compile-linkage linkage)))

;; Compiling simple expressions
(define (compile-self-evaluating exp target linkage)
  (end-with-linkage
   linkage
   (make-instruction-sequence '() (list target)
			      `((assign ,target (const ,exp))))))

(define (compile-quoted exp target linkage)
  (end-with-linkage
   linkage
   (make-instruction-sequence
    '() (list target)
    `((assign ,target (const ,(test-of-quotation exp)))))))

(define (compile-variable exp target linkage)
  (end-with-linkage
   linkage
   (make-instruction-sequence
    '(env) (list target)
    `((assign ,target
	      (op lookup-variable-value)
	      (const ,exp)
	      (reg env))))))

(define (compile-assignment exp target linkage)
  (let ((var (assignment-variable exp))
	(get-value-code
	 (compile (assignment-value exp) 'val 'next)))
    (end-with-linkage
     linkage
     (preserving '(env)
		 get-value-code
		 (make-instruction-sequence
		  '(env val) (list target)
		  '((perform (op set-variable-value!)
			     (const ,var)
			     (reg val)
			     (reg env))
		    (assign ,target (const ok))))))))

(define (compile-definition exp target linkage)
  (let ((var (definition-variable exp))
	(get-value-code
	 (compile (definition-value exp) 'val 'next)))
    (end-with-linkage
     linkage
     (preserving '(env)
		 get-value-code
		 (make-instruction-sequence
		  '(env val) (list target)
		  '((perform (op define-variable!)
			     (const ,var)
			     (reg val)
			     (reg env))
		    (assign ,target (const ok))))))))
