;; "DEFPARAMETER" define variables globales (top-level definition).
(defparameter *small* 1)
;; a diferencia de defparameter, no sobreescribe la variable si ya existe.
(defvar *small* 5)
;; "SETF" setea las variables globales.
(setf *small* 4) 

;; "LET" define variables locales. Podemos usarlas SOLO en el cuerpo.
(let (variable declarations)
  ...body...)
(let ((a 5)
      (b 1))
  (+ a b))




;; "DEFUN" para definir funciones. (...) es el cuerpo, que puede tener varias
;; expresiones. La funcion retorna el valor de la ultima expresion evaluada.
(defun function_name (arguments) ...)
(defun start-over ()
  (defparameter *small* 1)
  (defparameter *big* 100)
  (guess-my-number))
;; "LABELS" es como let, pero para declarar funciones. Ademas, (a diferencia de "FLET")
;; permite que las funciones declaradas se llamen a si mismas o entre si (recursividad).
(labels ((func-a (n)
		 (* n 2))
	 (func-b (n)
		 (+ (func-a n) 1)))
	(b 2))
;; "DEFMETHOD" es como defun pero permite definir varias funciones con el mismo nombre. Permite al
;; compilador aplicar "type dispatching".
(defmethod add ((a number) (b number))
  (+ a b))
(defmethod add ((a list) (b list))
  (append a b))

;; "PROGN" permite evaluar varias expresiones, y retornar el valor de la ultima.
;; Por eso se lo suele usar en los "IF". Los condicionales "COND", "WHEN" y "UNLESS" lo tienen implicito.
(if (oddp 5)
    (progn (setf *number-was-odd* t)
	   'odd number)
  'even-number)

;; se usa "EQ" para comparar simbolos, "EQUAL" para todo lo demas.
(eq 'a 'a)
(equal '(a b c) '(a b c))


;; "`" es "cuasiquote". Permite crear datos con porciones de codigo Lisp embebido.
;; con la "," se indica que es codigo, y no dato.
`(there is a ,(car (door basket)) going ,(west south) from here.)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HIGH-ORDER FUNCTIONS  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; para las funciones que utilizan otras funciones como argumentos (high-order functions)
;; se usa "#'nombre" (como atajo de "(function nombre)") para indicar que es el nombre
;; de una funcion, y NO el de una variable, por como maneja CL los enviroments.
(mapcar #'car '((foo bar) (baz qux))) > (foo baz)
;; "MAPCAR" aplica la funcion pasada como parametro, a cada elemento de la lista, y los mete en otra lista.
;; "MAPC" idem "mapcar" pero no retorna la lista resultado, sino la misma lista de entrada.
;; (se usa generalmente para imprimir en pantalla). Es mas eficiente que "mapcar".
(maplist #'cdr '(a b c)) > ((b c) (c) nil)
;; "MAPLIST" aplica la funcion a todo en la lista desde el corriente item hasta el final. Y devuelve, como
;; "mapcar" la lista resultado.
;; "MAPCAN" es como "mapcar" pero asume que los valores generados son listas, y las appendea.
(mapcan #'cdr '((foo bar) (baz qux))) > (bar qux)
;; "MAP" es como "mapcar" pero para cualquier tipo de entrada, no solo listas.
;; "APPLY" pasa cada elemento de la lista como argumento para la funcion deseada.
(apply #'append '((mary had) (a little) (lamb)))
;; "APPEND" toma los elementos de las listas pasadas como argumento, y los junta en otra.
;; "SUBSTITUTTE-IF", dada una condicion determinada por una funcion, reemplaza o no el argumento deseado
;; dentro de la lista o string pasada como ultimo argumento
(substitute-if #\e (complement #'oddp) '(1 2 3 4 5))
;; "COMPLEMENT" se usa para negar funciones, como si fuese negado un valor booleano (not nil)




;; "ASSOC", dada una clave, busca a lo que esta asociada, y devuelve el conjunto.
(assoc 'nombre '((nombre pedro) (edad 22) (peso 80))) ;; es (nombre pedro)
;; "PUSH" pone un nuevo item en una lista ya definida
(push 8 *numeros*)
;; "LIST" crea una lista que contiene a los argumentos
(list 'quote 'hola) ; > (quote hola)



;;;;;;;;;;;;;;;;;;
;; READ Y PRINT ;;
;;;;;;;;;;;;;;;;;;
(print 'asdf) > ASDF
(print "asdf") > "asdf"
(prin1 'asdf) > ASDF ;; igual a print, salvo que NO lo imprime en una nueva linea.
(princ "asdf") > asdf ;; imprime una salida "para humanos"
(princ '#\a) > a

;; WRITE-TO-STRING, o sus variantes mas simples, PRIN1-TO-STRING y PRINC-TO-STRING pasan objetos a strings
(write-to-string 250 :base 5) > "2000"




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ADVANCED DATATYPES & GENERIC FUNCTIONS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; los arrays son equivalentes a las listas, pero con mucha mejor performance
(make-array 4) > #(nil nil nil nil)
(setf foo (make-array 2)) > foo > #(nil nil)
(aref foo 2) > nil
;; AREF se usa para obtener el valor en la posicion n, y (en conjunto con setf) para setear un valor
;; "setf" es un "setter generico"
(setf (aref foo 2) 'a) > foo > #(nil 2)
;; las tablas hash son como las alist (listas de asociaciones), pero con mejor performance
(make-hash-table) > #S(hash-table ...)
;; "DEFSTRUCT" define una estructura (clase en POO) y crea automaticamente funciones para acceder a ella.
;; "MAKE-(nombre_del_struct)" crea un struct de ese tipo.
(defstruct person
  name
  age
  waist-size
  favorite-color)
(defparameter bob (make-person :name "Bob" :age 35 :waist-size 32 :favorite-color "blue"))
(person-age bob) > 35 ;; un ejemplo de las funciones creadas automaticamente para acceder
(setf (person-age bob) 36)


; "VALUES" hace que una funcion retorne mas de un valor. Lisp considera el primero como el mas importante.
(defun foo ()
  (values 1 2))
> (foo)
1
2
> (* 8 (foo))
8
> (multiple-value-bind (a b) (foo)
  (* a b))
2

;; "REDUCE" es un ejemplo de "generic sequence function". Itera a travez de la secuencia y devuelve un valor.
(reduce #'+ '(3 4 6 5 2)) > 20

