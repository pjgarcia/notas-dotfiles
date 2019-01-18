(defun squares-triangle (number-of-rows)
  "Add up the number of pebbles in a triangle.
The first row has one pebble, the second row four pebbles,
the third row nine pebbles, and so on.
The argument is NUMBER-OF-ROWS."
  (let ((total 0)
	(row-number 1))
    (while (<= row-number number-of-rows)
      (setq total (+ total (* row-number row-number)))
      (setq row-number (1+ row-number)))
    total))

(defun squares-triangle-multip (number-of-rows)
  "Multiplies the number of pebbles in a triangle.
The first row has one pebble, the second row four pebbles,
the third row nine pebbles, and so on.
The argument is NUMBER-OF-ROWS."
  (let ((total 1)
	(row-number 1))
    (while (<= row-number number-of-rows)
      (setq total (* total (* row-number row-number)))
      (setq row-number (1+ row-number)))
    total))

(squares-triangle-multip 3)

(defun squares-triangle-recur (row)
  "Add up the number of pebbles in a triangle.
The first row has one pebble, the second row four pebbles,
the third row nine pebbles, and so on.
The argument is NUMBER-OF-ROWS."
  (cond ((= row 1) 1)
	((> row 1)
	 (+ (* row row)
	    (squares-triangle-recur (1- row))))))

(defun create-paragraphs-dfn-index ()
  (interactive)
  (save-excursion
    (while (search-forward "@dfn{")
      (let ((opening-point (point))
	    (closing-point (search-forward "}")))
	(copy-region-as-kill opening-point (1- closing-point))
	(forward-paragraph -1)
	(yank)
	(goto-char closing-point)))))

"Getting rid of a file is called @dfn{hola} it."

"Getting rid of a file is called @dfn{deleting} and @dfn{sarasa} it."

