(insert-rectangle '("first" "second" "third"))

(defvar graph-symbol "*" "String used as a symthirdbol in graph.")
(defvar graph-blank " " "String used as a blank in graph.")


(defun column-of-graph (max-graph-height actual-height)
  "Return the list of strings that is one column of a graph."
  (let (column
	(spaces (- max-graph-height actual-height)))
    (while (> actual-height 0)
      (setq column (cons graph-symbol column))
      (setq actual-height (1- actual-height)))
    (while (> spaces 0)
      (setq column (cons graph-blank column))
      (setq spaces (1- spaces)))
    column))

(defun graph-body-print (numbers-list)
  "Print a bar graph of the NUMBERS-LIST.
The numbers-list consist of the y-axis values."
  (let ((height (apply 'max numbers-list))
	(symbol-width (length graph-symbol))
	from-position)
    (while numbers-list
      (setq from-position (point))
      (insert-rectangle
       (column-of-graph height (car numbers-list)))
      (goto-char from-position)
      (forward-char symbol-width)
      ;; draw graph column by column
      (sit-for 0)
      (setq numbers-list (cdr numbers-list)))
    ;; Place point for X axis labels.
    (forward-line height)
    (insert "\n")))

(graph-body-print '(1 2 3))


(defun recursive-print (columns height symbol-width)
  (unless (null columns)
    (let ((from-position (point)))
      (insert-rectangle
       (column-of-graph height (car columns)))
      (goto-char from-position)
      (forward-char symbol-width)
      (sit-for 0)
      (recursive-print (cdr columns)
		       height
		       symbol-width))))

(defun recursive-graph-body-print (numbers-list)
  (let ((height (apply 'max numbers-list))
	(symbol-width (length graph-symbol)))
    (recursive-print numbers-list height symbol-width)
    (forward-line height)
    (insert "\n")))

(recursive-graph-body-print '(1 2 3))


(defun line-graph-body-print (numbers-list)
  (let ((height (apply 'max numbers-list)))
    (insert "\n")
    (dolist (number numbers-list)
      (dotimes (n number)
	(insert graph-symbol))
      (dotimes (n (- height number))
	(insert graph-blank))
      (insert "\n"))))

(line-graph-body-print '(5 2 3))

