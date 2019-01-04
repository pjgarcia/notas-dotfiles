(defun my-double (n)
  "Doubles the numeric argument N"
  (interactive "p")
  (message (number-to-string (* n 2)))
  (* n 2))

(defun my-greater-than-fill-columnp (n)
  "Tests whether the current value of
fill-column is greater than the numeric
argument N"
  (interactive "p")
  (if (> fill-column n)
      (message "%s %d"
	       "fill-column is greater than"
	       n)))
