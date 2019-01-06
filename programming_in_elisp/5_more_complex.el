(defun compare-to-fill-column (&optional n)
  "Tests wether N is greater, equal, or less than the value
of `fill-column`. Tell the result in a message"
  (interactive "P")
  (let ((num (if n (prefix-numeric-value n) 56)))
    (if (> num fill-column)
  	(message "%d is greater than fill-column (%d)"
  		 num fill-column)
      (if (< num fill-column)
  	  (message "%d is lesser than fill-column (%d)"
  		   num fill-column)
  	(message "%d is equal to fill-column (%d)"
  		 num fill-column)))))
