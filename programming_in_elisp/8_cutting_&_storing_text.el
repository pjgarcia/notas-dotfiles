(defun test-search (string)
  "Searches for a STRING in current buffer.
Leaves the point after it, and prints a message."
  (interactive "sSearch for: ")
  (when (< (point) (search-forward string))
    (message "Found!")))

(defun print-third-kill-ring ()
  "Prints the third element of kill ring."
  (interactive)
  (let ((element (or (nth 2 kill-ring)
		     "Nothing found.")))
    (message "%s" element)))
