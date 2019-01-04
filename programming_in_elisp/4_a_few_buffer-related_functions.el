(defun simplified-beginning-of-buffer ()
  "Move the cursor to the beginning of the buffer;
leave mark at previous position."
  (interactive)
  (push-mark)
  (goto-char (point-min)))

(defun simpified-end-of-buffer ()
  "Move the cursor to the end of the buffer;
leave mark at previous position."
  (interactive)
  (push-mark)
  (goto-char (point-max)))


(defun buffer-existsp (buffer)
  "Prints a message if the given buffer exists"
  (interactive (list (read-buffer
		      "Buffer to check: "
		      (other-buffer (current-buffer) t))))
  (if (get-buffer buffer)
      (message "The buffer %s exists" buffer)
    (message "The buffer %s does not exist" buffer)))


