(defun display-first-60-chars ()
  "Displays the first 60 characters of the current buffer,
even if narrowed."
  (interactive)
  (save-restriction
    (widen)
    (save-excursion
      (message (buffer-substring-no-properties 1 60)))))
