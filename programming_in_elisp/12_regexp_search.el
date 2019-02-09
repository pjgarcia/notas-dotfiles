(defun my-re-search-blank-lines ()
  "Searches for two or more blank lines in sequence."
  (interactive)
  (if (re-search-forward "^\n\n+")
      (goto-char (match-beginning 0))))



;;hola

;;chau


;;sarasa
