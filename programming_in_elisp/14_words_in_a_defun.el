
(defun count-words-in-defun ()
  "Return the number of words and symbols in a defun."
  (beginning-of-defun)
  (let ((count 0)
	(end (save-excursion (end-of-defun) (point))))
    (while (and (< (point) end)
		(re-search-forward
		 "\\(\\w\\|\\s_\\)+[^ \t\n]*[ \t\n]*"
		 end t))
      (setq count (1+ count)))
    count))

(defun count-words-defun ()
  (interactive)
  (message "Counting words and symbols in function definition...")
  (let ((count (count-words-in-defun)))
    (cond ((zerop count)
	   (message "No words."))
	  ((= 1 count)
	   (message "One word."))
	  (t
	   (message "%d words." count)))))
      
(global-set-key "\C-c=" 'count-words-defun)

(defun lengths-list-file (filename)
  "Return list of definitions' lengths within FILE.
The returned list is a list of numbers.
Each number is the number of words or symbols
in one function definition."
  (message "Working on %s..." filename)
  (save-excursion
    (let ((buffer (find-file-noselect filename))
	  (lengths-list))
      (set-buffer buffer)
      (setq buffer-read-only t)
      (widen)
      (goto-char (point-min))
      (while (re-search-forward "^(defun" nil t)
	(setq lengths-list
	      (cons (count-words-in-defun) lengths-list)))
      (kill-buffer buffer)
      lengths-list)))

;; (lengths-list-file "~/notas-dotfiles/programming_in_elisp/11_loops_&_recursion.el")

(defun lengths-list-many-files (list-of-files)
  "Return list of lengths of defuns in LIST-OF-FILES."
  (let (lengths-list)
    (while list-of-files
      (setq lengths-list
	    (append lengths-list
		    (lengths-list-file
		     (expand-file-name (car list-of-files)))))
      (setq list-of-files (cdr list-of-files)))
    lengths-list))

(defun recursive-lengths-list-many-files (list-of-files)
  (cond ((null list-of-files) ())
	(t (append (lengths-list-file
		    (expand-file-name (car list-of-files)))
		   (recursive-lengths-list-many-files (cdr list-of-files))))))

(defun files-in-below-directory (dirname)
  "Return list of .el files in DIRNAME and any sub-directory."
  (interactive "DDirectory name: ")
  (let ((files-and-dirs (directory-files-and-attributes dirname))
	(result))
    (while files-and-dirs
      (let ((file (car files-and-dirs)))
	;; esto no va a funcionar, RE-SEARCH busca en un buffer
	(cond ((and (null (cadr file))
		    (equal ".el" (substring (car file) -3)))
	       (setq result (cons (car file) result)))
	      ;; is a dir, other than . or ..
	      ((and (cadr file)
		    (not (string= (car file) "."))
		    (not (string= (car file) "..")))
	       (setq result (append result
				    (files-in-below-directory (concat dirname "/" (car file))))))))
      (setq files-and-dirs (cdr files-and-dirs)))
    result))

(defvar top-of-ranges
  '(10  20  30  40  50
        60  70  80  90 100
	110 120 130 140 150
	160 170 180 190 200
	210 220 230 240 250
	260 270 280 290 300)
  "List specifying ranges for `defuns-per-range'.")

(defun defuns-per-range (sorted-lengths top-of-ranges)
  "SORTED-LENGTHS defuns in each of TOP-OF-RANGES range."
  (let (defuns-per-range-list
	 (number-within-range 0))
    (while top-of-ranges
      (while (and (car sorted-lengths)
		  (<= (car sorted-lengths) (car top-of-ranges)))
	(setq number-within-range (1+ number-within-range))
	(setq sorted-lengths (cdr sorted-lengths)))

      (setq defuns-per-range-list
	    (cons number-within-range defuns-per-range-list))
      (setq number-within-range 0)
      (setq top-of-ranges (cdr top-of-ranges)))

    ;; para agregar las funciones por encima del rango superior
    (setq defuns-per-range-list
	  (cons (length sorted-lengths)
		defuns-per-range-list))
    
    (nreverse defuns-per-range-list)))

(defuns-per-range '(1 1 2 5 25 100 101 150 190)
  '(10 20 30 40 100 120))
      
	

