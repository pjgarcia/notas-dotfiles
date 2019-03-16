(call-process "ls" nil t nil "-l")
(let ((lines (process-lines "ls" "-l"))



