;;; shamedb.el --- interfaz para ShameDB  -*- lexical-binding: t; -*-

;;; Commentary:
;; hacer MAKEFILE en base a https://nullprogram.com/blog/2020/01/22/
 
;;; Code:

(require 'dom)

(defvar-local shamedb--report-date (format-time-string "%F")
  "Fecha del reporte, en formato YYYY-MM-DD.")
(defvar-local shamedb--logdir "logKen"
  "Directorio en la maquina logudp de los logs.")

(defun shamedb--sort-by-entries-qty (a b)
  "Dice si A tiene más entradas que B."  
  (let ((entries-a (elt (cadr a) 0))
        (entries-b (elt (cadr b) 0)))
    (if (> (string-to-number entries-a) (string-to-number entries-b)) t nil)))

(defun shamedb--filter-log-metadata (log)
  "Recorta metadata como uow, thread, etc de la linea de LOG."
  (string-join (seq-drop (split-string log " ") 7) " "))

(defun shamedb--fetch-report ()
  "Devuelve el DOM del reporte de `shamedb--report-date' y `shamedb--logdir'."
  (message "Buscando el reporte de Shame DB...")
  (let ((command (concat "curl -s http://10.2.7.101:1440/batch/"
                         shamedb--report-date "/"
                         shamedb--logdir "/")))
    (shell-command-to-string command)))

(defun shamedb--build-items-from-report (report)
  "Devuelve una lista con los items del REPORT.
Parsea el HTML, arma el DOM, y busca una tabla de donde obtener los items."
  (let ((dom (with-temp-buffer
               (insert report)
               (libxml-parse-html-region (point-min) (point-max))))
        (result))    

    (dolist (row (dom-by-tag (dom-by-tag dom 'tbody) 'tr) result)
      (let* ((row-elements (dom-by-tag row 'td))
             (entries-qty (dom-text (car row-elements)))
             (name (dom-texts (cadr row-elements)))
             (archetype (dom-text (caddr row-elements))))
        (setq result
              (cons (list name (vector entries-qty name (shamedb--filter-log-metadata archetype))) result))))))

(defun shamedb--change-date-by-days (date days)
  "Devuelve la fecha cambiando DATE en un numero de DAYS.
DATE debe tener el formato YYYY-MM-DD y DAYS es un entero."
  (let* ((current-time (date-to-time (concat date "T00:00:00")))
         (ct-in-seconds (time-convert current-time 'integer))
         (days-in-seconds (* 60 60 24 days)))                       
    (format-time-string "%F" (+ ct-in-seconds days-in-seconds))))
              
(defun shamedb--previous-day ()
  (interactive)
  "Cambia al reporte del dia anterior."
  (let* ((previous-day
          (shamedb--change-date-by-days shamedb--report-date -1))
         (previous-day-buffer-name (shamedb--buffer-name
                                    shamedb--logdir
                                    previous-day)))
    (if (get-buffer previous-day-buffer-name)
        (switch-to-buffer previous-day-buffer-name)
      (message "generando reporte del dia anterior...")
      (shamedb--report-in-new-buffer shamedb--logdir previous-day))))

(defun shamedb--current-day ()
  (interactive)
  "Cambia al reporte del dia actual."
  (message "generando reporte para hoy...")
  (let* ((today (format-time-string "%F"))
         (current-day-buffer-name (shamedb--buffer-name
                                   shamedb--logdir
                                   today)))
    (if (get-buffer current-day-buffer-name)
        (switch-to-buffer current-day-buffer-name)
    (setq shamedb--report-date today)
    (shamedb--build-report))))

(defun shamedb--report-in-new-buffer (logdir date)
  "Crea buffer para LOGDIR y DATE, y arma el reporte."
  (switch-to-buffer
   (get-buffer-create (shamedb--buffer-name logdir date)))
  (shamedb-mode)
  (setq shamedb--report-date date)
  (shamedb--build-report))

(defun shamedb--next-day ()
  (interactive)
  "Cambia al reporte del dia siguiente."  
  (let* ((next-day
          (shamedb--change-date-by-days shamedb--report-date 1))
         (next-day-buffer-name (shamedb--buffer-name
                                shamedb--logdir
                                next-day)))
    (if (get-buffer next-day-buffer-name)
        (switch-to-buffer next-day-buffer-name)
      (message "generando reporte del dia siguiente...")
      (shamedb--report-in-new-buffer shamedb--logdir next-day))))

(defun shamedb--buffer-name (logdir date)
  "Construye el nombre para el buffer del reporte de LOGDIR y DATE."
  (format "*Shame DB - %s - %s*" logdir date))

(defun shamedb--grep-error-on-line ()
  (interactive)
  "Muestra el error en el log correspondiente."
  ;TODO: puedo ejecutar un "ls -l" en el logdir y parsear la respuesta
  (message "Buscando el error en %s para la fecha %s:\n%s"
           shamedb--logdir
           shamedb--report-date
           (tabulated-list-get-entry)))

(defvar shamedb-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "(" 'shamedb--togle-log-metadata)
    (define-key map "P" 'shamedb--previous-day)
    (define-key map "N" 'shamedb--next-day)
    (define-key map "." 'shamedb--current-day)
    (define-key map "\r" 'shamedb--grep-error-on-line)
    map)
  "Keymap for Shame DB commands.")

(defun shamedb--build-report ()
  "Construye el reporte para `shamedb--report-date' y `shamedb--logdir'."  
  (setq-local tabulated-list-format
              [("Entries" 10 shamedb--sort-by-entries-qty)
               ("Name" 15 t)
               ("Archetype" 100 nil)])
  (tabulated-list-init-header)
  (setq-local tabulated-list-entries nil)    
  (let ((report (shamedb--fetch-report)))
    (dolist (item (shamedb--build-items-from-report report))
      (setq tabulated-list-entries
            (cons item tabulated-list-entries))))    
  (message "Armando tabla con %d entradas." (length tabulated-list-entries))
  (tabulated-list-print))

(define-derived-mode shamedb-mode
  tabulated-list-mode "Shame DB"
  "Major mode para ver los reportes de Shame DB.

\\{shamedb-mode-map}")

;TODO: agregar comando para mostrar o sacar los metadatos del log (puede ser "(" como Dired)
;TODO: cuando el numero es mayora NNN, ponerlo en ROJO
(defun list-shamedb-report ()
  "Muestra el reporte de Shame DB para `shamedb--report-date'."
  (interactive)
  (shamedb--report-in-new-buffer
   shamedb--logdir
   shamedb--report-date))

;;TODO: HACER COMANDO PARA IRME A ESE ERROR EN LOS LOGS, PARA UN SOCIALID
;; buscarle ahi la linea del reporte, quedarse con una y de eso solo su REQUESTID O SOCIALID
;; buscar ese ID nuevamente en el log y mostrarlo en un nuevo buffer
(defun shamedb--find-log ()
  "Devuelve el log correspondiente a la fecha `shamedb--report-date'."  
  (shamedb-find-dile-modified-on-date
   ;; los logs de un dia cierran (y se modifican) al siguiente
   (shamedb--change-date-by-days shamedb--report-date 1)))

(defun shamedb--find-file-modified-on-date (date)
  "Devuelve el nombre del log del directorio actual modificado por ultima vez en DATE."
  (let* ((find-upper-limit (shamedb--change-date-by-days date 1))
         (find-lower-limit date))
    (s-trim (shell-command-to-string
             (format "find . -type f -regex '.*log\\.[0-9]+' -newermt %s ! -newermt %s"
                     find-lower-limit
                     find-upper-limit)))))

(provide 'shamedb)
