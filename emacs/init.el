(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; Initialise the packages
(package-initialize)

;; Make sure `use-package' is available.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Configure `use-package' prior to loading it.
(eval-and-compile
  (setq use-package-enable-imenu-support t)
  ;; The following is VERY IMPORTANT.  Write hooks using their real name
  ;; instead of a shorter version: after-init ==> `after-init-hook'.
  ;;
  ;; This is to empower help commands with their contextual awareness,
  ;; such as `describe-symbol'.
  (setq use-package-hook-name-suffix nil))

(eval-when-compile
  (require 'use-package))


(use-package emacs
  :init
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)

  :config
  (setq create-lockfiles nil)
  (setq text-scale-mode-step 1.05)
  (setq use-file-dialog nil)
  (setq use-dialog-box t)               ; only for mouse events
  (setq inhibit-splash-screen t)
  (setq-default indent-tabs-mode nil)
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-x C-z"))
  (global-unset-key (kbd "C-h h"))

  (setq isearch-lazy-count 1)

  (winner-mode 1)

  (setq-default tab-always-indent 'complete)

  (setq backup-inhibited t)
  (setq auto-save-default nil)

  (add-to-list 'default-frame-alist
               '(fullscreen . maximized))
  
  (defun pulse-line (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point)))

  (dolist (command '(scroll-up-command scroll-down-command
                                       recenter-top-bottom other-window))
    (advice-add command :after #'pulse-line))

  (defvar prot/window-configuration nil
    "Current window configuration.
Intended for use by `prot/window-monocle'.")

  (defun prot/window-single-toggle ()
    "Toggle between multiple windows and single window.
This is the equivalent of maximising a window.  Tiling window
managers such as DWM, BSPWM refer to this state as 'monocle'."
    (interactive)
    (if (one-window-p)
        (when prot/window-configuration
          (set-window-configuration prot/window-configuration))
      (setq prot/window-configuration (current-window-configuration))
      (delete-other-windows)))

  (defun my-window-registers-config ()
    "Lo mismo que Eyebrowse pero con registers."
    (interactive)
    ;; agenda REG 1
    (org-agenda nil "c")
    (delete-other-windows)
    (window-configuration-to-register ?1)
    ;; shells extras (no lo pongo en ningun REG, antes estaba en REG 4)
    ;; (shell "*shell-logs*") (hacer que el default-directory sea el de los logs remotos y ahi abrir el shell)
    ;; (shell "*shell-vpn*")
    ;; (end-of-line)
    ;; (insert "vpn-pulse")
    ;; (comint-send-input)
    ;; KEN git & sheell REG 2
    (magit-status "~/despegar/ken/")
    (delete-other-windows)
    (window-configuration-to-register ?2)
    ;; dejo el shell afuera del registro 2, asi solo queda magit
    (shell "*shell-normal*")
    (end-of-line)
    ;; restclient REG 3
    (find-file "~/despegar/requests/")
    (delete-other-windows)
    (window-configuration-to-register ?3))

  (defun my-kill-buffers (str)
    "Cierra los buffers que tienen STR en su fila de *Buffer List*"
    (interactive "sSubstring a buscar: ")
    (with-current-buffer (get-buffer "*Buffer List*")
      (goto-char (point-min))
      (while (search-forward str nil t)
		(Buffer-menu-delete))
      (Buffer-menu-execute)
      (message "Borranding los buffers que matchearon...")))

  (defun bachi-agregar-nombre-profes ()
    "Arma una lista de los profes para tareas de Org-Mode"
    (interactive)
    (apply #'insert
           (mapcar (lambda (nombre) (format "- [ ] %s\n" nombre)) '
                   ("Juli" "Angie" "Romi" "Feli" "Flor" "Mateo" "Laura" "Mile"
                    "Claudia" "Jose" "Ivana" "Pilar" "Pao"
                    "Agustin" "Ernesto" "Ines" "Martin"
                    "Eli"))))

  (defun jao-toggle-selective-display (column)
    (interactive "P")
    (set-selective-display
     (if selective-display nil (or column 1))))

  :hook (after-init-hook . column-number-mode)
  :bind ([f1] . jao-toggle-selective-display))

(use-package deadgrep
  :bind ([f7] . #'deadgrep))

;; (use-package org-jira
;;   :load-path "/home/pedro/workspace/org-jira"
;;   :config
;;   (setq jiralib-url "https://jira.despegar.com"))


(use-package eglot
  :ensure t
  :config
  (add-to-list 'eglot-server-programs
               `(((js-mode :language-id "javascript")
				  (js-ts-mode :language-id "javascript")
				  (tsx-ts-mode :language-id "typescriptreact")
				  (typescript-ts-mode :language-id "typescript")
				  (typescript-mode :language-id "typescript"))
				 "typescript-language-server" "--stdio"
				 ;; (:tsserver (:logVerbosity "verbose" :logDirectory "/tmp/tsls-logs/"))
				 ))
  :hook ((js-mode-hook tsx-ts-mode-hook typescript-ts-mode-hook java-ts-mode-hook) . eglot-ensure))

(use-package treesit
  :init
  (setq treesit-language-source-alist
        ;;(LANG . (URL REVISION SOURCE-DIR CC C++))
        '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
          (c . ("https://github.com/tree-sitter/tree-sitter-c"))
          (css . ("https://github.com/tree-sitter/tree-sitter-css"))
          (html . ("https://github.com/tree-sitter/tree-sitter-html"))
          (java . ("https://github.com/tree-sitter/tree-sitter-java"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
          (json . ("https://github.com/tree-sitter/tree-sitter-json"))
          (lua . ("https://github.com/Azganoth/tree-sitter-lua"))
          (make . ("https://github.com/alemuller/tree-sitter-make"))
          (python . ("https://github.com/tree-sitter/tree-sitter-python"))
          (php . ("https://github.com/tree-sitter/tree-sitter-php"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
          (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
          (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
          (sql . ("https://github.com/m-novikov/tree-sitter-sql")))))

(use-package prog-mode
  :hook
  ((prog-mode-hook . (lambda () (setq tab-width 4)))
   (prog-mode-hook . indent-tabs-mode)))

(use-package java-ts-mode
  :mode "\\.java\\'")

(use-package kotlin-mode
  :mode "\\.kt\\'")

(use-package typescript-mode
  :ensure t
  :hook (typescript-mode-hook . (lambda ()
                                  (setq indent-tabs-mode t)
                                  (setq tab-width 4))))

(use-package typescript-ts-mode
  :ensure t
  :config
  (setq typescript-ts-mode-indent-offset 4))

(use-package web-mode
  :ensure t
  :mode
  (("\\.ftl\\'" . web-mode)
   ("\\.html?\\'" . web-mode))
  :hook
  ((web-mode-hook . (lambda ()
                      (setq web-mode-code-indent-offset 2)
                      (setq indent-tabs-mode t)
                      (web-mode-use-tabs)))))

(use-package nxml-mode
  :config
  (setq nxml-child-indent 4)
  :hook
  (nxml-mode-hook . (lambda ()
					  (setq tab-width 4)
					  (setq indent-tabs-mode t))))

(use-package sql
  :ensure
  :config
  (setq sql-product 'mysql)
  (setq sql-connection-alist
		'(("loyalty-lotso-prod"
		   (sql-product 'mysql)
		   (sql-user "pjgarcia")
		   (sql-password (funcall (plist-get (nth 0 (auth-source-search :max 1 :host "mariadb-loyalty-r-00")) :secret)))
		   (sql-port 64280)
		   (sql-server "mariadb-loyalty-r-00")
		   (sql-database "loyalty_lotso"))
		  ("loyalty-lotso-sbox"
		   (sql-product 'mysql)
		   (sql-user "pjgarcia")
           (sql-password (funcall (plist-get (nth 0 (auth-source-search :max 1 :host "mariadb-loyalty-sb-00")) :secret)))
		   (sql-port 64280)
		   (sql-server "mariadb-loyalty-sb-00")
		   (sql-database "loyalty_lotso"))
		  ("loyalty-lotso-beta"
		   (sql-product 'mysql)
		   (sql-user "lotso_beta_app")
           (sql-password (funcall (plist-get (nth 0 (auth-source-search :max 1 :host "loyal-mariadb-b-00")) :secret)))
		   (sql-port 33033)
		   (sql-server "loyal-mariadb-b-00")
		   (sql-database "loyalty_lotso")))))


(use-package rmail
  :config
  (setq rmail-preserve-inbox t)
  (setq rmail-movemail-flags '("--uidl" ))
  (push "imaps://pmauro%40posteo.net:s7TUkjaZQc2a@posteo.de:993"
        rmail-inbox-list))

(use-package gnus
  :init
  (setq gnus-gcc-mark-as-read t)
  ;; Set name and email:  
  (setq user-full-name "Pedro Mauro"
        user-mail-address "pmauro@posteo.net"))

(use-package paren
  :config
  (setq show-paren-when-point-in-periphery t)
  :hook (after-init-hook . show-paren-mode))

(use-package hl-line
  :init
  (global-hl-line-mode 1))

(use-package savehist
  :config
  (setq savehist-save-minibuffer-history t)
  (savehist-mode 1))

(use-package ibuffer
  :bind (("C-x C-b" . ibuffer)))

(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode))

(use-package dired
  :config
  (setq dired-listing-switches "-hl")
  (setq dired-dwim-target t)

  ;; advice para hacer que Dired abra musica y videos en VLC
  (defun my-dired-open-externally-or-find-file (orig-fun &rest args)
    "Use xdg-open for certain types of files and dired-find-file for the others."
    (let ((filename (dired-get-file-for-visit)))
      (cond ((or (string-suffix-p "mp4" filename t)
                 (string-suffix-p "mp3" filename t)
                 (string-suffix-p "mkv" filename t)
                 (string-suffix-p "ogv" filename t))
             (start-process "vlc" nil "vlc" filename))
            (t (apply orig-fun args)))))

  (advice-add 'dired-find-file :around #'my-dired-open-externally-or-find-file))

;; todavia falta bindearlo a M-?
;; ver por que xref-find-references no cambia el current-buffer (lo hace solo cuando es interactivo)
(use-package xref
  :config
  (setq xref-prompt-for-identifier nil)
  (defun my-xref--only-one-reference-p ()
    "Devuelve t si hay solo una referencia."
    (dotimes (i 2 result)
      (setq result (xref--search-property 'xref-item)))
    (not result))

  (defun my-xref--jump-when-one-ref ()
    (interactive)
    "Navega a la referencia cuando es la unica."
    (xref-find-references (xref--read-identifier "Find references of: "))
    (with-current-buffer "*xref*"
      (when (my-xref--only-one-reference-p)
        (xref-goto-xref t)))))



(use-package eshell
  :config
  (defun eshell-here ()
    "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
    (interactive)
    (let* ((parent (if (buffer-file-name)
                       (file-name-directory (buffer-file-name))
                     default-directory))
           (height (/ (window-total-height) 3))
           (name   (car (last (split-string parent "/" t)))))
      (split-window-vertically (- height))
      (other-window 1)
      (eshell "new")
      (rename-buffer (concat "*eshell: " name "*"))

      (insert (concat "ls"))
      (eshell-send-input)))
  (defun eshell/x ()
    (insert "exit")
    ;; ERROR: al cerrar eshell con "x", (eshell-send-input) tira el siguiente error.
    ;; Wrong type argument: integer-or-marker-p, nil
    ;; x
    (eshell-send-input)
    (delete-window))
  :bind ("C-!" . eshell-here))





(use-package restclient
  :ensure t
  :config
  (add-to-list 'auto-mode-alist
			   '("/home/pedro/despegar/requests/.*" . restclient-mode)))

(use-package vlf
  :ensure t
  :config
  (require 'vlf-setup))

(use-package visual-regexp :ensure t)

(use-package yaml-mode :ensure t)
(use-package markdown-mode :ensure t)

(use-package magit
  :ensure t
  :config
  (setq magit-process-finish-apply-ansi-colors t)
  (setq magit-repository-directories '(("~/despegar/" . 1)))
  :bind ("C-c g" . magit-status)
  :hook (magit-mode-hook . (lambda () (setq tab-width 4))))


(use-package tramp
  :config
  (setq tramp-default-method "ssh")
  (setq tramp-verbose 1)
  (setq tramp-completion-reread-directory-timeout nil)
  (setq vc-ignore-dir-regexp
		(format "\\(%s\\)\\|\\(%s\\)"
				vc-ignore-dir-regexp
				tramp-file-name-regexp))
  (setq explicit-shell-file-name "/bin/bash")

  (add-to-list 'tramp-methods
			   '("ssh-cloud-logs"
				 (tramp-tmpdir "/home/logger/workspace")
				 (tramp-login-program "ssh")
				 (tramp-login-args
				  (("-l" "%u")
				   ("-p" "%p")
				   ("%c")
				   ("-e" "none")
				   ("%h")))
				 (tramp-async-args
				  (("-q")))
				 (tramp-remote-shell "/bin/sh")
				 (tramp-remote-shell-login
				  ("-l"))
				 (tramp-remote-shell-args
				  ("-c"))
				 (tramp-gw-args
				  (("-o" "GlobalKnownHostsFile=/dev/null")
				   ("-o" "UserKnownHostsFile=/dev/null")
				   ("-o" "StrictHostKeyChecking=no")))
				 (tramp-default-port 22))))


(use-package org
  :commands (my-switch-to-agenda)
  
  :config
  (setq org-log-done 'time)
  (setq org-html-checkbox-type 'html)
  (setq org-deadline-warning-days 5) ;; pongo 5 para el alquiler
  (setq org-agenda-include-diary t)

  (defun ar/org-insert-link-dwim ()
    "Like `org-insert-link' but with personal dwim preferences."
    (interactive)
    (let* ((point-in-link (org-in-regexp org-link-any-re 1))
           (clipboard-url (when (string-match-p "^http" (current-kill 0))
                            (current-kill 0)))
           (region-content (when (region-active-p)
                             (buffer-substring-no-properties (region-beginning)
                                                             (region-end)))))
      (cond ((and region-content clipboard-url (not point-in-link))
             (delete-region (region-beginning) (region-end))
             (insert (org-make-link-string clipboard-url region-content)))
            ((and clipboard-url (not point-in-link))
             (insert (org-make-link-string
                      clipboard-url
                      (read-string "title: "
                                   (with-current-buffer (url-retrieve-synchronously clipboard-url)
                                     (dom-text (car
                                                (dom-by-tag (libxml-parse-html-region
                                                             (point-min)
                                                             (point-max))
                                                            'title))))))))
            (t
             (call-interactively 'org-insert-link)))))
  
  (defun my-switch-to-agenda ()
    "Cambia al buffer de Org Agenda (si existe). Si no, lo crea. 
Tambien cambia a la window config (registers) que deberia."  
    (interactive)
    (let ((agenda-buffer
		   (seq-filter (lambda (buffer)
						 (string= (buffer-name buffer)
								  "*Org Agenda*"))
					   (buffer-list))))
      (if agenda-buffer
		  (switch-to-buffer (nth 0 agenda-buffer))
		(org-agenda))))

  ;; cambios para org-mode vistos en https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html
  ;; tambien en http://doc.norang.ca/org-mode.html
  (setq org-agenda-files '("~/despegar/notas/enrollment.org"
						   "~/notas-dotfiles/notas.org.gpg"
						   "~/notas-dotfiles/bachi_la_poderosa/bachi.org.gpg"))
  (setq org-agenda-custom-commands '(("c" "Simple agenda view" ((agenda "") (alltodo "")))))
  (setq org-capture-templates
		'(("a" "TODO laboral." entry (file+headline "~/despegar/notas/enrollment.org" "Tareas de trabajo")
		   "* TODO %?\n  SCHEDULED: %t\n  %a")
		  ("p" "TODO personal." entry (file+headline "~/notas-dotfiles/notas.org.gpg" "Tasks")
		   "* TODO %?\n  SCHEDULED: %t\n  %a")
		  ("b" "TODO bachi." entry (file+headline "~/notas-dotfiles/bachi_la_poderosa/bachi.org.gpg" "Tasks")
		   "* TODO %?\n  SCHEDULED: %t\n  %a")
		  ("s" "trabajo de Silvia" table-line (file+olp "~/notas-dotfiles/notas.org.gpg" "Tasks" "generar recibo de Silvia en AFIP")
		   "| %^{Fecha}u | 20000 |")
		  ("o" "Objetivos del PDP de Despe")
		  ("oe" "Relevar error social-login y login-wapi" entry (file+headline "~/despegar/notas/enrollment.org" "Relevar errores de social-login y login-wapi") "* TODO %?\n  SCHEDULED: %t\n  %a")
		  ("oa" "Resolver alarma" entry (file+headline "~/despegar/notas/enrollment.org" "Resolver 1 alarma por semana") "* TODO %?\n  SCHEDULED: %t\n  %a")
		  ("ob" "Backlog tecnico" entry (file+headline "~/despegar/notas/enrollment.org" "Generar Backlog tecnico") "* TODO %?\n  SCHEDULED: %t\n  %a")
		  ("or" "Responder pedido" entry (file+headline "~/despegar/notas/enrollment.org" "Dar respuesta a un pedido por mail/chat de otro equipo por semana") "* TODO %?\n  SCHEDULED: %t\n  %a")
		  ("ot" "Charla tecnica" entry (file+headline "~/despegar/notas/enrollment.org" "Dar charla tecnica") "* TODO %?\n  SCHEDULED: %t\n  %a")
		  ("oc" "Chalenge ideas y PRs" entry (file+headline "~/despegar/notas/enrollment.org" "Challengear ideas, prioridades y PRs, anticipar problemas") "* TODO %?\n  SCHEDULED: %t\n  %a")
		  ("om" "Minuta" entry (file+headline "~/despegar/notas/enrollment.org" "Asentar minutas") "* TODO %?\n  SCHEDULED: %t\n  %a")))

  (defun orgtbl-to-AFIP (table params)
	"Convierte la TABLA orgtbl-mode a una descripcion para presentar en AFIP."
	;; (format-time-string "%A %d" (date-to-time "2023-06-06"))
	;; (:fmt (2 "$%s$" 4 (lambda (c) (format "$%s$" c))))
	;; (org-timestamp-to-time "[2023-06-06 vie]")
	(let ((horas (* 4 (seq-length (seq-drop table 2))))
		  (remuneracion (seq-reduce #'+ (mapcar #'string-to-number
												(mapcar #'cadr (seq-drop table 2))) 0))
		  (descripcion  (orgtbl-to-generic
						 table
						 (org-combine-plists
						  '(:tstart "Este recibo corresponde a las 4 horas de trabajo en las jornadas:"
									:tend "Fueron abonadas al finalizar cada jornada."
									:lend "\n"
									:skip 1
									:skipcols (2)
									:fmt (1 (lambda (c) (format-time-string "%A %d" (org-time-string-to-time c)))))
						  params))))
	  (format "
Remuneracion: %s
========================
Horas trabajadas: %s
========================
%s
" remuneracion horas descripcion)))

  (defun hacer-reporte-afip ()
	"Procesa el headline del recibo de Silvia."
	(require 'org-plot)
	(if (and (seq-contains-p (org-get-tags-at) "reporte_afip")
			 (string= org-state "IN-PROGRESS"))
		(let* ((current-month (decoded-time-month (decode-time (current-time))))
			   (last-month (if (= 1 current-month) 12 (1- current-month)))
			   (year (- (decoded-time-year (decode-time (current-time))) (if (= 1 current-month) 1 0)))
			   (filename (format "~/notas-dotfiles/reportes-afip/silvia-%d-%02d" year last-month)))
		  (org-plot/goto-nearest-table)
		  (org-table-export filename)
		  (find-file filename))))

  (defun borrar-datos-de-tabla ()
	"Elimina los datos cargados en la tabla."
	(require 'org-plot)
	(if (and (seq-contains-p (org-get-tags-at) "reporte_afip")
			 (string= org-state "DONE"))
		(progn
		  (org-plot/goto-nearest-table)
		  (next-line 2)
		  (while (not (org-at-table-hline-p))
			(org-table-kill-row)))))

  (setq org-refile-targets '((org-agenda-files . (:regexp . "Tasks"))
							 (org-agenda-files . (:regexp . "Tareas de trabajo"))
							 (org-agenda-files . (:tag . "errores"))
							 (org-agenda-files . (:tag . "alarmas"))
							 (org-agenda-files . (:tag . "backlog"))
							 (org-agenda-files . (:tag . "pedidos"))
							 (org-agenda-files . (:tag . "charla"))
							 (org-agenda-files . (:tag . "challengear"))
							 (org-agenda-files . (:tag . "minuta"))))
  ;; para que no pida confirmacion al ejecutar un code-block
  (setq org-confirm-babel-evaluate nil)
  ;; para poder ejecutar org code-blocks
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (python . t)
     (sql . t)
     (ruby . t)))

  :bind (("C-c k" . 'org-capture)
		 ([f12] . 'my-switch-to-agenda))

  :hook
  ((org-agenda-mode-hook . (lambda () (local-set-key (kbd "r") 'org-agenda-refile)))
   (org-mode-hook . visual-line-mode)
   (org-after-todo-state-change-hook . hacer-reporte-afip)
   (org-after-todo-state-change-hook . borrar-datos-de-tabla)))

(use-package minibuffer
  :config
  (add-to-list 'completion-styles 'flex)
  (setq read-buffer-completion-ignore-case t
		read-file-name-completion-ignore-case t
		completion-ignore-case t))

(use-package flymake
  :config
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error))

(use-package nebula
  :load-path "/home/pedro/.emacs.d/lisp"
  :commands (list-shamedb-report)
  :bind-keymap
  ("C-c n" . nebula-prefix-map))

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5))

(use-package jenkins
  :ensure t
  :config
  (setq jenkins-api-token "110cc93c7fec80c1593a051ae9eed8a449")
  (setq jenkins-url "http://enr-jenkins-00:9290/enrollment")
  (setq jenkins-username "pedro"))

(use-package vertico
  :init
  (vertico-mode)

  ;; Use `consult-completion-in-region' if Vertico is enabled.
  ;; Otherwise use the default `completion--in-region' function.
  (setq completion-in-region-function
		(lambda (&rest args)
          (apply (if vertico-mode
					 #'consult-completion-in-region
                   #'completion--in-region)
				 args))))

(use-package marginalia
  :init
  (marginalia-mode))

;; (use-package orderless
;;   :ensure t
;;   :custom
;;   (completion-styles '(orderless basic))
;;   (completion-category-overrides '((file (styles basic partial-completion)))))

;; (use-package corfu
;;   :init
;;   (global-corfu-mode))

(use-package grep
  :config
  (setq grep-find-ignored-directories
		(append
		 (list
		  "local"
		  "logs"
		  "node_modules"
		  "dist"
		  "target"
		  ".idea"
		  ".log"
		  "eva"
		  "r-eva"
		  "build"
		  ".project"
		  "social-web-static-resources"
		  ".settings")
		 grep-find-ignored-directories)))


(use-package compile
  :config
  (setq compilation-scroll-output 'first-error)
  (setq scroll-conservatively 101)
  (require 'notifications)
  (defvar compilation-mode-server-started-regex "Started @[0-9]+ms"
    "Regex a buscar en la salida de *compilation* que indica que el servidor está corriendo.")
  (defun pm/compilation-mode-server-started-filter ()
    "Se fija si el servidor arrancó."
    (when (string-match-p compilation-mode-server-started-regex
                          (buffer-substring compilation-filter-start
                                            (point)))
      (notifications-notify :title "STARTED" :body "El servidor está corriendo")
      ;; Fake mode line display as if `start-process' were run.
      (setq mode-line-process
			'((:propertize ":started" face compilation-mode-line-exit)
              compilation-mode-line-errors))))
  :bind
  ([f5] . recompile)
  :hook
  (compilation-filter-hook . pm/compilation-mode-server-started-filter))
;; (compilation-filter-hook . (lambda () (ansi-color-apply-on-region (point-min) (point-max))))))


(defun lista-de-educas ()  
  "Escribe la lista de educadores."
  (interactive)
  (insert "  - Clau comu
  - Juli
  - Mile
  - Agus
  - Cata
  - Clau exactas
  - Ernest
  - Eve
  - Ivan
  - Lau
  - Lili
  - Lucas
  - Manu
  - Lucre
  - Mateo
  - Pao
  - Pilar
  - Romi"))

(defun sketch-body-as-html ()  
  "Extrae el body del json de la respuesta y lo acomoda"
  (interactive)
  (search-forward "body")
  (forward-char 4)
  (kill-region (point-min) (point))
  (end-of-line)
  (delete-char -2)
  (kill-region (point) (point-max))
  (beginning-of-buffer)
  (replace-string "\\n" "\n")
  (beginning-of-buffer)
  (replace-string "\\\"" "\""))

(fset 'sketch-extract-email-html
	  (kmacro-lambda-form [?\C-s ?b ?o ?d ?y ?\C-f ?\C-f ?\C-f ?\C-  ?\C-a ?\M-< backspace ?\C-d ?\C-e backspace ?\C-  ?\M-> backspace ?\M-< ?\M-x ?r ?e ?p ?l ?a ?c ?e ?\C-n return ?\\ ?n return return ?\C-a ?\C-e backspace ?\C-a ?\M-x return ?| backspace ?\\ ?\" return ?\" return ?\C-a] 0 "%d"))


(defun my-tracing-function (orig-fun &rest args)
  (message "%s called with args %S" orig-fun args)
  (let ((res (apply orig-fun args)))
    (message "%s returned %S" orig-fun res)
    res))

(setq geiser-log-verbose t)
(setq geiser-log-verbose-debug t)

(require 'ef-themes)
(setq ef-themes-to-toggle '(ef-summer ef-winter))
(load-theme 'ef-summer :no-confirm)

(setq which-key-lighter nil)

(global-prettify-symbols-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(calendar-date-style 'european)
 '(calendar-mark-diary-entries-flag t)
 '(diary-show-holidays-flag nil)
 '(geiser-mode-autodoc-p t)
 '(package-selected-packages
   '(@ consult corfu deadgrep docker dockerfile-mode ef-themes eglot
	   geiser geiser-guile hyperbole jenkins magit marginalia
	   markdown-mode modus-themes orderless org-jira pdf-tools
	   restclient typescript-mode use-package vertico visual-regexp
	   vlf web-mode which-key yaml-mode yasnippet-snippets
	   zerodark-theme))
 '(safe-local-variable-values
   '((projectile-project-root . "/home/pedro/despegar/ken/ken-back/")
	 (projectile-project-run-cmd . "make run_backend")
	 (vc-prepare-patches-separately)
	 (diff-add-log-use-relative-names . t)
	 (vc-git-annotate-switches . "-w"))))

(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
