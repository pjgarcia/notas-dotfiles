;;; nebula.el --- Nebula  -*- lexical-binding: t; -*-

;;; Commentary:
;; hacer MAKEFILE en base a https://nullprogram.com/blog/2020/01/22/
 
;;; Code:

(require 'shamedb)

(defvar nebula-prefix-map
  (let ((map (make-sparse-keymap)))
	(define-key map "l" 'nebula-goto-logs)
    (define-key map "E" 'list-shamedb-report)
	(define-key map "t" 'nebula-goto-http-traffic)
	(define-key map "o" 'nebula-goto-outbound-http-traffic)
	(define-key map "r" 'nebula-goto-resources)
    map)
  "Keymap for Nebula commands.")

(defconst nebula--logs-url-template
  "http://monitoring-nebula.us-east-1.despegar.net/d/vTjL4QZGk2/logs?orgId=1&refresh=10s&var-datasource=Loki-Users&var-cluster=jupiter&var-environment=%s&var-application=%s&var-service=%s&var-deployment_group=All&var-instance=All&var-filterRegex=&var-filterMatch="
  "URL del Grafana de Nebula para ver los logs")

(defconst nebula--http-traffic-url-template
  "https://monitoring-nebula.us-east-1.despegar.net/d/http-traffic-services/services-http-traffic?orgId=1&refresh=1m&var-cluster=All&var-upstream_cluster=%s&var-environment=%s&var-deployment_group=All&var-operation=All&var-method=All&var-status=All&var-version=All&var-upstream_node=All&var-server=All&var-downstream_cluster=All"
  "URL del Grafana de Nebula para ver el trafico HTTP")

(defconst nebula--http-outbound-traffic-url-template
  "https://monitoring-nebula.us-east-1.despegar.net/d/wZInyxd4k/services-http-outbound-traffic?orgId=1&var-cluster=All&var-downstream_cluster=%s&var-environment=%s&var-version=All&var-deployment_group=All&var-downstream_node=All&var-method=All&var-status=All&var-upstream_workload=All&var-upstream_cluster=All&var-upstream_service=All"
  "URL del Grafana de Nebula para ver el trafico HTTP de salida")
  
(defconst nebula--resources-url-template
  "http://monitoring-nebula.us-east-1.despegar.net/d/6581e46e4e5c7ba40a07646395ef7b2a/application-resources?orgId=1&refresh=30s&var-cluster=jupiter&var-environment=%s&var-application=%s&var-serviceName=%s&var-version=All&var-pod=All&var-interval=4h&var-statusCode=All"
  "URL del Grafana de Nebula para ver los recursos de un servicio")

(defconst nebula--services-alist
  '(("sketch" "sketch")
    ("barbie" "barbie")
    ("caboom" "gamification")
    ("ken" "enrollment")
    ("lotso" "enrollment")
	("forky" "enrollment")
    ("social-login" "login-front")
    ("login-wapi" "login-front")
	("social-web" "profile")
	("wasapi" "profile")
	("wheezy" "subscription-front"))
  "Lista (asociativa) de los servicios y e informacion adicional.
Cada clave+valores esta formado por <nombre_servicio> <application>")

(defconst nebula--services
  (mapcar #'car nebula--services-alist)
  "List de los servicios en Nebula")

(defun nebula--current-project-name ()
  "Devuelve el nombre del proyecto actual"
  (let ((project (and (project-current) (project-root (project-current)))))
    (when project (file-name-base (directory-file-name project)))))

(defun nebula--prompt-for-service-env ()
  "Pide el ambiente del servicio actual, o al de una lista de servicios."
  (let ((service (or (nebula--current-project-name)
					 (completing-read "Service: " nebula--services)))
		(env (completing-read "Environment: " '("test" "sandbox" "prod"))))
	(list service env)))
  
(defun nebula-goto-logs (service env)
  "Ver logs de SERVICE."
  (interactive (nebula--prompt-for-service-env))
  (browse-url-button-open-url
   (format nebula--logs-url-template env (cadr (assoc service nebula--services-alist)) service)))

(defun nebula-goto-http-traffic (service env)
  "Ver el trafico HTTP del SERVICE (ambiente ENV) en Grafana."
  (interactive (nebula--prompt-for-service-env))
  (browse-url-button-open-url
   (format nebula--http-traffic-url-template service env)))

(defun nebula-goto-outbound-http-traffic (service env)
  "Ver el trafico HTTP de salida del SERVICE (ambiente ENV) en Grafana."
  (interactive (nebula--prompt-for-service-env))
  (browse-url-button-open-url
   (format nebula--http-outbound-traffic-url-template service env)))

(defun nebula-goto-resources (service env)
  "Ver el trafico HTTP de salida del SERVICE (ambiente ENV) en Grafana."
  (interactive (nebula--prompt-for-service-env))
  (browse-url-button-open-url
   (format nebula--resources-url-template
		   env
		   (cadr (assoc service nebula--services-alist))
		   service)))


(provide 'nebula)

;;; nebula.el ends here
