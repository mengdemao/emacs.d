;;; init-company.el --- Completion with company -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; Snippets
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :init (yas-global-mode t))

;; Autocomplete
(use-package company
  :defer 10
  :diminish company-mode
  :bind (:map company-active-map
			  ("M-j" . company-select-next)
			  ("M-k" . company-select-previous))
  :preface
  ;; enable yasnippet everywhere
  (defvar company-mode/enable-yas t
	"Enable yasnippet for all backends.")
  (defun company-mode/backend-with-yas (backend)
	(if (or
		 (not company-mode/enable-yas)
		 (and (listp backend) (member 'company-yasnippet backend)))
		backend
	  (append (if (consp backend) backend (list backend))
			  '(:with company-yasnippet))))

  :init (global-company-mode t)
  :config
  ;; no delay no autocomplete
  (setq
   company-idle-delay 0
   company-minimum-prefix-length 2
   company-tooltip-limit 20)

  (setq company-backends
		(mapcar #'company-mode/backend-with-yas company-backends)))

;; Code-comprehension server
(use-package ycmd
  :ensure t
  :init (add-hook 'c++-mode-hook #'ycmd-mode)
  :config
  (set-variable 'ycmd-server-command '("python3" "/opt/ycmd/ycmd"))
  (set-variable 'ycmd-global-config (expand-file-name "~/.ycm_conf.py"))

  (set-variable 'ycmd-extra-conf-whitelist '("~/Repos/*"))

  (use-package company-ycmd
	:ensure t
	:init (company-ycmd-setup)
	:config (add-to-list 'company-backends (company-mode/backend-with-yas 'company-ycmd))))

;; On-the-fly syntax checking
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :init (global-flycheck-mode t))

(use-package flycheck-ycmd
  :commands (flycheck-ycmd-setup)
  :init (add-hook 'ycmd-mode-hook 'flycheck-ycmd-setup))

;; Show argument list in echo area
(use-package eldoc
  :diminish eldoc-mode
  :init (add-hook 'ycmd-mode-hook 'ycmd-eldoc-setup))

(provide 'init-company)
;;; init-company.el ends here
