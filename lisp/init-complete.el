;;; init-complete.el --- Completion with company -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

;; 补全前端
(use-package company
  :ensure t
  :defer 10
  :diminish company-mode
  :init (add-hook 'after-init-hook'global-company-mode)
  :bind (:map company-active-map
			  ("M-j" . company-select-next)
			  ("M-k" . company-select-previous)))

(use-package company-posframe
  :ensure t
  :init
  (company-posframe-mode 1))

(use-package desktop
  :ensure t
  :config
  (push '(company-posframe-mode . nil)
		desktop-minor-mode-table))

;; 检查前端
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :init (global-flycheck-mode t)
  :config
  (setq
   company-idle-delay 0
   company-minimum-prefix-length 2
   company-tooltip-limit 20))

;; 文档前端
(use-package eldoc
  :diminish eldoc-mode
  :config (global-eldoc-mode 1))

;; 补全后端

;; tags补全
(use-package company-ctags
  :ensure t)

;; ycmd
(use-package ycmd
  :ensure t
  :init
  (progn
	(add-hook 'c++-mode-hook #'ycmd-mode)
	(add-hook 'c-mode-hook #'ycmd-mode))
  :config
  (set-variable 'ycmd-server-command '("python3" "/opt/ycmd/ycmd"))
  (set-variable 'ycmd-global-config (expand-file-name "~/.ycm_conf.py"))
  (set-variable 'ycmd-extra-conf-whitelist '("~/Repos/*")))

;; ycmd补全
(use-package company-ycmd
  :ensure t
  :init (company-ycmd-setup)
  :config (add-to-list 'company-backends 'company-ycmd))

(use-package flycheck-ycmd
  :commands (flycheck-ycmd-setup)
  :init (add-hook 'ycmd-mode-hook 'flycheck-ycmd-setup))

;; rtags
(use-package rtags
  :ensure t)

(use-package company-rtags
  :ensure t)

(use-package flycheck-rtags
  :ensure t)

;; irony
(use-package irony
  :ensure t
  :config
  (progn
	(add-hook 'c++-mode-hook 'irony-mode)
	(add-hook 'c-mode-hook 'irony-mode)))

(use-package company-irony
  :ensure t
  :config
  (add-to-list 'company-backends 'company-irony))

(use-package flycheck-irony
  :ensure t
  :config
  (add-to-list 'flycheck-backends 'flycheck-irony))

;; lsp
(use-package ccls
  :ensure t
  :init
  (setq ccls-executable "ccls"))

(use-package company-lsp
  :ensure t
  :config
  (add-to-list 'company-backends 'company-lsp))

;; Clang Tool
(use-package flycheck-clang-analyzer
  :ensure t)

(use-package flycheck-clang-tidy
  :ensure t)

;; 头文件补全
(use-package company-c-headers
  :ensure t
  :config
  ((add-to-list 'company-backends 'company-c-headers)))

(provide 'init-complete)
;;; init-complete.el ends here
