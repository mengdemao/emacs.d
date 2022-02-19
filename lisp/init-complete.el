;;; init-complete.el --- Completion with company -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package spinner
  :ensure t)

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

;; tags补全
(use-package company-ctags
  :ensure t)

(use-package lsp-mode
  :ensure t
  :custom
  (lsp-enable-snippet t)
  (lsp-keep-workspace-alive t)
  (lsp-enable-xref t)
  (lsp-enable-imenu t)
  (lsp-enable-completion-at-point nil)

  :config
  (add-hook 'go-mode-hook #'lsp)
  (add-hook 'python-mode-hook #'lsp)
  (add-hook 'c++-mode-hook #'lsp)
  (add-hook 'c-mode-hook #'lsp)
  (add-hook 'rust-mode-hook #'lsp)
  (add-hook 'html-mode-hook #'lsp)
  (add-hook 'js-mode-hook #'lsp)
  (add-hook 'typescript-mode-hook #'lsp)
  (add-hook 'json-mode-hook #'lsp)
  (add-hook 'yaml-mode-hook #'lsp)
  (add-hook 'dockerfile-mode-hook #'lsp)
  (add-hook 'shell-mode-hook #'lsp)
  (add-hook 'css-mode-hook #'lsp))

(use-package company-c-headers
  :ensure t
  :config
  ((add-to-list 'company-backends 'company-c-headers)))

(provide 'init-complete)
;;; init-complete.el ends here
