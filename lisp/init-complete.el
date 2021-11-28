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

;; tags补全
(use-package company-ctags
  :ensure t)

;; Clang Tool
(use-package flycheck-clang-analyzer
  :ensure t)

(use-package flycheck-clang-tidy
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
  (add-hook 'css-mode-hook #'lsp)

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "pyls")
					:major-modes '(python-mode)
					:server-id 'pyls))
  (setq company-minimum-prefix-length 1
		company-idle-delay 0.500) ;; default is 0.2
  (require 'lsp-clients)
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :custom-face
  (lsp-ui-doc-background ((t (:background ni))))
  :init (setq lsp-ui-doc-enable t
			  lsp-ui-doc-include-signature t

			  lsp-enable-snippet nil
			  lsp-ui-sideline-enable nil
			  lsp-ui-peek-enable nil

			  lsp-ui-doc-position              'at-point
			  lsp-ui-doc-header                nil
			  lsp-ui-doc-border                "white"
			  lsp-ui-doc-include-signature     t
			  lsp-ui-sideline-update-mode      'point
			  lsp-ui-sideline-delay            1
			  lsp-ui-sideline-ignore-duplicate t
			  lsp-ui-peek-always-show          t
			  lsp-ui-flycheck-enable           nil
			  )
  :bind (:map lsp-ui-mode-map
			  ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
			  ([remap xref-find-references] . lsp-ui-peek-find-references)
			  ("C-c u" . lsp-ui-imenu))
  :config
  (setq lsp-ui-sideline-ignore-duplicate t)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(setq lsp-prefer-capf t)

;; 头文件补全
;; (use-package company-c-headers
;;  :ensure t
;;  :config
;;  ((add-to-list 'company-backends 'company-c-headers)))

(provide 'init-complete)
;;; init-complete.el ends here
