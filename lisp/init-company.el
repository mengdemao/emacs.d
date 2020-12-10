;;; init-company.el --- Completion with company -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

;; 补全前端
(use-package company
  :ensure t
  :defer 10
  :diminish company-mode
  :bind (:map company-active-map
			  ("M-j" . company-select-next)
			  ("M-k" . company-select-previous))

  :config
  (setq
   company-idle-delay 0
   company-minimum-prefix-length 2
   company-tooltip-limit 20))

;; ycmd补全
(use-package company-ycmd
  :ensure t
  :init (company-ycmd-setup)
  :config (add-to-list 'company-backends 'company-ycmd))

;; LSP服务器
(use-package company-lsp
  :ensure t
  :config
  (add-to-list 'company-backends 'company-lsp))

;; Irony 补全服务器
(use-package company-irony
  :ensure t
  :config
  (add-to-list 'company-backends 'company-irony))

;; Tabnine补全
(use-package company-tabnine
  :ensure t)

;; 头文件补全
(use-package company-c-headers
  :ensure t
  :config
  ((add-to-list 'company-backends 'company-c-headers)))

(provide 'init-company)
;;; init-company.el ends here
