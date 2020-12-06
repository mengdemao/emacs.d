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

(provide 'init-flycheck)
