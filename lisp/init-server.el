;; YCMD服务器
(use-package ycmd
  :ensure t
  :init (add-hook 'c++-mode-hook #'ycmd-mode)
  :config
  (set-variable 'ycmd-server-command '("python3" "/opt/ycmd/ycmd"))
  (set-variable 'ycmd-global-config (expand-file-name "~/.ycm_conf.py"))
  (set-variable 'ycmd-extra-conf-whitelist '("~/Repos/*")))

;; irony-mode
(use-package irony
  :ensure t
  :config
  (progn
	(add-hook 'c++-mode-hook 'irony-mode)
	(add-hook 'c-mode-hook 'irony-mode)))

;; rtags 补全
(use-package rtags
  :ensure t)

;; lsp补全服务器
(use-package ccls
  :ensure t
  :init
  (setq ccls-executable "ccls"))

(provide 'init-server)
