;;; init-input.el --- Input Method -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package rime
  :ensure t
  :custom
  (default-input-method "rime")
  :config
  (setq rime-posframe-properties
		(list :background-color "#333333"
			  :foreground-color "#dcdccc"
			  :font "YaHei Consolas Hybrid-14"
			  :internal-border-width 10))

  (setq default-input-method "rime"
		rime-show-candidate 'posframe))

(provide 'init-input)
;;; init-input.el ends here
