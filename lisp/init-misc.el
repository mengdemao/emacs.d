;;; init-misc.el --- misc setting -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode))

(use-package dts-mode
  :ensure t)

(use-package lua-mode
  :ensure t)

(require 'kconfig-mode)

;; cmake配置
(use-package cmake-mode
  :ensure t
  :config
  (setq auto-mode-alist
		(append '(("CMakeLists\\.txt\\'" . cmake-mode)
				  ("\\.cmake\\'" . cmake-mode))
				auto-mode-alist)))

(provide 'init-misc)
;;; init-misc.el ends here
