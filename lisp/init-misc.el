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

;; editorconfig
(use-package editorconfig
  :ensure t)

;; Makrdown Mode
(use-package markdown-mode
  :ensure
  :config
  (define-key markdown-mode-map (kbd "C-c C-c") 'markdown-preview-mode))
(use-package markdown-preview-mode
  :ensure t
  :defer t)

(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "sbcl"))

(use-package geiser
  :ensure t)

(use-package avy
  :ensure t
  :bind (("M-1" . avy-goto-char)
		 ("M-2" . avy-goto-char-2)
		 ("M-l" . avy-goto-line)))

;; docker file
(use-package dockerfile-mode
  :ensure t)

(provide 'init-misc)
;;; init-misc.el ends here
