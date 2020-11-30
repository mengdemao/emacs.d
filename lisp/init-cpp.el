;;; init-cpp.el --- Day-to-day editing helpers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package cc-mode
  :ensure t
  :bind (:map c-mode-base-map
	 ("C-c c" . compile))
  :hook (c-mode-common . (lambda () (c-set-style "gnu")))
  :init (setq-default c-basic-offset 4)
  :config
  (use-package modern-cpp-font-lock
	:diminish
	:init (modern-c++-font-lock-global-mode t)))

;; 配置 C&c++ mode
;; ------------------------------------------------------------------------------
(add-hook 'c-mode-hook 'linux-c-mode)
(add-hook 'c++-mode-hook 'linux-cpp-mode)
(defun linux-c-mode()
	(interactive)
	(c-set-style "K&R")
	(c-toggle-hungry-state)
	(setq c-basic-offset 4)
	)
(defun linux-cpp-mode()
  (define-key c++-mode-map [return] 'newline-and-indent)
 (define-key c++-mode-map [(control c) (c)] 'compile)
  (interactive)
  (c-set-style "K&R")
  (c-toggle-auto-state)
  (c-toggle-hungry-state)
  (setq c-basic-offset 4)
  (imenu-add-menubar-index)
  (which-function-mode))

(require 'ggtags)
(add-hook 'c-mode-common-hook
		  (lambda ()
			(when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
			  (ggtags-mode 1))))

(provide 'init-cpp)
