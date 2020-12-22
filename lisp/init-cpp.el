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
	:ensure t
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
  (interactive)
  (c-set-style "K&R")
  (c-toggle-hungry-state)
  (setq c-basic-offset 4))

(add-hook 'c-mode-hook 'hs-minor-mode)
(add-hook 'c++-mode-hook 'hs-minor-mode)

(electric-pair-mode 1)
(defun my-c-mode-auto-pair ()
  (interactive)
  (make-local-variable 'skeleton-pair-alist)
  (setq skeleton-pair-alist  '(
							   (?\' _ "'")
							   (?\" _ "\"")
							  (?\( _ ")")
							   (?\[ _ "]")
							   (?{ \n > _ \n ?} >)))
  (setq skeleton-pair t)
  (local-set-key (kbd "(") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "{") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "'") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "[") 'skeleton-pair-insert-maybe))
(add-hook 'c-mode-hook 'my-c-mode-auto-pair)
(add-hook 'c++-mode-hook 'my-c-mode-auto-pair)

(require 'ggtags)
(add-hook 'c-mode-common-hook
		  (lambda ()
			(when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
			  (ggtags-mode 1))))
(global-set-key (kbd "M-[") 'ggtags-find-definition)

(require 'doxymacs)
(add-hook 'c-mode-common-hook 'doxymacs-mode)
(add-hook 'c++-mode-common-hook 'doxymacs-mode)
(add-hook 'asm-mode-common-hook 'doxymacs-mode)

(provide 'init-cpp)
;;; init-cpp.el ends here
