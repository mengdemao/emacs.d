;;; init-edit.el --- Day-to-day editing helpers -*- lexical-binding: t -*-
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

(provide 'init-cpp)
