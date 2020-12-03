;;; init-flycheck.el --- Configure Flycheck global behaviour -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package flycheck-color-mode-line
  :ensure t)

(when (require 'flycheck)
  (add-hook 'after-init-hook 'global-flycheck-mode)
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)

  (when (require 'flycheck-color-mode-line)
	(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)))

(setq flycheck-emacs-lisp-load-path 'inherit)

(provide 'init-flycheck)
;;; init-flycheck.el ends here
