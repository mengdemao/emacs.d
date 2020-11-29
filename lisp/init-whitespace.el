;;; init-whitespace.el --- Special handling for whitespace -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq-default show-trailing-whitespace nil)

;;; Whitespace

(defun show-trailing-whitespace ()
  "Enable display of trailing whitespace in this buffer."
  (setq-local show-trailing-whitespace t))

(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook 'show-trailing-whitespace))

(require 'whitespace-cleanup-mode)
(add-hook 'after-init-hook 'global-whitespace-cleanup-mode)
(with-eval-after-load 'whitespace-cleanup-mode
  (diminish 'whitespace-cleanup-mode))

(global-set-key [remap just-one-space] 'cycle-spacing)


(provide 'init-whitespace)
;;; init-whitespace.el ends here
