;;; init-evil.el --- evil key-binding -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package evil)
(evil-mode 1)

;; 设置打开时默认emacs模式
(setq evil-default-state 'emacs)

;; 设置临时执行的evil模式
(define-key evil-emacs-state-map (kbd "C-o") 'evil-execute-in-normal-state)

(provide 'init-evil)
;;; init-evil.el ends here
