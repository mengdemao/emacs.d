;;; init-edit.el --- Day-to-day editing helpers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'smartparens-config)
(add-hook 'c-mode-hook #'smartparens-mode)
(add-hook 'c++-mode-hook #'smartparens-mode)
(add-hook 'asm-mode-hook #'smartparens-mode)

;; TODO FIXME
(require 'hl-todo)
(setq hl-todo-keyword-faces
	  '(("TODO"   . "#FF0000")
		("FIXME"  . "#FF0000")
		("DEBUG"  . "#A020F0")
		("GOTCHA" . "#FF4500")
		("STUB"   . "#1E90FF")))
;; (define-key hl-todo-mode-map (kbd "C-c p") 'hl-todo-previous)
;; (define-key hl-todo-mode-map (kbd "C-c n") 'hl-todo-next)
;; (define-key hl-todo-mode-map (kbd "C-c o") 'hl-todo-occur)
;; (define-key hl-todo-mode-map (kbd "C-c i") 'hl-todo-insert)

(provide 'init-edit)
;;; init-edit.el ends here
