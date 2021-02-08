;;; init-graphic.el --- Behaviour specific to non-TTY frames -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(blink-cursor-mode -1) ;; 禁止光标闪烁

(setq-default
 window-resize-pixelwise t
 frame-resize-pixelwise t)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

(menu-bar-mode -1)

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

(defun adjust-opacity (frame incr)
  "Adjust the background opacity of FRAME by increment INCR."
  (unless (display-graphic-p frame)
	(error "Cannot adjust opacity of this frame"))
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
	 ;; The 'alpha frame param became a pair at some point in
	 ;; emacs 24.x, e.g. (100 100)
	 (oldalpha (if (listp oldalpha) (car oldalpha) oldalpha))
	 (newalpha (+ incr oldalpha)))
	(when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
	  (modify-frame-parameters frame (list (cons 'alpha newalpha))))))

;; TODO: use seethru package instead?
(global-set-key (kbd "M-C-8") (lambda () (interactive) (adjust-opacity nil -2)))
(global-set-key (kbd "M-C-9") (lambda () (interactive) (adjust-opacity nil 2)))
(global-set-key (kbd "M-C-7") (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))

(setq frame-title-format
	  '((:eval (if (buffer-file-name)
		   (abbreviate-file-name (buffer-file-name))
		 "%b"))))

;; Non-zero values for `line-spacing' can mess up ansi-term and co,
;; so we zero it explicitly in those cases.
(add-hook 'term-mode-hook
	  (lambda ()
		(setq line-spacing 0)))

;; Change global font size easily

(require 'default-text-scale)
(add-hook 'after-init-hook 'default-text-scale-mode)

(require 'disable-mouse)
(global-disable-mouse-mode)

(require 'window-numbering)
(window-numbering-mode t)

;; (require 'all-the-icons)

;; neotree
(use-package neotree
  :ensure t
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))
(global-set-key [f8] 'neotree-toggle)

(provide 'init-graphic)
;;; init-graphic.el ends here
