;;; init-theme.el --- Settings Color themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(add-to-list 'custom-theme-load-path (expand-file-name "themes/" user-emacs-directory))

(setq custom-safe-themes t)

;; doom theme enable
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  (if (display-graphic-p)
      (progn
        ;; Enable custom neotree theme (all-the-icons must be installed!)
        (doom-themes-neotree-config)
        ;; or for treemacs users
        (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
        (doom-themes-treemacs-config)))
  (doom-themes-org-config))
(load-theme 'doom-monokai-classic)

;; modeline
(use-package doom-modeline
   :ensure t
   :hook (after-init . doom-modeline-mode))

 (set-face-background 'mode-line nil)

(provide 'init-themes)
;; init-elpa.el ends here
