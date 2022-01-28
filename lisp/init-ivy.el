(use-package ivy
  :ensure t
  :diminish ivy-mode
  :hook (after-init . ivy-mode))

(use-package counsel
  :ensure t
  :diminish counsel-mode
  :hook (after-init . counsel-mode))

(provide 'init-ivy)
;;; init-ivy.el ends here
