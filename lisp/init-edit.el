
(use-package which-key 
  :defer nil 
  :config (which-key-mode))

;; 括号自动补全
(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

(provide 'init-edit)