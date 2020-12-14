;;; init-project.el --- Projetc Management -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; projectile配置
(use-package projectile
  :ensure t)

;; cmake配置
(use-package cmake-mode
  :ensure t
  :config
  (setq auto-mode-alist
		(append '(("CMakeLists\\.txt\\'" . cmake-mode)
				  ("\\.cmake\\'" . cmake-mode))
				auto-mode-alist)))

;; 编译快捷键

(provide 'init-project)
;;; init-project.el ends here
