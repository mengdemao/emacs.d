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
;; cmake-project
(use-package cmake-project
  :ensure t)

(defun maybe-cmake-project-mode ()
  (if (or (file-exists-p "CMakeLists.txt")
		  (file-exists-p (expand-file-name "CMakeLists.txt" (car (project-roots (project-current))))))
	  (cmake-project-mode)))

(add-hook 'c-mode-hook 'maybe-cmake-project-mode)
(add-hook 'c++-mode-hook 'maybe-cmake-project-mode)

;; 编译快捷键

(provide 'init-project)
;;; init-project.el ends here
