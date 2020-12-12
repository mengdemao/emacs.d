;;; init-project.el --- Projetc Management -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; projectile配置
(require 'projectile)

;; cmake配置
(require 'cmake-mode)
(setq auto-mode-alist
	  (append '(("CMakeLists\\.txt\\'" . cmake-mode)
				("\\.cmake\\'" . cmake-mode))
			  auto-mode-alist))

;; 编译快捷键

(provide 'init-project)
;;; init-project ends here
