;; init.el --- Where all config file start -*- lexical-binding: t -*-
;;; Commentary:

;; Copyright (C) 2022 Meng Demao

;; Author: Meng Demao mengdemao19951021@163.com

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;; 加载emacs自带配置文件
(require 'cl-lib)
(require 'package)

(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

;; 新建
(setq *win* (eq system-type 'windows-nt))
(setq *cygwin* (eq system-type 'cygwin) )
(setq *linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)) )
(setq *unix* (or *linux* (eq system-type 'usg-unix-v) (eq system-type 'berkeley-unix)) )

;; 加载custom.el
;; 用户可以在此文件中加载自设定配置
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
(unless package-archive-contents
  (package-refresh-contents))

;; 设置垃圾回收缓存为1G,开启结束压缩,加速软件开启
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold 8000000000)
(add-hook 'emacs-startup-hook (lambda () "Restore defalut values after init."
				(setq file-name-handler-alist default-file-name-handler-alist)
				(setq gc-cons-threshold (* 1024 1024 1024))
				(add-hook 'focus-out-hook 'garbage-collect)))

;; 添加自动插件
(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
   If NO-REFRESH is non-nil, the available package lists will not be
   re-downloaded in order to locate PACKAGE."
  (or (package-installed-p package min-version)
      (let* ((known (cdr (assoc package package-archive-contents)))
	     (best (car (sort known (lambda (a b)
				      (version-list-<= (package-desc-version b)
						       (package-desc-version a)))))))
	(if (and best (version-list-<= min-version (package-desc-version best)))
	    (package-install best)
	  (if no-refresh
	      (error "No version of %s >= %S is available" package min-version)
	    (package-refresh-contents)
	    (require-package package min-version t)))
	(package-installed-p package min-version))))

;; auto-compile
(require-package 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)
(setq auto-compile-display-buffer nil)
(setq auto-compile-mode-line-counter t)

;; Should set before loading `use-package'
;; 安装包管理器
(eval-when-compile
  (require-package 'use-package))

(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t)
  (setq use-package-verbose t))

;; Load path
;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("site" "lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory (expand-file-name "site" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)
(update-load-path)

;; 自动更新配置文件
(defun config-update()
  "Autoupdate local git repository .emacs.d."
  (interactive)
  (compilation-start "cd ~/.emacs.d && git pull"))
(define-key global-map (kbd "<f12>") 'config-update)

(require 'init-basic)
(require 'init-edit)
(require 'init-ui)
(require 'init-cc)
(require 'init-latex)
(require 'init-org)
(require 'init-evil)
(require 'init-ivy)
(require 'init-vcs)

;; (require 'init-complete)
(require 'init-project)
(require 'init-misc)

(provide 'init)
;;; init.el ends here
