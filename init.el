;;; init.el --- Where all config file start -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Copyright (C) 2020 Meng Demao

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

(require 'cl-lib)

(setq package-archives
	  '(
		("gnu" . "http://mirrors.ustc.edu.cn/elpa/gnu/")
		("melpa" . "http://mirrors.ustc.edu.cn/elpa/melpa/")
		("melpa-stable" . "http://mirrors.ustc.edu.cn/elpa/melpa-stable/")
		("org" . "http://mirrors.ustc.edu.cn/elpa/org/")))

(defun add-subdirs-to-load-path (parent-dir)
  "Add every non-hidden subdir of PARENT-DIR to `load-path'."
  (let ((default-directory parent-dir))
	(setq load-path
		  (append
		   (cl-remove-if-not
			#'file-directory-p
			(directory-files (expand-file-name parent-dir) t "^[^\\.]"))
		   load-path))))

(let ((lisp-dir (expand-file-name "lisp/" user-emacs-directory)))
  (push lisp-dir load-path)
  (add-subdirs-to-load-path lisp-dir))

(let ((site-dir (expand-file-name "site/" user-emacs-directory)))
  (push site-dir load-path)
  (add-subdirs-to-load-path site-dir))

(let ((user-dir (expand-file-name "user/" user-emacs-directory)))
  (push user-dir load-path)
  (add-subdirs-to-load-path user-dir))

(require 'packed)
(require 'bind-key)
(require 'diminish)

;; 设置调试模式
(setq debug-on-error t)

;;设置垃圾回收缓存为1G,开启结束压缩,加速软件开启
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold 8000000000)
(add-hook 'emacs-startup-hook (lambda () "Restore defalut values after init."
				(setq file-name-handler-alist default-file-name-handler-alist)
				(setq gc-cons-threshold (* 1024 1024 1024))
				(add-hook 'focus-out-hook 'garbage-collect)))

;; 减少污染文件
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; 自动更新配置文件
(defun doglock-update()
  "Autoupdate local git repository .emacs.d."
  (interactive)
  (compilation-start "cd ~/.emacs.d && git pull"))
(define-key global-map (kbd "<f12>") 'doglock-update)

;; 基础配置
(require 'init-basic)
(require 'init-graphic)
(require 'init-themes)
(require 'init-edit)
(require 'init-cc)

;; 增强配置
(require 'init-helm)
(require 'init-org)
;; (require 'init-evil)

;; 扩展配置
(require 'init-vcs)
(require 'init-term)
(require 'init-complete)
(require 'init-project)
(require 'init-misc)
(require 'init-input)

(provide 'init)
;;; init.el ends here
