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

;; emacs版本号判断
;; (when (or (< emacs-major-version 26)
;;	  (and (= emacs-major-version 26)
;;		   (< emacs-minor-version 1)))
;;   (x-popup-dialog t `(,(format "Sorry, you need GNU Emacs version 26.6 or higher
;; to run Dema Emacs.

;; Your installed Emacs reports:
;; %s" (emacs-version))
;;			  ("OK :(" . t)))
;;   (save-buffers-kill-emacs t))

;; 版本号

(setq-default MAJOR_VERSION "1")
(setq-default MINOR_VERSION "3")
(setq-default PATCH_VERSION "2")
(setq-default TWAEK_VERSION "2")

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "user" user-emacs-directory))

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
(defun auto-update()
  "Autoupdate local git repository .emacs.d."
  (interactive)
  (compilation-start "cd ~/.emacs.d && git pull"))
(define-key global-map (kbd "<f12>") 'auto-update)

;; 插件包配置
(require 'init-const)
(require 'init-package)
(require 'init-benchmark)

;; 基础配置
(require 'init-basic)
(require 'init-graphic)
(require 'init-themes)
(require 'init-compile)
(require 'init-cpp)

;; 增强配置
(require 'init-helm)
(require 'init-org)
(require 'init-evil)

;; 扩展配置
(require 'init-vcs)
(require 'init-term)
(require 'init-project)
(require 'init-complete)
(require 'init-misc)

;; server模式启动
(add-hook 'after-init-hook
	  (lambda ()
		(require 'server)
		(unless (server-running-p)
		  (server-start))))

(provide 'init)
;;; init.el ends here
