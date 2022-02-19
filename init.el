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

;; 计算启动时间
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; 设置垃圾回收缓存为1G,开启结束压缩,加速软件开启
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold most-positive-fixnum)

;; 加载自定义文件
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; 系统信息
(setq *windows* (eq system-type 'windows-nt))
(setq *cygwin* (eq system-type 'cygwin) )
(setq *linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)) )
(setq *unix* (or *linux* (eq system-type 'usg-unix-v) (eq system-type 'berkeley-unix)) )

;; 配置插件管理
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

(unless package-archive-contents
  (package-refresh-contents))

(when (not (package-installed-p 'use-package))
  (package-install 'use-package))

(require 'use-package)

(custom-set-variables '(use-package-always-ensure t))

(custom-set-variables '(use-package-always-defer t))

(custom-set-variables '(use-package-verbose nil))

(custom-set-variables '(load-prefer-newer t))

(use-package auto-compile
  :defer nil
  :config (auto-compile-on-load-mode))

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
(require 'init-complete)
(require 'init-project)
(require 'init-misc)

(provide 'init)
;;; init.el ends here
