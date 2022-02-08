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

;; 设置垃圾回收缓存为1G,开启结束压缩,加速软件开启
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold 8000000000)
(add-hook 'emacs-startup-hook (lambda () "Restore defalut values after init."
				(setq file-name-handler-alist default-file-name-handler-alist)
				(setq gc-cons-threshold (* 1024 1024 1024))
				(add-hook 'focus-out-hook 'garbage-collect)))

;; emacs基础配置
(setq debug-on-error t)			;; 调试模式
(setq load-prefer-newer t)		;; 加载新的脚本
(setq-default blink-cursor-interval 0.4) ;;
(setq-default bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory))
(setq buffers-menu-max-size 30)
(setq case-fold-search t)
(setq column-number-mode t)
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq create-lockfiles nil)
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq mouse-yank-at-point t)
(setq save-interprogram-paste-before-kill t)
(setq scroll-preserve-screen-position 'always)
(setq set-mark-command-repeat-pop t)
(setq tooltip-delay 1.5)
(setq truncate-lines nil)
(setq truncate-partial-width-windows nil)
(setq backup-inhibited -1)                                        ;; 不产生备份

(add-to-list 'default-frame-alist '(fullscreen	. maximized)) ;; 设置全屏
(setq set-scroll-bar-mode nil)	;; 取消滚动栏
(setq tool-bar-mode 0)		;; 取消工具栏
(setq menu-bar-mode 0)		;; 禁用菜单栏，F10 开启关闭菜单
(setq gdb-many-windows t)	;; gdb多窗口模式
(setq global-hl-line-mode 1)		;; 高亮当前行
(add-hook 'before-save-hook 'whitespace-cleanup) ;; 保存时删除多余的空格个
(delete-selection-mode 1)			 ;; 选中删除
(global-auto-revert-mode t)	;; 自动载入文件
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
(setq time-stamp-active t)
(setq time-stamp-warn-inactive t)
(setq time-stamp-format "%:y-%02m-%02d %3a %02H:%02M:%02S mengdemao")
(setq mouse-avoidance-mode 'animate) ;; 鼠标自动避开指针
(setq blink-cursor-mode -1)	     ;; 指针停止闪动
(setq transient-mark-mode 1)         ;; 高亮显示要拷贝的内容
(setq show-paren-mode 1)	     ;; 当指针到一个括号时，自动显示所匹配的另一个括号
(setq mouse-wheel-mode t)	     ;; 是用滚轴鼠标
(setq track-eol t)		     ;; 当光标在行尾上下移动的时候，始终保持在行尾。
(setq transient-mark-mode t) ;;
(setq visible-bell t)		 ;;
(fset 'yes-or-no-p'y-or-n-p) ;;

(setq-default initial-major-mode 'fundamental-mode) ;; 启动为普通模式
(setq-default major-mode 'text-mode)		    ;; 设置为文本模式
(setq make-backup-files nil)			    ;;
(setq-default auto-save-mode t)				    ;;
(setq-default make-backup-files nil)		    ;;
(setq-default initial-scratch-message nil)	    ;;

(global-set-key [(meta ?/)] 'hippie-expand) ;; 绑定自动补全按键
(setq hippie-expand-try-functions-list      ;; 搜索路径
      '(try-expand-dabbrev try-expand-dabbrev-visible try-expand-dabbrev-all-buffers
			   try-expand-dabbrev-from-kill try-complete-file-name-partially
			   try-complete-file-name try-expand-all-abbrevs try-expand-list
			   try-expand-line try-complete-lisp-symbol-partially
			   try-complete-lisp-symbol))

(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(blink-cursor-mode -1) ;; 禁止光标闪烁

(setq-default
 window-resize-pixelwise t
 frame-resize-pixelwise t)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

(menu-bar-mode -1)

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

;; TAB按键的长度设置
(setq default-tab-width 4)  ;; 设置tab默认长度
(setq tab-width 4)	    ;; 设置tab默认长度
(setq indent-tabs-mode nil)   ;; 不使用空格替换tab
(cl-loop for x downfrom 40 to 1 do
	 (setq tab-stop-list (cons (* x 4) tab-stop-list)))
(setq backward-delete-char-untabify-method nil)		    ;; tab退格删除
(add-hook 'before-save-hook 'delete-trailing-whitespace)    ;; 关闭时自动删除多余空格

;; 编码
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Mouse & Smooth Scroll
;; Scroll one line at a time (less "jumpy" than defaults)
(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
	mouse-wheel-progressive-speed nil))
(setq scroll-step 1
      scroll-margin 0
      scroll-conservatively 100000)

;; Misc
(setq visible-bell t
      inhibit-compacting-font-caches t  ; Don’t compact font caches during GC.
      delete-by-moving-to-trash t       ; Deleting files go to OS's trash folder
      make-backup-files nil             ; Forbide to make backup files
      auto-save-default nil             ; Disable auto save

      uniquify-buffer-name-style 'post-forward-angle-brackets ; Show path if names are same
      adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*"
      adaptive-fill-first-line-regexp "^* *$"
      sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
      sentence-end-double-space nil)

(display-time-mode 1)							;; 时间显示
(setq display-time-24hr-format t)				;; 24小时制
(setq display-time-day-and-date t)				;; 显示具体的时间和日期
(setq display-time-format "%02H:%02M:%02S %Y-%02m-%02d %3a") ;; 设定时间格式
(setq display-time-interval 1)					;; 时间的变化频率
(display-time)									;; 显示时间

;; 设置行号
(require 'linum)
(global-display-line-numbers-mode nil)
(setq display-line-numbers "%4d \u2502")

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

;; 设置title
(setq frame-title-format
      (list "[" '(:eval (projectile-project-name)) "]" " @ "
	    '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; 如果有工程文件存在则设置工程,否则设置文件名字
(setq frame-title-format
      '((:eval (if (buffer-file-name)
		   (abbreviate-file-name (buffer-file-name))
		 "%b"))))

;;中文与外文字体设置
(defun set-font (english chinese english-size chinese-size)
  "Set fonts."
  (if (display-graphic-p)
      (progn
	(set-face-attribute 'default nil :font
			    (format   "%s:pixelsize=%d"  english english-size))
	(dolist (charset '(kana han symbol cjk-misc bopomofo))
	  (set-fontset-font (frame-parameter nil 'font) charset
			    (font-spec :family chinese :size chinese-size))))))
(set-font   "Source Code Pro" "YaHei Consolas Hybrid" 22 22)

(require 'font-lock-plus)           ;; 语法高亮文件
(global-font-lock-mode 1)           ;; 开启语法高亮

;; (require-package 'monokai-theme)
(require-package 'molokai-theme)
;; (require-package 'doom-themes)

(load-theme 'molokai t)
;; (load-theme 'monokai t)

;; 调整透明状态
(defun adjust-opacity (frame incr)
  "Adjust the background opacity of FRAME by increment INCR."
  (unless (display-graphic-p frame)
    (Error "Cannot Adjust opacity of this frame"))
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
	 ;; The 'alpha frame param became a pair at some point in
	 ;; emacs 24.x, e.g. (100 100)
	 (oldalpha (if (listp oldalpha) (car oldalpha) oldalpha))
	 (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))

(global-set-key (kbd "M-C-8") (lambda () (interactive) (adjust-opacity nil -2)))
(global-set-key (kbd "M-C-9") (lambda () (interactive) (adjust-opacity nil 2)))
(global-set-key (kbd "M-C-7") (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))

;; doxygen配置
(require 'doxymacs)
(add-hook 'c-mode-common-hook 'doxymacs-mode)
(add-hook 'c++-mode-common-hook 'doxymacs-mode)
(add-hook 'asm-mode-common-hook 'doxymacs-mode)

;; 编号
(require-package 'window-numbering)
(window-numbering-mode t)

;; 图标
(require-package 'all-the-icons)

;; neotree
(require-package 'neotree)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(global-set-key [f8] 'neotree-toggle)

;; 括号自动补全
(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))


;; 设置C/C++编程模式
;; avoid default "gnu" style, use more popular one
(setq c-default-style '((java-mode . "java")
			(awk-mode . "awk")
			(other . "linux")))

(defun fix-c-indent-offset-according-to-syntax-context (key val)
  ;; remove the old element
  (setq c-offsets-alist (delq (assoc key c-offsets-alist) c-offsets-alist))
  ;; new value
  (add-to-list 'c-offsets-alist '(key . val)))

(setq-default c-basic-offset 4)

(defun my-common-cc-mode-setup ()
  "setup shared by all languages (java/groovy/c++ ...)"
  ;; give me NO newline automatically after electric expressions are entered
  (setq c-auto-newline nil)

  ;; syntax-highlight aggressively
  ;; (setq font-lock-support-mode 'lazy-lock-mode)
  (setq lazy-lock-defer-contextually t)
  (setq lazy-lock-defer-time 0)

  ;make DEL take all previous whitespace with it
  (c-toggle-hungry-state 1)

  ;; indent
  ;; google "C/C++/Java code indentation in Emacs" for more advanced skills
  ;; C code:
  ;;   if(1) // press ENTER here, zero means no indentation
  (fix-c-indent-offset-according-to-syntax-context 'substatement 0)
  ;;   void fn() // press ENTER here, zero means no indentation
  (fix-c-indent-offset-according-to-syntax-context 'func-decl-cont 0))

(defun my-c-mode-setup ()
  "C/C++ only setup."
  ;; @see http://stackoverflow.com/questions/3509919/ \
  ;; emacs-c-opening-corresponding-header-file
  (local-set-key (kbd "C-x C-o") 'ff-find-other-file)

  (setq cc-search-directories '("." "/usr/include" "/usr/local/include/*" "../*/include" "$WXWIN/include"))

  ;; @see https://github.com/redguardtoo/cpputils-cmake
  ;; In theory, you can write your own Makefile for `flymake-mode' without cmake.
  ;; Nobody actually does it in real world.

  ;; debugging Emacs C code
  (push '(nil "^DEFUN *(\"\\([a-zA-Z0-9-]+\\)" 1) imenu-generic-expression )

  ;; make a #define be left-aligned
  (setq c-electric-pound-behavior (quote (alignleft))))

(defun cc-mode-common-hook-setup ()
  "C/C++ setup."
    (my-common-cc-mode-setup)
    (unless (or (derived-mode-p 'java-mode)
		(derived-mode-p 'groovy-mode))
      (my-c-mode-setup)))

(add-hook 'c-mode-common-hook 'cc-mode-common-hook-setup)
(add-hook 'c++-mode-common-hook 'cc-mode-common-hook-setup)

;; 触发ident
(add-hook 'c-mode-common-hook '(lambda () (c-toggle-auto-state 1)))
(add-hook 'c++-mode-common-hook '(lambda () (c-toggle-auto-state 1)))

(require 'init-org)
(require 'init-evil)
(require 'init-ivy)

(require 'init-latex)
(require 'init-vcs)
(require 'init-complete)
(require 'init-project)
(require 'init-misc)

(provide 'init)
;;; init.el ends here
