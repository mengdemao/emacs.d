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
(use-package linum
  :ensure nil
  :config 
  (global-display-line-numbers-mode nil)
  (setq display-line-numbers "%4d \u2502"))

(provide 'init-basic)
