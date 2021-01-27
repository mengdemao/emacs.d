;;; init-basic.el --- Basic Configure for Doglock Emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'cl-lib)

;; 设置title
(setq frame-title-format (list "["
							   '(:eval (projectile-project-name))
							   "]" " @ " '(buffer-file-name "%f" (dired-directory dired-directory
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

(set-font   "Source Code Pro" "YaHei Consolas Hybrid" 20 18)

(setq backup-inhibited -1)                                        ;;不产生备份;

(setq-default
 blink-cursor-interval 0.4
 bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)
 buffers-menu-max-size 30
 case-fold-search t
 column-number-mode t
 delete-selection-mode t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 indent-tabs-mode nil
 create-lockfiles nil
 auto-save-default nil
 make-backup-files nil
 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 scroll-preserve-screen-position 'always
 set-mark-command-repeat-pop t
 tooltip-delay 1.5
 truncate-lines nil
 truncate-partial-width-windows nil)

;; 编码
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(display-time-mode 1)		   ;; 时间显示
(setq display-time-24hr-format t)  ;; 24小时制
(setq display-time-day-and-date t) ;; 显示具体的时间和日期
(setq display-time-format "%02H:%02M:%02S %Y-%02m-%02d %3a") ;; 设定时间格式
(setq display-time-interval 1) ;; 时间的变化频率
(display-time)             ;; 显示时间

(add-to-list 'default-frame-alist '(fullscreen	. maximized)) ;; 设置全屏
(set-scroll-bar-mode nil)	;; 取消滚动栏
(tool-bar-mode 0)		;; 取消工具栏
(menu-bar-mode 0)		;; 禁用菜单栏，F10 开启关闭菜单
(setq gdb-many-windows t)	;; gdb多窗口模式
(global-hl-line-mode 1)		;; 高亮当前行
(global-auto-revert-mode t)	;; 自动载入文件
;; (add-hook 'before-save-hook 'whitespace-cleanup) ;; 保存时删除多余的空格个
(delete-selection-mode 1)			 ;; 选中删除

(setq time-stamp-active t)
(setq time-stamp-warn-inactive t)
(setq time-stamp-format "%:y-%02m-%02d %3a %02H:%02M:%02S mengdemao")

;; TAB按键的长度设置
(setq c-default-style "linux")				;;
(setq default-tab-width 4)					;;
(setq tab-width 4)							;;
(setq indent-tabs-mode t)
(cl-loop for x downfrom 40 to 1 do
		 (setq tab-stop-list (cons (* x 4) tab-stop-list)))
(setq backward-delete-char-untabify-method nil)             ;;tab退格删除
;; (add-hook 'before-save-hook 'delete-trailing-whitespace) ;;关闭时自动删除多余空格

(require 'font-lock+)
(global-font-lock-mode 1)			 ;; 开启语法高亮
(require 'modern-cpp-font-lock)		 ;; cpp font-lock

(add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)
(add-hook 'c-mode-hook #'modern-c++-font-lock-mode)

(setq mouse-avoidance-mode 'animate) ;; 鼠标自动避开指针
(setq blink-cursor-mode -1)			 ;; 指针停止闪动
(setq transient-mark-mode 1)         ;; 高亮显示要拷贝的内容
(setq show-paren-mode 1)	 ;; 当指针到一个括号时，自动显示所匹配的另一个括号
(setq mouse-wheel-mode t)	 ;; 是用滚轴鼠标
(setq track-eol t)			 ;; 当光标在行尾上下移动的时候，始终保持在行尾。
(setq transient-mark-mode t) ;;
(setq visible-bell t)		 ;;
(fset 'yes-or-no-p'y-or-n-p) ;;
(setq-default initial-major-mode 'fundamental-mode) ;; 设计启动为普通模式
(setq make-backup-files nil)                ;;
(setq auto-save-mode nil)               ;;
(setq-default make-backup-files nil)            ;;
(setq-default initial-scratch-message nil)      ;;

(global-set-key [(meta ?/)] 'hippie-expand) ;; 绑定自动补全按键
(setq hippie-expand-try-functions-list      ;; 搜索路径
	  '(try-expand-dabbrev try-expand-dabbrev-visible try-expand-dabbrev-all-buffers
						   try-expand-dabbrev-from-kill try-complete-file-name-partially
						   try-complete-file-name try-expand-all-abbrevs try-expand-list
						   try-expand-line try-complete-lisp-symbol-partially
						   try-complete-lisp-symbol))

;; 设置行号
(require 'linum)
(global-display-line-numbers-mode nil)
(setq display-line-numbers "%4d \u2502")

;; 设置按键映射
(global-set-key [(f11)] 'delete-other-windows) ;;设置F11为删除其它窗口
(global-set-key [(meta return)] 'semantic-ia-complete-symbol-menu) ;;设置Alt+Enter为自动补全菜单
(global-set-key [C-.] 'cscope-find-global-definition) ;;搜索定义
(global-set-key [remap kill-buffer] #'kill-this-buffer)	;;C-c k 关闭 buffer 时不需要询问
(global-set-key "\C-x\C-n" 'next-buffer) ;; 下一个buffer
(global-set-key "\C-x\C-p" 'previous-buffer)
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\C-x\C-h" 'backward-kill-word)
(global-set-key "\C-m" 'newline-and-indent)
(global-set-key "\C-j" 'newline)
(global-set-key [(f9)] 'list-bookmarks)
(global-set-key [(f10)] 'bookmark-set)
(global-set-key (kbd "C->") 'other-window)	;;窗口之间的切换
(global-set-key [(meta p)] 'backward-paragraph)	;;上一个段落
(global-set-key [(meta n)] 'forward-paragraph)	;;下一个段落

;; History
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

(use-package recentf
  :ensure nil
  :bind (("C-x C-r" . recentf-open-files))
  :hook (after-init . recentf-mode)
  :init (setq recentf-max-saved-items 300
			  recentf-exclude
			  '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
				"\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
				"\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
				"^/tmp/" "^/var/folders/.+$" ; "^/ssh:"
				(lambda (file) (file-in-directory-p file package-user-dir))))
  :config (push (expand-file-name recentf-save-file) recentf-exclude))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :init (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
			  history-length 1000
			  savehist-additional-variables '(mark-ring
											  global-mark-ring
											  search-ring
											  regexp-search-ring
											  extended-command-history)
			  savehist-autosave-interval 300))

(use-package simple
  :ensure nil
  :hook ((after-init . size-indication-mode)
		 (text-mode . visual-line-mode)
		 ((prog-mode markdown-mode conf-mode) . enable-trailing-whitespace))
  :init
  (setq column-number-mode t
		line-number-mode t
		;; kill-whole-line t               ; Kill line including '\n'
		line-move-visual nil
		track-eol t                     ; Keep cursor at end of lines. Require line-move-visual is nil.
		set-mark-command-repeat-pop t)  ; Repeating C-SPC after popping mark pops it again

  (defun enable-trailing-whitespace ()
	"Show trailing spaces and delete on saving."
	;; (setq show-trailing-whitespace t)
	;; (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)
	))

;; Mouse & Smooth Scroll
;; Scroll one line at a time (less "jumpy" than defaults)
(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
		mouse-wheel-progressive-speed nil))
(setq scroll-step 1
	  scroll-margin 0
	  scroll-conservatively 100000)

;; Misc
(setq-default major-mode 'text-mode
			  fill-column 80
			  tab-width 4
			  indent-tabs-mode t)

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

;; 高亮括号
(show-paren-mode 1)
(require 'highlight-parentheses)
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
	(highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

(add-hook 'after-init-hook 'global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
	  auto-revert-verbose nil)
(with-eval-after-load 'autorevert
  (diminish 'auto-revert-mode))

(require 'undo-tree)
(global-undo-tree-mode)
(defadvice undo-tree-visualizer-mode (after undo-tree-face activate)
  (buffer-face-mode))

(require 'tramp)

(use-package smartparens
  :ensure t)

(require 'bug-hunter)

(require 'ws-butler)
(add-hook 'prog-mode-hook #'ws-butler-mode)

(provide 'init-basic)
;;; init-basic.el ends here
