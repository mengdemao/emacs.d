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

;; (require 'font-lock-plus)           ;; 语法高亮文件
;; (global-font-lock-mode 1)           ;; 开启语法高亮

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)  
  (setq doom-theme 'doom-gruvbox)
  (setq doom-themes-treemacs-theme "doom-gruvbox")
  (doom-themes-treemacs-config)
  (doom-themes-org-config))
(load-theme 'doom-gruvbox t)

;; 编号
(use-package window-numbering)
(window-numbering-mode t)

;; 图标
(use-package all-the-icons)

;; neotree
(use-package neotree)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(global-set-key [f8] 'neotree-toggle)

(use-package smart-mode-line 
    :init 
    (setq sml/no-confirm-load-theme t) 
    (setq sml/theme 'respectful) 
    (sml/setup))

(defun dashboard-banner ()
  (setq dashboard-banner-logo-title
        (format "Emacs ready in %.2f seconds with %d garbage collections."
                (float-time (time-subtract after-init-time before-init-time)) gcs-done)))

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

(provide 'init-ui)