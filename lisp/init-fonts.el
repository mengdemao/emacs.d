;;; init-compile.el --- Helpers for M-x compile -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun doglock-font-existsp (font)
  (if (null (x-list-fonts font))
	  nil t))

(defun doglock-make-font-string (font-name font-size)
  (if (and (stringp font-size)
		   (equal ":" (string (elt font-size 0))))
	  (format "%s%s" font-name font-size)
	(format "%s-%s" font-name font-size)))

(defvar doglock-english-font-size nil)
(defun doglock-set-font (english-fonts
					   english-font-size
					   chinese-fonts
					   &optional chinese-fonts-scale
					   )
  (setq chinese-fonts-scale (or chinese-fonts-scale 1.2))
  (save-excursion
	(with-current-buffer (find-file-noselect "~/.config/system-config/emacs-font-size")
	  (delete-region (point-min) (point-max))
	  (insert (format "%s" english-font-size))
	  (let ((before-save-hook nil)
			(after-save-hook nil))
		(save-buffer))
	  (kill-buffer)))
  (setq face-font-rescale-alist `(("Microsoft Yahei" . ,chinese-fonts-scale)
								  ("Microsoft_Yahei" . ,chinese-fonts-scale)
								  ("微软雅黑" . ,chinese-fonts-scale)
								  ("WenQuanYi Zen Hei" . ,chinese-fonts-scale)))
  "english-font-size could be set to \":pixelsize=18\" or a integer.
If set/leave chinese-font-size to nil, it will follow english-font-size"
  (require 'cl)                         ; for find if
  (setq doglock-english-font-size english-font-size)
  (let ((en-font (doglock-make-font-string
				  (find-if #'doglock-font-existsp english-fonts)
				  english-font-size))
		(zh-font (font-spec :family (find-if #'doglock-font-existsp chinese-fonts))))

	;; Set the default English font
	;;
	;; The following 2 method cannot make the font settig work in new frames.
	;; (set-default-font "Consolas:pixelsize=18")
	;; (add-to-list 'default-frame-alist '(font . "Consolas:pixelsize=18"))
	;; We have to use set-face-attribute
	(set-face-attribute
	 'default nil :font en-font)
	(condition-case font-error
		(progn
		  (set-face-font 'italic (font-spec :family "JetBrains Mono" :slant 'italic :weight 'normal :size (+ 0.0 english-font-size)))
		  (set-face-font 'bold-italic (font-spec :family "JetBrains Mono" :slant 'italic :weight 'bold :size (+ 0.0 english-font-size)))

		  (set-fontset-font t 'symbol (font-spec :family "JetBrains Mono")))
	  (error nil))
	(set-fontset-font t 'symbol (font-spec :family "Unifont") nil 'append)
	(set-fontset-font
	 t '(#x2009 . #x2009) (font-spec :family "B&H LucidaBright"))
	(set-fontset-font t nil (font-spec :family "DejaVu Sans"))

	;; Set Chinese font
	;; Do not use 'unicode charset, it will cause the english font setting invalid
	(dolist (charset '(kana han cjk-misc bopomofo))
	  (set-fontset-font t charset zh-font)))
  (when (and (boundp 'global-emojify-mode)
			 global-emojify-mode)
	(global-emojify-mode 1))
  (shell-command-to-string "setsid sawfish-client -e '(maximize-window (input-focus))'"))


(defvar doglock-english-fonts '("JetBrains Mono" "Monaco" "Consolas" "DejaVu Sans Mono" "Monospace" "Courier New"))
(defvar doglock-chinese-fonts '("Microsoft Yahei" "Microsoft_Yahei" "微软雅黑" "文泉驿等宽微米黑" "黑体" "新宋体" "宋体"))

(doglock-set-font
 doglock-english-fonts
 (if (file-exists-p "~/.config/system-config/emacs-font-size")
	 (save-excursion
	   (find-file "~/.config/system-config/emacs-font-size")
	   (goto-char (point-min))
	   (let ((monaco-font-size (read (current-buffer))))
		 (kill-buffer (current-buffer))
		 (if (numberp monaco-font-size)
			 monaco-font-size
		   12.5)))
   12.5)
 doglock-chinese-fonts)

(defvar chinese-font-size-scale-alist nil)

;; On different platforms, I need to set different scaling rate for
;; differnt font size.
(cond
 ((and (boundp '*is-a-mac*) *is-a-mac*)
  (setq chinese-font-size-scale-alist '((10.5 . 1.3) (11.5 . 1.3) (16 . 1.3) (18 . 1.25))))
 ((and (boundp '*is-a-win*) *is-a-win*)
  (setq chinese-font-size-scale-alist '((11.5 . 1.25) (16 . 1.25))))
 (t ;; is a linux:-)
  (setq chinese-font-size-scale-alist '((12 . 1.25) (12.5 . 1.25) (14 . 1.20) (16 . 1.25) (20 . 1.20)))))

(defvar doglock-english-font-size-steps '(9 10.5 11.5 12 12.5 13 14 16 18 20 22 40))
(defun doglock-step-frame-font-size (step)
  (let ((steps doglock-english-font-size-steps)
		next-size)
	(when (< step 0)
		(setq steps (reverse doglock-english-font-size-steps)))
	(setq next-size
		  (cadr (member doglock-english-font-size steps)))
	(when next-size
		(doglock-set-font doglock-english-fonts next-size doglock-chinese-fonts (cdr (assoc next-size chinese-font-size-scale-alist)))
		(message "Your font size is set to %.1f" next-size))))

(global-set-key [(control x) (meta -)] (lambda () (interactive) (doglock-step-frame-font-size -1)))
(global-set-key [(control x) (meta +)] (lambda () (interactive) (doglock-step-frame-font-size 1)))

(set-face-attribute 'default nil :font (font-spec))

;; {%org-mode%}
;; here are 20 hanzi and 40 english chars, see if they are the same width
;; 你你你你你你你你你你你你你你你你你你你你
;; aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
;; /aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa/
;; {%/org-mode%}
