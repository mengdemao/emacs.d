;;; init-org.el --- Org-mode config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(add-to-list 'load-path (expand-file-name "site/org-mode/lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site/org-mode/lisp/contrib/lisp" user-emacs-directory))
(require 'org-install)

;; 允许使用选择
(setq org-support-shift-select t)

;; 禁止下划线转义
(setq org-export-with-sub-superscripts nil)

(setq org-plantuml-jar-path
	  (expand-file-name "~/.emacs.d/bin/plantuml.jar"))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   `((R . t)
	 (makefile . t)
	 (scheme . t)
	 (ditaa . t)
	 (dot . t)
	 (shell . t)
	 (emacs-lisp . t)
	 (gnuplot . t)
	 (haskell . nil)
	 (latex . t)
	 (ledger . t)
	 (ocaml . nil)
	 (octave . t)
	 (plantuml . t)
	 (python . t)
	 (ruby . t)
	 (screen . nil)
	 (,(if (locate-library "ob-sh") 'sh 'shell) . t)
	 (sql . t)
	 (sqlite . t))))

(setq geiser-default-implementation 'chez)

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(require 'toc-org)
(if (require 'toc-org nil t)
	(add-hook 'org-mode-hook 'toc-org-mode)

  ;; enable in markdown, too
  (add-hook 'markdown-mode-hook 'toc-org-mode)
  (define-key markdown-mode-map (kbd "\C-c\C-o") 'toc-org-markdown-follow-thing-at-point))

;; 将图片拖入org-mode中
(use-package org-download
  :ensure t)

(setq org-confirm-babel-evaluate nil)	;; 直接计算

(use-package ox-hugo
  :ensure t)

(use-package ox-publish
  :ensure nil
  :config  
  (setq org-publish-project-alist
        '(("orgfiles"
           ;; ; Sources and destinations for files.
           :base-directory "~/.emacs.d/blog/org/"          ;; ** 源  .org 文件放置的目录
           :publishing-directory "~/.emacs.d/blog/public/" ;; ** 生成的站点文件放置的目录
           ;; ; Selecting files
           :base-extension "org"
           :recursive t
           ;; ; Publishing action
           :publishing-function org-html-publish-to-html
		   
           ;; ;;; Options for the exporters
		   
           ;; ; Generic properties
           :headline-levels 4
           :section-numbers nil
           :with-author "Jack Liu"                ;; ** 站点拥有者名称
           :with-priority t
           :with-toc t
		   
           ;; ; HTML specific properties
           :html-doctype "html5"
		   
           ;; ; Other options
           :table-of-contents t
           )
		  
          ;; static assets
          ;; 静态文件输出设置
          ("js"
           :base-directory "~/.emacs.d/blog/js/"
           :base-extension "js"
           :publishing-directory "~/.emacs.d/blog/public/js/"
           :recursive t
           :publishing-function org-publish-attachment
           )
          ("css"
           :base-directory "~/.emacs.d/blog/css/"
           :base-extension "css"
           :publishing-directory "~/.emacs.d/blog/public/css/"
           :recursive t
           :publishing-function org-publish-attachment
           )
          ("images"
           :base-directory "~/.emacs.d/blog/images/"
           :base-extension "jpg\\|gif\\|png\\|svg\\|gif"
           :publishing-directory "~/.emacs.d/blog/public/images/"
           :recursive t
           :publishing-function org-publish-attachment
           )
          ("assets"
           :base-directory "~/.emacs.d/blog/assets/"
           :base-extension "mp3"
           :publishing-directory "~/.emacs.d/blog/public/assets/"
           :recursive t
           :publishing-function org-publish-attachment
           )
          ("webfonts"
           :base-directory "~/.emacs.d/blog/webfonts/"
           :base-extension "eot\\|svg\\|ttf\\|woff\\|woff2"
           :publishing-directory "~/.emacs.d/blog/public/webfonts/"
           :recursive t
           :publishing-function org-publish-attachment
           )
		  
          ("website" :components ("orgfiles" "js" "css" "images" "assets" "webfonts"))
          ("statics" :components ("js" "css" "images" "assets" "webfonts"))
          )))

(provide 'init-org)
;;; init-org.el ends here
