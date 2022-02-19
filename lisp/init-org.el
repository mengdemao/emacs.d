;;; init-org.el --- Org-mode config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package org)
(use-package org-plus-contrib)

;; 允许使用选择
(setq org-support-shift-select t)

;; 禁止下划线转义
(setq org-export-with-sub-superscripts nil)

(setq org-startup-indented t
      org-ellipsis "  " ;; folding symbol
      org-pretty-entities t
      org-hide-emphasis-markers t
      ;; show actually italicized text instead of /italicized text/
      org-agenda-block-separator ""
      org-fontify-whole-heading-line t
      org-fontify-done-headline t
      org-fontify-quote-and-verse-blocks t)

;; 修复org-mode源码格式化错误
(setq org-src-preserve-indentation t)
(setq org-edit-src-content-indentation 0)

;; 导出html禁止validate
(setq org-html-validation-link nil)

(setq org-plantuml-jar-path
	  (expand-file-name "~/.emacs.d/bin/plantuml.jar"))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
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
	 (sql . t)
	 (sqlite . t))))

(setq geiser-default-implementation 'chez)

(use-package org-superstar)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
(setq org-superstar-prettify-item-bullets t)

(use-package toc-org)
(if (require 'toc-org nil t)
	(add-hook 'org-mode-hook 'toc-org-mode)

  ;; enable in markdown, too
  (add-hook 'markdown-mode-hook 'toc-org-mode)
  (define-key markdown-mode-map (kbd "\C-c\C-o") 'toc-org-markdown-follow-thing-at-point))

;; 将图片拖入org-mode中
(use-package org-download)

(setq org-confirm-babel-evaluate nil)	;; 直接计算

(use-package ox-hugo)

;; 设置任务流程(这是我的配置)
(setq org-todo-keywords
      '((sequence "未开始(p!)" "进行中(t!)" "阻塞中(s!)" "|" "已完成(d!)" "已取消(a@/!)")))

;; 设置任务样式
(setq org-todo-keyword-faces
   '(("未开始" . (:foreground "red" :weight bold))
     ("阻塞中" . (:foreground "red" :weight bold))
     ("进行中" . (:foreground "orange" :weight bold))
     ("已完成" . (:foreground "green" :weight bold))
     ("已取消" . (:background "gray" :foreground "black"))
))

(setq org-agenda-files (list "~.emacs.d/gtd/todo.org"))
(global-set-key "\C-ca" 'org-agenda)

;; 添加orgmode简历
(require 'ox-awesomecv)
(require 'ox-moderncv)
(require 'ox-hugocv)
(require 'ox-altacv)

;; 添加导出minted
(setq org-latex-listings 'minted)
(setq org-latex-minted-options
      '(
	("bgcolor" "white")
	("breaklines" "true")
	("autogobble" "true")
	("fontsize" "\\large")
       )
)

(add-to-list 'org-latex-packages-alist '("" "minted"))

(provide 'init-org)
;;; init-org.el ends here
