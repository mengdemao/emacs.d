;;; init-org.el --- Org-mode config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package org)

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

(use-package toc-org
  :ensure nil)

;; (if (require 'toc-org nil t)
;; 	(add-hook 'org-mode-hook 'toc-org-mode)

;;   ;; enable in markdown, too
;;   (add-hook 'markdown-mode-hook 'toc-org-mode)
;;   (define-key markdown-mode-map (kbd "\C-c\C-o") 'toc-org-markdown-follow-thing-at-point))

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

;; (require 'ox-latex)

(setq org-latex-pdf-process '("latexmk -xelatex -quiet -shell-escape -f %f"))

(setq org-latex-listings 'minted)
(setq org-latex-minted-options
      '(("bgcolor" "white")
		("breaklines" "true")
		("autogobble" "true")
		("fontsize" "\\small")))

;; 设置文档构建
;; (add-to-list 'org-latex-classes
;; 	     '("org-article"
;; 	       "\\documentclass[letterpaper,11pt]{article}
;; \\usepackage{graphicx}
;; \\usepackage{xcolor}
;; \\usepackage{ctex}
;; \\usepackage{fixltx2e}
;; \\usepackage{longtable}
;; \\usepackage{float}
;; \\usepackage{tikz}
;; \\usepackage{wrapfig}
;; \\usepackage{latexsym,amssymb,amsmath}
;; \\usepackage{textcomp}
;; \\usepackage{listings}
;; \\usepackage{marvosym}
;; \\usepackage{textcomp}
;; \\usepackage{latexsym}
;; \\usepackage{natbib}
;; \\usepackage{geometry}
;;  [NO-DEFAULT-PACKAGES]
;;  [PACKAGES]
;;  [EXTRA]"
;; 	       ("\\section{%s}" . "\\section*{%s}")
;; 	       ("\\subsection{%s}" . "\\subsection*{%s}")
;; 	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
;; 	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
;; 	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; ;; 设置书籍构建
;; (add-to-list 'org-latex-classes '("org-book"
;; 				  "\\documentclass[letterpaper,14pt]{article}
;; \\usepackage{graphicx}
;; \\usepackage{xcolor}
;; \\usepackage{ctex}
;; \\usepackage{fixltx2e}
;; \\usepackage{longtable}
;; \\usepackage{float}
;; \\usepackage{tikz}
;; \\usepackage{wrapfig}
;; \\usepackage{latexsym,amssymb,amsmath}
;; \\usepackage{textcomp}
;; \\usepackage{listings}
;; \\usepackage{marvosym}
;; \\usepackage{textcomp}
;; \\usepackage{latexsym}
;; \\usepackage{natbib}
;; \\usepackage{geometry}
;; \\usepackage{minted}
;; \\usepackage{hyperref}
;;  [NO-DEFAULT-PACKAGES]
;;  [PACKAGES]
;;  [EXTRA]"
;; 				  ("\\section{%s}" . "\\section*{%s}")
;; 				  ("\\subsection{%s}" . "\\subsection*{%s}")
;; 				  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
;; 				  ("\\paragraph{%s}" . "\\paragraph*{%s}")
;; 				  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; ;; 使用Listings宏包格式化源代码(只是把代码框用listing环境框起来，还需要额外的设置)
;; (setq org-export-latex-listings t)
;; ;; Options for \lset command（reference to listing Manual)
;; (setq org-export-latex-listings-options
;;       '(
;; 	("basicstyle" "\\color{foreground}\\small\\mono")           ; 源代码字体样式
;; 	("keywordstyle" "\\color{function}\\bfseries\\small\\mono") ; 关键词字体样式
;; 	("identifierstyle" "\\color{doc}\\small\\mono")
;; 	("commentstyle" "\\color{comment}\\small\\itshape")         ; 批注样式
;; 	("stringstyle" "\\color{string}\\small")                    ; 字符串样式
;; 	("showstringspaces" "false")                                ; 字符串空格显示
;; 	("numbers" "left")                                          ; 行号显示
;; 	("numberstyle" "\\color{preprocess}")                       ; 行号样式
;; 	("stepnumber" "1")                                          ; 行号递增
;; 	("backgroundcolor" "\\color{background}")                   ; 代码框背景色
;; 	("tabsize" "4")                                             ; TAB等效空格数
;; 	("captionpos" "t")                                          ; 标题位置 top or buttom(t|b)
;; 	("breaklines" "true")                                       ; 自动断行
;; 	("breakatwhitespace" "true")                                ; 只在空格分行
;; 	("showspaces" "false")                                      ; 显示空格
;; 	("columns" "flexible")                                      ; 列样式
;; 	("frame" "single")                                          ; 代码框：阴影盒
;; 	("frameround" "tttt")                                       ; 代码框： 圆角
;; 	("framesep" "0pt")
;; 	("framerule" "8pt")
;; 	("rulecolor" "\\color{background}")
;; 	("fillcolor" "\\color{white}")
;; 	("rulesepcolor" "\\color{comdil}")
;; 	("framexleftmargin" "10mm")
;; 	))

;; (add-to-list 'org-latex-packages-alist '("" "minted"))

(provide 'init-org)
;;; init-org.el ends here
