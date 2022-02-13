;;; init-latex.el --- Latex-mode config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'ox-latex)

(setq org-latex-pdf-process '("latexmk -lualatex -quiet -shell-escape -f %f"))

(setq org-latex-listings 'minted)
(setq org-latex-minted-options
      '(("bgcolor" "white")
	("breaklines" "true")
	("autogobble" "true")
	("fontsize" "\\small")))

(add-to-list 'org-latex-classes
	     '("org-cv"
	       "\\documentclass[a4paper,titlepage]{article}
\\usepackage{graphicx}
\\usepackage{xcolor}
\\usepackage{ctex}
\\usepackage{fixltx2e}
\\usepackage{longtable}
\\usepackage{float}
\\usepackage{tikz}
\\usepackage{wrapfig}
\\usepackage{latexsym,amssymb,amsmath}
\\usepackage{textcomp}
\\usepackage{listings}
\\usepackage{marvosym}
\\usepackage{textcomp}
\\usepackage{latexsym}
\\usepackage{natbib}
\\usepackage{geometry}
\\geometry{a4paper,scale=0.75,centering}
\\pagestyle{empty}							% 没有页眉页脚
\\usepackage[cache=false]{minted}			% 代码生成环境
\\usepackage{hyperref}
 [NO-DEFAULT-PACKAGES]
 [PACKAGES]
 [EXTRA]"
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; 设置文档构建
(add-to-list 'org-latex-classes
	     '("org-article"
	       "\\documentclass[letterpaper,11pt]{article}
\\usepackage{graphicx}
\\usepackage{xcolor}
\\usepackage{ctex}
\\usepackage{fixltx2e}
\\usepackage{longtable}
\\usepackage{float}
\\usepackage{tikz}
\\usepackage{wrapfig}
\\usepackage{latexsym,amssymb,amsmath}
\\usepackage{textcomp}
\\usepackage{listings}
\\usepackage{marvosym}
\\usepackage{textcomp}
\\usepackage{latexsym}
\\usepackage{natbib}
\\usepackage{geometry}
 [NO-DEFAULT-PACKAGES]
 [PACKAGES]
 [EXTRA]"
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; 设置书籍构建
(add-to-list 'org-latex-classes '("org-book"
				  "\\documentclass[letterpaper,14pt]{article}
\\usepackage{graphicx}
\\usepackage{xcolor}
\\usepackage{ctex}
\\usepackage{fixltx2e}
\\usepackage{longtable}
\\usepackage{float}
\\usepackage{tikz}
\\usepackage{wrapfig}
\\usepackage{latexsym,amssymb,amsmath}
\\usepackage{textcomp}
\\usepackage{listings}
\\usepackage{marvosym}
\\usepackage{textcomp}
\\usepackage{latexsym}
\\usepackage{natbib}
\\usepackage{geometry}
\\usepackage{minted}
\\usepackage{hyperref}
 [NO-DEFAULT-PACKAGES]
 [PACKAGES]
 [EXTRA]"
				  ("\\section{%s}" . "\\section*{%s}")
				  ("\\subsection{%s}" . "\\subsection*{%s}")
				  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
				  ("\\paragraph{%s}" . "\\paragraph*{%s}")
				  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; 使用Listings宏包格式化源代码(只是把代码框用listing环境框起来，还需要额外的设置)
(setq org-export-latex-listings t)
;; Options for \lset command（reference to listing Manual)
(setq org-export-latex-listings-options
      '(
	("basicstyle" "\\color{foreground}\\small\\mono")           ; 源代码字体样式
	("keywordstyle" "\\color{function}\\bfseries\\small\\mono") ; 关键词字体样式
	("identifierstyle" "\\color{doc}\\small\\mono")
	("commentstyle" "\\color{comment}\\small\\itshape")         ; 批注样式
	("stringstyle" "\\color{string}\\small")                    ; 字符串样式
	("showstringspaces" "false")                                ; 字符串空格显示
	("numbers" "left")                                          ; 行号显示
	("numberstyle" "\\color{preprocess}")                       ; 行号样式
	("stepnumber" "1")                                          ; 行号递增
	("backgroundcolor" "\\color{background}")                   ; 代码框背景色
	("tabsize" "4")                                             ; TAB等效空格数
	("captionpos" "t")                                          ; 标题位置 top or buttom(t|b)
	("breaklines" "true")                                       ; 自动断行
	("breakatwhitespace" "true")                                ; 只在空格分行
	("showspaces" "false")                                      ; 显示空格
	("columns" "flexible")                                      ; 列样式
	("frame" "single")                                          ; 代码框：阴影盒
	("frameround" "tttt")                                       ; 代码框： 圆角
	("framesep" "0pt")
	("framerule" "8pt")
	("rulecolor" "\\color{background}")
	("fillcolor" "\\color{white}")
	("rulesepcolor" "\\color{comdil}")
	("framexleftmargin" "10mm")
	))

(provide 'init-latex)
;;; init-latex.el ends here
