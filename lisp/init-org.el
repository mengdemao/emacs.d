;;; init-org.el --- Org-mode config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(add-to-list 'load-path (expand-file-name "site/org-mode/lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site/org-mode/lisp/contrib/lisp" user-emacs-directory))
(require 'org-install)

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
  :ensure t
  :after ox)

(provide 'init-org)
;;; init-org.el ends here
