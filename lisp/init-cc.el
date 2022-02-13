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

;; modeline
;; (require-package 'doom-modeline)
;; (doom-modeline-mode 1)

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

(provide 'init-cc)