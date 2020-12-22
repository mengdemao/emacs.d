;;; init-theme.el --- Settings Color themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(add-to-list 'custom-theme-load-path (expand-file-name "themes/" user-emacs-directory))
(setq custom-safe-themes t)

;; (load-theme 'monokai t)
;; (load-theme 'molokai t)

;; doom主题
(load-theme 'doom-molokai t)

;; 基础主题
;; (load-theme 'zenburn t)
;; (load-theme 'grandshell t)
;; (load-theme 'cyberpunk t)
;; (load-theme 'dorsey t)
;; (load-theme 'dracula t)
;; (load-theme 'granger t)

;; 扩展主题
;; (load-theme 'gruvbox t)

;; (setq ;; foreground and background
;;	  monokai-foreground     "#ABB2BF"
;;	  monokai-background     "#282C34"
;;	  ;; highlights and comments
;;	  monokai-comments       "#F8F8F0"
;;	  monokai-emphasis       "#282C34"
;;	  monokai-highlight      "#FFB269"
;;	  monokai-highlight-alt  "#66D9EF"
;;	  monokai-highlight-line "#1B1D1E"
;;	  monokai-line-number    "#F8F8F0"
;;	  ;; colours
;;	  monokai-blue           "#61AFEF"
;;	  monokai-cyan           "#56B6C2"
;;	  monokai-green          "#98C379"
;;	  monokai-gray           "#3E4451"
;;	  monokai-violet         "#C678DD"
;;	  monokai-red            "#E06C75"
;;	  monokai-orange         "#D19A66"
;;	  monokai-yellow         "#E5C07B")

(provide 'init-themes)
;; init-elpa.el ends here
