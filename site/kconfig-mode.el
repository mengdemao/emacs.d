;;; kconfig.el - a major mode for editing linux kernel config (Kconfig) files
;; Copyright © 2014 Yu Peng
;; Copyright © 2014 Michal Sojka
 
(defvar kconfig-mode-font-lock-keywords
  '(("^[\t, ]*\\_<bool\\_>" . font-lock-type-face)
    ("^[\t, ]*\\_<int\\_>" . font-lock-type-face)
    ("^[\t, ]*\\_<boolean\\_>" . font-lock-type-face)
    ("^[\t, ]*\\_<tristate\\_>" . font-lock-type-face)
    ("^[\t, ]*\\_<depends on\\_>" . font-lock-variable-name-face)
    ("^[\t, ]*\\_<select\\_>" . font-lock-variable-name-face)
    ("^[\t, ]*\\_<help\\_>" . font-lock-variable-name-face)
    ("^[\t, ]*\\_<---help---\\_>" . font-lock-variable-name-face)
    ("^[\t, ]*\\_<default\\_>" . font-lock-variable-name-face)
    ("^[\t, ]*\\_<range\\_>" . font-lock-variable-name-face)
    ("^\\_<config\\_>" . font-lock-constant-face)
    ("^\\_<comment\\_>" . font-lock-constant-face)
    ("^\\_<menu\\_>" . font-lock-constant-face)
    ("^\\_<endmenu\\_>" . font-lock-constant-face)
    ("^\\_<if\\_>" . font-lock-constant-face)
    ("^\\_<endif\\_>" . font-lock-constant-face)
    ("^\\_<menuconfig\\_>" . font-lock-constant-face)
    ("^\\_<source\\_>" . font-lock-keyword-face)
    ("\#.*" . font-lock-comment-face)
    ("\".*\"$" . font-lock-string-face)))

(defvar kconfig-headings
  '("bool" "int" "boolean" "tristate" "depends on" "select"
    "help" "---help---" "default" "range" "config" "comment"
    "menu" "endmenu" "if" "endif" "menuconfig" "source"))

(defun kconfig-outline-level ()
  (looking-at "[\t ]*")
  (let ((prefix (match-string 0))
	(result 0))
    (dotimes (i (length prefix) result)
      (setq result (+ result
		      (if (equal (elt prefix i) ?\s)
			  1 tab-width))))))

(define-derived-mode kconfig-mode text-mode
  "kconfig"
  (set (make-local-variable 'font-lock-defaults)
       '(kconfig-mode-font-lock-keywords t))
  (set (make-local-variable 'outline-regexp)
       (concat "^[\t ]*" (regexp-opt kconfig-headings)))
  (set (make-local-variable 'outline-level)
       'kconfig-outline-level))

(add-to-list 'auto-mode-alist '("Kconfig" . kconfig-mode))

(provide 'kconfig-mode)