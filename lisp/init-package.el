;;; init-package.el --- Settings and helpers for package.el -*- lexical-binding: t -*-
;;; Commentary:
;;; Emacs安装包管理
;;; Code:

(require 'package)
(require 'cl-lib)

(setq package-archives '
	  (
	   ("gnu" . "http://mirrors.ustc.edu.cn/elpa/gnu/")
	   ("melpa" . "http://mirrors.ustc.edu.cn/elpa/melpa/")
	   ("melpa-stable" . "http://mirrors.ustc.edu.cn/elpa/melpa-stable/")
	   ("org" . "http://mirrors.ustc.edu.cn/elpa/org/")))

(defun add-subdirs-to-load-path (parent-dir)
  "Add every non-hidden subdir of PARENT-DIR to `load-path'."
  (let ((default-directory parent-dir))
	(setq load-path
		  (append
		   (cl-remove-if-not
			#'file-directory-p
			(directory-files (expand-file-name parent-dir) t "^[^\\.]"))
		   load-path))))

;; Add both site and its immediate subdirs to `load-path'
(let ((site-dir (expand-file-name "site/" user-emacs-directory)))
  (push site-dir load-path)
  (add-subdirs-to-load-path site-dir))


;;; Utilities for grabbing upstream libs
(defun site-dir-for (name)
  (expand-file-name (format "site/%s" name) user-emacs-directory))

(defun site-library-el-path (name)
  (expand-file-name (format "%s.el" name) (site-dir-for name)))

(defun download-site-module (name url)
  (let ((dir (site-dir-for name)))
	(message "Downloading %s from %s" name url)
	(unless (file-directory-p dir)
	  (make-directory dir t))
	(add-to-list 'load-path dir)
	(let ((el-file (site-library-el-path name)))
	  (url-copy-file url el-file t nil)
	  el-file)))

(defun ensure-lib-from-url (name url)
  (unless (site-library-loadable-p name)
	(byte-compile-file (download-site-module name url))))

(defun site-library-loadable-p (name)
  "Return whether or not the library `name' can be loaded from a
source file under ~/.emacs.d/site/name/"
  (let ((f (locate-library (symbol-name name))))
	(and f (string-prefix-p (file-name-as-directory (site-dir-for name)) f))))

;; Work-around for https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
(when
	(and
	 (version< emacs-version "26.3")
	 (boundp 'libgnutls-version)
	 (>= libgnutls-version 30604))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;;; On-demand installation of packages

(defun require-package
	(package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (or
   (package-installed-p package min-version)
   (let*
	   (
		(known
		 (cdr
		  (assoc package package-archive-contents)))
		(versions
		 (mapcar #'package-desc-version known)))
	 (if
		 (cl-some
		  (lambda
			(v)
			(version-list-<= min-version v)) versions)
		 (package-install package)
	   (if no-refresh
		   (error "No version of %s >= %S is available" package min-version)
		 (package-refresh-contents)
		 (require-package package min-version t))))))

(defun maybe-require-package
	(package &optional min-version no-refresh)
  "Try to install PACKAGE, and return non-nil if successful.
In the event of failure, return nil and print a warning message.
Optionally require MIN-VERSION.  If NO-REFRESH is non-nil, the
available package lists will not be re-downloaded in order to
locate PACKAGE."
  (condition-case err
	  (require-package package min-version no-refresh)
	(error
	 (message "Couldn't install optional package `%s': %S" package err)
	 nil)))


;;; Fire up package.el

(setq package-enable-at-startup nil)
(package-initialize)


;; package.el updates the saved version of package-selected-packages correctly only
;; after custom-file has been loaded, which is a bug. We work around this by adding
;; the required packages to package-selected-packages after startup is complete.

(defvar required-packages nil)

(defun note-selected-package
	(oldfun package &rest args)
  "If OLDFUN reports PACKAGE was successfully installed, note that fact.
The package name is noted by adding it to
`required-packages'.  This function is used as an
advice for `require-package', to which ARGS are passed."
  (let
	  (
	   (available
		(apply oldfun package args)))
	(prog1
		available
	  (when available
		(add-to-list 'required-packages package)))))

(advice-add 'require-package :around 'note-selected-package)

(when
	(fboundp 'package--save-selected-packages)
  (require-package 'seq)
  (add-hook 'after-init-hook
			(lambda
			  ()
			  (package--save-selected-packages
			   (seq-uniq
				(append required-packages package-selected-packages))))))


(require 'fullframe)
(fullframe list-packages quit-window)

(let
	(
	 (package-check-signature nil))
  (require-package 'gnu-elpa-keyring-update))


(defun set-tabulated-list-column-width
	(col-name width)
  "Set any column with name COL-NAME to the given WIDTH."
  (when
	  (> width
		 (length col-name))
	(cl-loop for column across tabulated-list-format
			 when
			 (string= col-name
					  (car column))
			 do
			 (setf
			  (elt column 1) width))))

(defun maybe-widen-package-menu-columns
	()
  "Widen some columns of the package menu table to avoid truncation."
  (when
	  (boundp 'tabulated-list-format)
	(set-tabulated-list-column-width "Version" 13)
	(let
		(
		 (longest-archive-name
		  (apply 'max
				 (mapcar 'length
						 (mapcar 'car package-archives)))))
	  (set-tabulated-list-column-width "Archive" longest-archive-name))))

(add-hook 'package-menu-mode-hook 'maybe-widen-package-menu-columns)

;; 插件包管理器配置

;; Initialize packages
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

;; auto-compile
(setq load-prefer-newer t)
(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)
(setq auto-compile-display-buffer nil)
(setq auto-compile-mode-line-counter t)

(require 'packed)
(require 'bind-key)
(require 'diminish)

;; Should set before loading `use-package'
(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t)
  (setq use-package-verbose t))

(eval-when-compile
  (require 'use-package))

(provide 'init-package)
;; init-package.el ends here
