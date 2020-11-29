;;; init-site.el --- Support elisp manually installed in the site dir
;;; -*- lexical-binding: t -*- Commentary: Code:

;;; Set load path

(require 'cl-lib)

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


(provide 'init-site)
;;; init-site.el ends here
