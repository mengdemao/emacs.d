;;; init-const.el --- const  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defconst centaur-homepage
  "https://github.com/seagle0128/.emacs.d"
  "The Github page of Centaur Emacs.")

(defconst centaur-custom-example-file
  (expand-file-name "custom-example.el" user-emacs-directory)
  "Custom example file of Centaur Emacs.")

(defconst centaur-custom-post-file
  (expand-file-name "custom-post.el" user-emacs-directory)
  "Custom file after startup.

Put private configurations to override defaults here.")

(defconst centaur-custom-post-org-file
  (expand-file-name "custom-post.org" user-emacs-directory)
  "Custom org file after startup.

Put private configurations to override defaults here.
Loaded by `org-babel-load-file'.")

(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst sys/mac-x-p
  (and (display-graphic-p) sys/macp)
  "Are we running under X on a Mac system?")

(defconst sys/mac-ns-p
  (eq window-system 'ns)
  "Are we running on a GNUstep or Macintosh Cocoa display?")

(defconst sys/mac-cocoa-p
  (featurep 'cocoa)
  "Are we running with Cocoa on a Mac system?")

(defconst sys/mac-port-p
  (eq window-system 'mac)
  "Are we running a macport build on a Mac system?")

(defconst sys/linux-x-p
  (and (display-graphic-p) sys/linuxp)
  "Are we running under X on a GNU/Linux system?")

(defconst sys/cygwinp
  (eq system-type 'cygwin)
  "Are we running on a Cygwin system?")

(defconst sys/rootp
  (string-equal "root" (getenv "USER"))
  "Are you using ROOT user?")

(defconst emacs/>=25p
  (>= emacs-major-version 25)
  "Emacs is 25 or above.")

(defconst emacs/>=26p
  (>= emacs-major-version 26)
  "Emacs is 26 or above.")

(defconst emacs/>=27p
  (>= emacs-major-version 27)
  "Emacs is 27 or above.")

(defconst emacs/>=25.3p
  (or emacs/>=26p
	  (and (= emacs-major-version 25) (>= emacs-minor-version 3)))
  "Emacs is 25.3 or above.")

(defconst emacs/>=25.2p
  (or emacs/>=26p
	  (and (= emacs-major-version 25) (>= emacs-minor-version 2)))
  "Emacs is 25.2 or above.")

(provide 'init-const)
;;; init-const.el ends here
