;;; init-vcs.el --- Vcs Settting -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package magit
  :ensure t)

(use-package git-timemachine
  :ensure t)

(provide 'init-vcs)
;;; init-vcs.el ends here
