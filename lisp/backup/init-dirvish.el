;;; init-dirvish.el --- dir -*- lexical-binding: t -*-

;; Author: shaowen he

;;; Commentary:

;; commentary

;;; Code:


;; 暂时不加载，与init-dired取其一

(use-package dired
  :config
  (setq dired-dwim-target t)
  :hook (dired-mode . dired-hide-details-mode))

(use-package dired-sidebar)

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package dirvish
  :config
  (setq dirvish-attributes '(subtree-state nerd-icons)
        dirvish-side-width 40)
  :bind ("TAB" . dirvish-toggle-subtree)
  :hook (dired-mode . dirvish-override-dired-mode)
  :when (dirvish-side-follow-mode))


(provide 'init-dirvish)

;;; init-dirvish.el ends here
