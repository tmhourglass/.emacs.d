;;; init-ctags.el --- ctags -*- lexical-binding: t -*-

;; Author: shaowen he

;;; Commentary:

;; commentary

;;; Code:

;; A fancy ctags frontend
(use-package citre
  :init
  ;; Load the prelude.
  (require 'citre-config)
  :bind (("C-c c j" . citre-jump)
         ("C-c c J" . citre-jump-back)
         ("C-c c p" . citre-peek)
         ("C-c c a" . citre-ace-peek)
         ("C-c c u" . citre-update-this-tags-file))
  :custom
  (citre-enable-capf-integration nil)
  (citre-prompt-language-for-ctags-command t)
  (citre-auto-enable-citre-mode-modes '(prog-mode)))



(provide 'init-ctags)

;;; init-ctags.el ends here
