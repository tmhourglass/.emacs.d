;;; init-dirvish.el --- dirvish -*- lexical-binding: t -*-

;; Author: shaowen he

;;; Commentary:

;; replace dired with dirvish
;; 使用dirvish替换dired

;;; Code:

(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :config
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-mode-line-height 10)
  (setq dirvish-attributes
        '(nerd-icons file-time file-size collapse subtree-state vc-state git-msg))
  (setq dirvish-subtree-state-style 'nerd)
  (setq delete-by-moving-to-trash t)
  (setq dirvish-path-separators (list
                                 (format "  %s " (nerd-icons-codicon "nf-cod-home"))
                                 (format "  %s " (nerd-icons-codicon "nf-cod-root_folder"))
                                 (format " %s " (nerd-icons-faicon "nf-fa-angle_right"))))
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")
  ;; (dirvish-peek-mode)                   ; Preview files in minibuffer
  ;; (dirvish-side-follow-mode)            ; similar to `treemacs-follow-mode'

  :custom
  (dirvish-quick-access-entries        ; It's a custom option, `setq' won't work
   '(("h" "~/" "Home")
     ("d" "~/Downloads/" "Downloads")))

  :bind                ; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish-fd)
   :map dirvish-mode-map                ; Dirvish inherits `dired-mode-map'
   ("a" . dirvish-quick-access)
   ("f" . dirvish-file-info-menu)
   ("y" . dirvish-yank-menu)
   ("N" . dirvish-narrow)
   ("^" . dirvish-history-last)
   ("h" . dirvish-history-jump)         ; remapped `describe-mode'
   ("s" . dirvish-quicksort)            ; remapped `dired-sort-toggle-or-edit'
   ;; ("v" . dirvish-vc-menu)              ; remapped `dired-view-file'
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-t" . dirvish-layout-toggle)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump)))


(provide 'init-dirvish)

;;; init-dirvish.el ends here
