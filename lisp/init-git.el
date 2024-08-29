;;; init-git.el --- git -*- lexical-binding: t -*-

;; Author: shaowen he

;;; Commentary:

;; commentary

;;; Code:

;; 注释的一行：是否显示在同一个窗口 -- 不需要过多的设置，直接可用
(use-package magit
  :commands (magit-status magit-add-section-hook)
  :custom
  (magit-diff-refine-hunk t)
  (magit-module-sections-nested nil)
  ;; (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :config
  (setq magit-show-long-lines-warning nil)
  (put 'magit-clean 'disabled nil)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-unpulled-from-upstream)
  (with-eval-after-load "magit-submodule"
    (remove-hook 'magit-module-sections-hook 'magit-insert-modules-unpulled-from-pushremote)
    (remove-hook 'magit-module-sections-hook 'magit-insert-modules-unpushed-to-upstream)
    (remove-hook 'magit-module-sections-hook 'magit-insert-modules-unpushed-to-pushremote)))


;; 获取当前buffer的github地址
(use-package git-link
  :bind (("C-c g l" . git-link)
         ("C-c g h" . git-link-homepage)
         ("C-c g c" . git-link-commit)))



(provide 'init-git)

;;; init-git.el ends here
