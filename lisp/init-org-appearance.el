;;; init-org-appearance.el --- Org appearance configuration -*- lexical-binding: t -*-

;; Author: shaowen he

;;; Commentary:

;; Org appearance: superstar, toc-org, org-appear, org-present, org-modern.

;;; Code:

(with-eval-after-load 'org
  (progn

    ;; Prettify UI -- 替换svg-tab-mode
    ;; org-modern有问题：三级标题乱码/列表缩进太多/标题的图标也不太好
    ;; 不使用这个也还好，还比较顺畅  -- 暂时不用，后面再调试
    ;; (use-package org-modern
    ;;   :after org
    ;;   :hook ((org-mode . org-modern-mode)
    ;;          (org-agenda-finalize . org-modern-agenda)
    ;;          (org-modern-mode . (lambda ()
    ;;                               "Adapt `org-modern-mode'."
    ;;                               ;; Disable Prettify Symbols mode
    ;;                               (setq prettify-symbols-alist nil)
    ;;                               (prettify-symbols-mode -1)))))


    (use-package org-superstar
      :after org
      :hook (org-mode . org-superstar-mode)
      :config
      (setq org-superstar-special-todo-items t))



    ;; 使用magit submodule来添加，不使用package安装
    ;; 非melpa包，使用quelpa安装的，均使用:ensure nil，否则启动报错
    ;; 未生效，没有效果
    ;; (use-package org-modern-indent
    ;;   :ensure nil
    ;;   :quelpa (org-modern-indent :fetcher github :repo "jdtsmith/org-modern-indent")
    ;;   :config
    ;;   (add-hook 'org-mode-hook #'org-modern-indent-mode 90))


    ;; Table of contents
    (use-package toc-org
      :after org
      :hook (org-mode . toc-org-mode))

    ;; Add graphical view of agenda
    ;; agenda任务计划中显示时间线 -- 暂时用不到（显示也不太好）
    ;; (use-package org-timeline
    ;;   :after org
    ;;   :hook (org-agenda-finalize . org-timeline-insert-timeline))


    (defun org-apperance-evil-hack ()
      (add-hook 'evil-insert-state-entry-hook #'org-appear-manual-start nil t)
      (add-hook 'evil-insert-state-exit-hook #'org-appear-manual-stop nil t))

    (use-package org-appear
      :after org
      :hook (org-mode . org-appear-mode)
      :init
      (setq org-appear-trigger 'manual)
      (add-hook 'org-mode-hook 'org-apperance-evil-hack))


    (use-package org-download
      :demand t
      :after org
      :config
      (add-hook 'dired-mode-hook 'org-download-enable)
      ;; :ensure-system-package (pngpaste . "brew install pngpaste")
      (when sys/macp
        (setq org-download-screenshot-method "pngpaste %s"))
      ;; https://imagemagick.org/script/download.php#windows  ImageMagick-7.1.0-portable-Q16-HDRI-x86.zip
      (when sys/win32p
        (setq org-download-screenshot-method "convert clipboard: %s"))
      (defun org-download-annotate-default (link)
        "Annotate LINK with the time of download."
        (make-string 0 ?\s))

      (setq-default org-download-heading-lvl nil
                    org-download-image-dir "./img"
                    org-download-screenshot-method "screencapture -i %s"
                    org-download-screenshot-file (expand-file-name "screenshot.jpg" temporary-file-directory)))

    (defun dw/org-present-prepare-slide ()
      (org-overview)
      (org-show-entry)
      (org-show-children))

    (defun dw/org-present-hook ()
      (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                         (header-line (:height 4.0) variable-pitch)
                                         (org-document-title (:height 1.75) org-document-title)
                                         (org-code (:height 1.55) org-code)
                                         (org-verbatim (:height 1.55) org-verbatim)
                                         (org-block (:height 1.25) org-block)
                                         (org-block-begin-line (:height 0.7) org-block)))
      (setq header-line-format " ")
      (org-display-inline-images)
      (dw/org-present-prepare-slide)
      (visual-fill-column-mode 1)
      (visual-line-mode 1))

    (defun dw/org-present-quit-hook ()
      (setq-local face-remapping-alist '((default variable-pitch default)))
      (setq header-line-format nil)
      (org-present-small)
      (org-remove-inline-images)
      (visual-fill-column-mode 0)
      (visual-line-mode 0))

    (defun dw/org-present-prev ()
      (interactive)
      (org-present-prev)
      (dw/org-present-prepare-slide))

    (defun dw/org-present-next ()
      (interactive)
      (org-present-next)
      (dw/org-present-prepare-slide))


    ;; 类似的包为org-tree-slide -- 参考Centaur
    (use-package org-present
      :bind (:map org-present-mode-keymap
                  ("C-c C-j" . dw/org-present-next)
                  ("C-c C-k" . dw/org-present-prev))
      :hook ((org-present-mode . dw/org-present-hook)
             (org-present-mode-quit . dw/org-present-quit-hook)))

    ))

(provide 'init-org-appearance)
;;; init-org-appearance.el ends here
