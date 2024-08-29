;;; init-read.el --- read -*- lexical-binding: t -*-

;; Author: shaowen he

;;; Commentary:

;; commentary

;;; Code:


;; olivetti 自动平衡窗口边缘，并不是完全的无干扰写作，改为writeroom-mode

;; writeroom-mode 无干扰写作模式，全身心码字，全屏化，隐藏工具栏等
;; org-mode自动进入散文模式prose-mode，退出时恢复之前的模式设置
;; 不绑定org-mode，否则会导致org-capture有问题，若需要进入，使用`SPC t w`进入
(use-package writeroom-mode
  ;; :hook (org-mode . prose-mode)
  :custom
  (writeroom-fullscreen-effect 'maximized)
  (writeroom-mode-line-toggle-position 'mode-line-format)
  :preface
  (define-minor-mode prose-mode

    "Set up a buffer for prose editing.
This enables or modifies a number of settings so that the
experience of editing prose is a little more like that of a
typical word processor."
    :init-value nil :lighter " Prose" :keymap nil
    (if prose-mode
        (progn
          (when (fboundp 'writeroom-mode)
            (writeroom-mode 1))
          (setq truncate-lines nil)
          (setq word-wrap t)
          (setq word-wrap-by-category t)
          (setq cursor-type 'bar)
          (when (eq major-mode 'org)
            (kill-local-variable 'buffer-face-mode-face))
          (buffer-face-mode 1)
          (setq-local blink-cursor-interval 0.6)
          (setq-local show-trailing-whitespace nil)
          ;; (setq-local line-spacing 0.2)
          (setq-local electric-pair-mode nil)
          (ignore-errors (flyspell-mode 1))
          (visual-line-mode 1))
      (kill-local-variable 'truncate-lines)
      (kill-local-variable 'word-wrap)
      (kill-local-variable 'word-wrap-by-category)
      (kill-local-variable 'cursor-type)
      (kill-local-variable 'blink-cursor-interval)
      (kill-local-variable 'show-trailing-whitespace)
      ;; (kill-local-variable 'line-spacing)
      (kill-local-variable 'electric-pair-mode)
      (buffer-face-mode -1)
      (flyspell-mode -1)
      (visual-line-mode -1)
      (when (fboundp 'writeroom-mode)
        (writeroom-mode 0)))))


;; 参考centaur
;; PDF reader
(use-package pdf-tools
  :diminish (pdf-view-themed-minor-mode
             pdf-view-midnight-minor-mode
             pdf-view-printer-minor-mode)
  :defines pdf-annot-activate-created-annotations
  :hook ((pdf-tools-enabled . pdf-view-auto-slice-minor-mode)
         (pdf-tools-enabled . pdf-isearch-minor-mode))
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :bind (:map pdf-view-mode-map
              ("C-s" . isearch-forward))
  :init (setq pdf-view-use-scaling t
              pdf-view-use-imagemagick nil
              pdf-annot-activate-created-annotations t)
  :config
  ;; Activate the package
  (pdf-tools-install t nil t nil))


;; Recover last viewed position
(use-package saveplace-pdf-view
  :after pdf-tools
  :when (ignore-errors (pdf-info-check-epdfinfo) t)
  :autoload (saveplace-pdf-view-find-file-advice saveplace-pdf-view-to-alist-advice)
  :init
  (advice-add 'save-place-find-file-hook :around #'saveplace-pdf-view-find-file-advice)
  (advice-add 'save-place-to-alist :around #'saveplace-pdf-view-to-alist-advice))


;; Epub reader
(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :hook (nov-mode . my-nov-setup)
  :init
  (defun my-nov-setup ()
    "Setup `nov-mode' for better read experience."
    (visual-line-mode 1)
    (my/read-mode)
    (face-remap-add-relative 'variable-pitch :family "Times New Roman" :height 1.5))
  :config
  (with-no-warnings
    ;; WORKAROUND: errors while opening `nov' files with Unicode characters
    ;; @see https://github.com/wasamasa/nov.el/issues/63
    (defun my-nov-content-unique-identifier (content)
      "Return the the unique identifier for CONTENT."
      (let* ((name (nov-content-unique-identifier-name content))
             (selector (format "package>metadata>identifier[id='%s']"
                               (regexp-quote name)))
             (id (car (esxml-node-children (esxml-query selector content)))))
        (and id (intern id))))
    (advice-add #'nov-content-unique-identifier :override #'my-nov-content-unique-identifier)))


;; elfeed 暂时用得少，Centaur有很多关于这个的配置，需使用时可参考


;; 使用org-mode进行文档注释
;; 消耗启动时间，并且暂时未用到，先去掉
;; (use-package org-noter
;;   :init
;;   (setq-default org-noter-always-create-frame nil))

;; (use-package dictionary-overlay
;;   :demand t
;;   :ensure nil)

;; ~/.authinfo
;; 使用百度翻译api进行翻译，100W/月  -- 用量短信提醒
;; brew install translate-shell
;; 改为使用bing
(use-package immersive-translate
  :init
  ;; (setq immersive-translate-backend 'baidu
  ;;       immersive-translate-baidu-appid "20240604002069851")
  (setq immersive-translate-backend 'trans
        immersive-translate-trans-engine "bing")
  :config
  (add-hook 'elfeed-show-mode-hook #'immersive-translate-setup)
  (add-hook 'nov-pre-html-render-hook #'immersive-translate-setup))


;; 翻译
(use-package go-translate
  :bind ("C-c t" . gts-do-translate)
  :custom (gts-translate-list '(("en" "zh"))))


(provide 'init-read)

;;; init-read.el ends here
