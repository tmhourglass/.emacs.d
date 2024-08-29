;;; init-snippets.el --- snippet -*- lexical-binding: t -*-

;; Author: shaowen he

;;; Commentary:

;; commentary

;;; Code:

;; 同类插件：tempel/eglot-tempel/tempel-collection


;; Emacs的模板系统，片段语法，不捆绑片段
;; yas-new-snippet 创建代码片段，放置在~/.emacs.d/snippets
;; yas-reload-all
(use-package yasnippet
  :diminish yas-minor-mode
  :custom (yas-keymap-disable-hook
           (lambda () (and (frame-live-p corfu--frame)
                           (frame-visible-p corfu--frame))))
  :hook (after-init . yas-global-mode)
  :config
  (progn
    (setq hippie-expand-try-functions-list
          '(yas/hippie-try-expand
            try-complete-file-name-partially
            try-expand-all-abbrevs
            try-expand-dabbrev
            try-expand-dabbrev-all-buffers
            try-expand-dabbrev-from-kill
            try-complete-lisp-symbol-partially
            try-complete-lisp-symbol))))


;; 使用consult来预览访问snippet
;; 快捷键 M-*
(use-package consult-yasnippet
  :bind ("M-*" . consult-yasnippet)
  :config
  (with-eval-after-load 'embark
    (defvar-keymap embark-yasnippet-completion-actions
      :doc "Keymap for actions for yasnippets."
      :parent embark-general-map
      "v" #'consult-yasnippet-visit-snippet-file)
    (push '(yasnippet . embark-yasnippet-completion-actions)
          embark-keymap-alist)))

;; snippet集合，代码片段集合包
(use-package yasnippet-snippets
  :after yasnippet)


;; Yasnippet Completion At Point Function
;; 使用capf完成yasnippet片段
(use-package yasnippet-capf
  :init (add-to-list 'completion-at-point-functions #'yasnippet-capf))


(provide 'init-snippets)

;;; init-snippets.el ends here
