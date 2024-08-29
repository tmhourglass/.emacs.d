;;; init-lsp.el --- lsp -*- lexical-binding: t -*-

;; Author: shaowen he

;;; Commentary:

;; commentary

;;; Code:

(define-derived-mode genehack-vue-mode web-mode "ghVue"
  "A major mode derived from web-mode, for editing .vue files with LSP support.")

(defun my-eglot-keybindgs ()
  (define-key evil-motion-state-map "gR" #'eglot-rename)
  (define-key evil-motion-state-map "gr" #'xref-find-references)
  (define-key evil-normal-state-map "gi" #'eglot-find-implementation)
  (define-key evil-motion-state-map "gh" #'eldoc)
  (define-key evil-normal-state-map "ga" #'eglot-code-actions))


;; 关于使用lsp-bridge替换eglot：发动有些大，需单独进行测试
;; 禁用其他补全插件 lsp-mode、eglot、company、corfu  -- 有其他补全插件，相互有引用，不方便直接禁用，暂时不用

;; 重新配置eglot，涉及flymake与flycheck，建议原生的flymake
(use-package eglot
  :init
  (advice-add 'eglot-ensure :after 'my-eglot-keybindgs)
  :bind (:map eglot-mode-map
              ("C-c l a" . eglot-code-actions)
              ("C-c l r" . eglot-rename)
              ("C-c l o" . eglot-code-action-organize-imports)
              ("C-c l f" . eglot-format)
              ("C-c l d" . eldoc)
              ("s-<return>" . eglot-code-actions))
  :custom (eglot-report-progress nil)
  :config
  (setq read-process-output-max (* 1024 1024)))


;; 使用eglot跳转到工作区符号并查阅
(use-package consult-eglot
  :after (consult eglot)
  :bind (:map eglot-mode-map ("M-g s" . consult-eglot-symbols)))

;; eglot面包屑：协助导航，可结合imenu跳转
;; 另外也支持文件路径显示：breadcrumb--header-line 在最上面有一行导航栏显示
;; 同样可显示文件中包的路径层级  -- 显示更丰富的路径
(use-package breadcrumb
  :config (breadcrumb-mode))


;; org代码块中支持的语言
(cl-defmacro eglot-org-babel-enable (lang)
  "Support LANG in org source code block."
  (cl-check-type lang string)
  (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
         (intern-pre (intern (format "eglot--%s" (symbol-name edit-pre)))))
    `(progn
       (defun ,intern-pre (info)
         (let ((file-name (->> info caddr (alist-get :file))))
           (unless file-name
             (setq file-name (concat default-directory (if (string= ,lang "C") "org-src-babel.c" "org-src-babel.cpp")))
             (write-region (point-min) (point-max) file-name))
           (setq buffer-file-name file-name)
           (eglot-ensure)))
       (put ',intern-pre 'function-documentation
            (format "Enable eglot mode in the buffer of org source block (%s)."
                    (upcase ,lang)))
       (if (fboundp ',edit-pre)
           (advice-add ',edit-pre :after ',intern-pre)
         (progn
           (defun ,edit-pre (info)
             (,intern-pre info))
           (put ',edit-pre 'function-documentation
                (format "Prepare local buffer environment for org source block (%s)."
                        (upcase ,lang))))))))


;; Jump to definition, used as a fallback of lsp-find-definition
(use-package dumb-jump
  :bind (("M-g j" . dumb-jump-go)
         ("M-g J" . dumb-jump-go-other-window))
  :custom
  (dumb-jump-quiet t)
  (dumb-jump-aggressive t)
  (dumb-jump-selector 'completing-read))



;; 以下配置已经在consult中进行设置，并进行了重新定义
;; (with-eval-after-load 'xref
;;   (setq xref-search-program 'ripgrep)     ;project-find-regexp
;;   (when (functionp 'xref-show-definitions-completing-read)
;;     (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
;;     (setq xref-show-xrefs-function #'xref-show-definitions-completing-read)))




(provide 'init-lsp)

;;; init-lsp.el ends here
