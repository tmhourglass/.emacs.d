;;; init-basic.el --- basic -*- lexical-binding: t -*-

;; Author: shaowen he

;;; Commentary:

;; 基本设置

;;; Code:

;; Elisp兼容库
(use-package compat)

;; 现代库列表API，类似cl-lib （提供很多Emacs可使用的库函数）
;; 库中的所有函数和构造都使用破折号（ - ）前缀
;; 部分函数的宏版本，使用两个破折号（ -- ）为前缀
;; -map 将函数应用于每个元素 --map 使用it临时变量存储当前元素（即可直接使用）
(use-package dash
  :hook (after-init . global-dash-fontify-mode))

;; 使用 EmacSQL 存储 EIEIO 对象，sqlite作为后端
;; 关联包closql
(use-package eieio)


;; Keep ~/.emacs.d/ clean.
;; 不乱丢垃圾，统一放到一处 /etc /var
(use-package no-littering)

;; custom文件加载
(use-package custom
  :no-require t
  :config
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (when (file-exists-p custom-file)
    (load custom-file)))

;; 自动编译
(use-package auto-compile
  :config
  (setq auto-compile-display-buffer               nil)
  (setq auto-compile-mode-line-counter            t)
  (setq auto-compile-source-recreate-deletes-dest t)
  (setq auto-compile-toggle-deletes-nonlib-dest   t)
  (setq auto-compile-update-autoloads             t))

;; epkg
(use-package epkg
  :defer 1
  :init
  (setq epkg-repository
        (expand-file-name "var/epkgs/" user-emacs-directory))
  (setq epkg-database-connector
        (if (>= emacs-major-version 29) 'sqlite-builtin 'sqlite-module)))


;; 高亮未提交的变更，可明显看到有变化的地方 -- 编辑内容中也有提示
(use-package diff-hl
  :config
  (setq diff-hl-draw-borders nil)
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh t))


(use-package diff-mode
  :defer 1
  :config
  (when (>= emacs-major-version 27)
    (set-face-attribute 'diff-refine-changed nil :extend t)
    (set-face-attribute 'diff-refine-removed nil :extend t)
    (set-face-attribute 'diff-refine-added   nil :extend t)))


;;; Isearch settings
;; 使用isearch搜索时，会让光标始终在中间
(use-package isearch
  :custom
  (isearch-lazy-count t)
  (isearch-allow-motion t)
  (isearch-motion-changes-direction t))


;; diff3简化编辑输出  -- 未使用到
;; (use-package smerge-mode
;;   :defer t
;;   :config
;;   (when (>= emacs-major-version 27)
;;     (set-face-attribute 'smerge-refined-removed nil :extend t)
;;     (set-face-attribute 'smerge-refined-added   nil :extend t)))



;; 启动server模式 -- 基本不用，emacsclient效果不好
;; (use-package server
;;   :functions (server-running-p)
;;   :config (or (server-running-p) (server-mode)))


;; with-no-warnings
;; 后台进行垃圾收集，减少对用户活动的干扰 （空闲5s）
(use-package gcmh
  :diminish nil
  :init
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold #x1000000) ; 16MB
  :hook (after-init . gcmh-mode))


;; Environment 环境变量引入
(use-package exec-path-from-shell
  :when (or (memq window-system '(mac ns x))
            (unless (memq system-type '(ms-dos windows-nt))
              (daemonp)))
  :custom (exec-path-from-shell-arguments '("-l"))
  :config
  (dolist (var '("GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))


;; History
(use-package saveplace
  :hook (after-init . save-place-mode)
  :init
  (setq save-place-file (no-littering-expand-etc-file-name "places")))


;; 最近文件列表
(use-package recentf
  :bind (("C-x C-r" . recentf-open-files))
  :hook (after-init . recentf-mode)
  :init (setq recentf-max-saved-items 300
              recentf-exclude
              '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
                "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
                "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
                "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"
                (lambda (file) (file-in-directory-p file package-user-dir))))
  :config
  (push (expand-file-name recentf-save-file) recentf-exclude)
  (add-to-list 'recentf-filename-handlers #'abbreviate-file-name))


;; 修改缓存路径
(use-package savehist
  :hook (after-init . savehist-mode)
  :init (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
              history-length 500
              savehist-file (no-littering-expand-etc-file-name "history")
              savehist-additional-variables '(mark-ring
                                              global-mark-ring
                                              search-ring
                                              regexp-search-ring
                                              extended-command-history)
              savehist-autosave-interval 300))


;; simple 标准库
(use-package simple
  :hook ((after-init . size-indication-mode)
         (text-mode . visual-line-mode)
         ((prog-mode markdown-mode conf-mode) . enable-trailing-whitespace))
  :init
  (setq column-number-mode nil
        line-number-mode nil
        ;; kill-whole-line t               ; Kill line including '\n'
        line-move-visual nil
        track-eol t ; Keep cursor at end of lines. Require line-move-visual is nil.
        set-mark-command-repeat-pop t) ; Repeating C-SPC after popping mark pops it again

  ;; Visualize TAB, (HARD) SPACE, NEWLINE
  (setq-default show-trailing-whitespace nil)

  ;; Don't show trailing whitespace by default
  ;; 保存时删除空白
  (defun enable-trailing-whitespace ()
    "Show trailing spaces and delete on saving."
    (setq show-trailing-whitespace t)
    (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)))


;; 内置包，显示时间
;; (use-package time
;;   :config
;;   (setq display-time-24hr-format t
;;         display-time-day-and-date t))


;; Misc
(setq-default major-mode 'text-mode
              fill-column 80
              tab-width 4
              indent-tabs-mode nil)     ; Permanently indent with spaces, never with TABs

(setq visible-bell t
      inhibit-compacting-font-caches t  ; Don’t compact font caches during GC.
      delete-by-moving-to-trash t       ; Deleting files go to OS's trash folder
      uniquify-buffer-name-style 'post-forward-angle-brackets ; Show path if names are same
      adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*"
      adaptive-fill-first-line-regexp "^* *$"
      sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
      sentence-end-double-space nil)


;; 简化Tar文件编辑  -- 没用过
(use-package tar-mode)


;; 快速重启
(use-package restart-emacs
  :commands (restart-emacs))


(use-package general
  :init
  (with-eval-after-load 'evil
    (general-add-hook 'after-init-hook
                      (lambda (&rest _)
                        (when-let ((messages-buffer (get-buffer "*Messages*")))
                          (with-current-buffer messages-buffer
                            (evil-normalize-keymaps))))
                      nil
                      nil
                      t))


  (general-create-definer global-definer
    :keymaps 'override
    :states '(insert emacs normal hybrid motion visual operator)
    :prefix "SPC"
    :non-normal-prefix "C-SPC")

  (defmacro +general-global-menu! (name infix-key &rest body)
    "Create a definer named +general-global-NAME wrapping global-definer.
Create prefix map: +general-global-NAME. Prefix bindings in BODY with INFIX-KEY."
    (declare (indent 2))
    `(progn
       (general-create-definer ,(intern (concat "+general-global-" name))
         :wrapping global-definer
         :prefix-map ',(intern (concat "+general-global-" name "-map"))
         :infix ,infix-key
         :wk-full-keys nil
         "" '(:ignore t :which-key ,name))
       (,(intern (concat "+general-global-" name))
        ,@body)))

  (general-create-definer global-leader
    :keymaps 'override
    :states '(emacs normal hybrid motion visual operator)
    :prefix ","
    "" '(:ignore t :which-key (lambda (arg) `(,(cadr (split-string (car arg) " ")) . ,(replace-regexp-in-string "-mode$" "" (symbol-name major-mode)))))))


;; (use-package paren
;;   :config (show-paren-mode))


;; smartparens 处理括号对并使其智能化
;; 从lisp模式中去掉特殊几个配对，针对C模式特殊处理
(use-package smartparens
  :init
  (smartparens-global-mode t)
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'emacs-lisp-mode "`" nil :actions nil)
  (sp-local-pair 'lisp-interaction-mode "'" nil :actions nil)
  :config
  (sp-with-modes
      '(c++-mode objc-mode c-mode)
    (sp-local-pair "{" nil :post-handlers '(:add ("||\n[i]" "RET")))))


;; 根据嵌套层级以不同颜色显示括号
(use-package highlight-parentheses
  :init
  (setq hl-paren-colors '("DarkOrange" "DeepSkyBlue" "DarkRed"))
  :hook ((find-file-hook . highlight-parentheses-mode)
         (prog-mode . highlight-parentheses-mode)))


;; 激进缩进模式，比electric-indent-mode更可靠
;; 在调整块时，可始终保持缩进，可指定需要的模式，也可全局
;; 可从全局中排除某个模式 aggresive-indent-excluded-modes
(use-package aggressive-indent
  :hook ((emacs-lisp-mode . aggressive-indent-mode)
         (python-mode . aggressive-indent-mode)))



(use-package hungry-delete
  :hook (after-init . global-hungry-delete-mode))


;; 使用direnv来设置每个目录/项目的环境变量，若存在.envrc，缓冲区局部变量
(use-package envrc
  :bind-keymap ("C-c e" . envrc-command-map)
  :hook (after-init . envrc-global-mode))


;; 替换内置的undo-tree：在右边显示
;; 可视化undo tree：在下边显示，支持更多的操作，有很多快捷键
;; https://github.com/casouri/vundo
;; 快捷键：f/b/n/p/a/w/e/l/r/m/u/d/q/C-c C-s
(use-package vundo
  :bind ("C-x u" . vundo)
  :custom (vundo-roll-back-on-quit nil))

;; (use-package undo-tree
;;   :diminish
;;   :init
;;   (global-undo-tree-mode 1)
;;   (setq undo-tree-auto-save-history nil)
;;   (evil-set-undo-system 'undo-tree))


;; 正则表达式实时反馈
;; Build regexp with visual feedback
(use-package re-builder
  :commands re-builder
  :bind (:map reb-mode-map
              ("C-c C-k" . reb-quit)
              ("C-c C-p" . reb-prev-match)
              ("C-c C-n" . reb-next-match))
  :custom
  (reb-re-syntax 'string))


;; Workaround with minified source files
;; (use-package so-long
;;   :hook (after-init . global-so-long-mode))



(provide 'init-basic)

;;; init-basic.el ends here
