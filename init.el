;;; init.el --- init -*- lexical-binding: t -*-

;; Author: shaowen he

;;; Commentary:

;; User init file

;;; Code:

(when (version< emacs-version "29.1")
  (error "This requires Emacs 29.1 and above!"))


;; 垃圾回收
;; gc-cons-threshold  gc-cons-percentage
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))


;; Load path -- 加载配置文件目录
(add-to-list 'load-path
             (expand-file-name "lisp" user-emacs-directory))


;;
;; Speed Up Startup
;;
;; Don't pass case-insensitive to `auto-mode-alist'
(setq auto-mode-case-fold nil)

;; 清空避免加载远程文件的时候分析文件
(unless (or (daemonp) noninteractive init-file-debug)
  ;; Suppress file handlers operations at startup
  ;; `file-name-handler-alist' is consulted on each call to `require' and `load'
  (let ((old-value file-name-handler-alist))
    (setq file-name-handler-alist nil)
    (set-default-toplevel-value 'file-name-handler-alist file-name-handler-alist)
    (add-hook 'emacs-startup-hook
              (lambda ()
                "Recover file name handlers."
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist old-value))))
              101)))


;; 先单独加载borg，并进行初始化
(eval-and-compile                       ; `borg'
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require 'borg)
  (borg-initialize))


(eval-and-compile ; `use-package'
  (setq use-package-enable-imenu-support t)
  (setq use-package-expand-minimally t)
  (setq use-package-compute-statistics t)
  (require  'use-package))

;; 优化启动时间
;; 必须放在最前面才能收集到（use-package之后）
(use-package benchmark-init
  :demand t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))



;; ========================================
;;; Private Configuration
;; ========================================


;; 常量及函数定义
(require 'init-const)
(require 'init-funcs)

;; emacs内置变量配置，不依赖于package
(require 'init-generic)

;; Core
(require 'init-basic)
(require 'init-completion)
(require 'init-evil)

;; uis
(require 'init-ui)
(require 'init-font)
(require 'init-dashboard)
(require 'init-window)
(require 'init-dired)
;; (require 'init-dirvish)

;; Tools
(require 'init-org)
(require 'init-git)
(require 'init-ctags)
(require 'init-syntaxcheck)
(require 'init-write)
(require 'init-read)

;; Frameworks
(require 'init-tabspace)

;; ai -- 基本不用（界面广告太多，可考虑转到这来使用，随时查询）
;; (require 'init-ai)

(require 'init-global-keys)

;; 用得少且耗时长的，延迟加载，按键两部分拆分开
(run-with-idle-timer
 1 nil
 #'(lambda ()
     (require 'init-tools)
     (require 'init-snippets)
     ;; 翻译
     (require 'init-dict)
     ;; Programming
     (require 'init-lsp)
     (require 'init-lisp)
     (require 'init-python)
     (require 'init-programming)
     (require 'init-general-keys)))


;;; init.el ends here
