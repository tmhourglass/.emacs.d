;;; init.el --- init -*- lexical-binding: t -*-

;; Author: shaowen he

;;; Commentary:

;; User init file. Layered loading for fast startup.

;;; Code:

(when (version< emacs-version "30.1")
  (error "This requires Emacs 30.1 and above!"))


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


;; use-package (Emacs 30.1 已内置，仅配置变量)
(eval-and-compile
  (setq use-package-enable-imenu-support t)
  (setq use-package-expand-minimally t)
  (setq use-package-compute-statistics t)
  (setq use-package-always-defer t))


;; 优化启动时间
;; 必须放在最前面才能收集到（use-package之后）
(use-package benchmark-init
  :demand t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))


;; ═══════════════════════════════════════════
;; 第0层：核心基础（同步加载 — 无外部依赖）
;; ═══════════════════════════════════════════
(require 'init-const)
(require 'init-funcs)
(require 'init-funcs-org)
(require 'init-funcs-edit)
(require 'init-generic)

;; ═══════════════════════════════════════════
;; 第1层：编辑与补全（同步加载 — 交互必需）
;; ═══════════════════════════════════════════
(require 'init-basic)
(require 'init-evil)
(require 'init-completion)

;; ═══════════════════════════════════════════
;; 第2层：UI 界面（同步加载 — 视觉必需）
;; ═══════════════════════════════════════════
(require 'init-font)
(require 'init-ui)
(require 'init-window)
(require 'init-dashboard)
(require 'init-theme-switch)

;; ═══════════════════════════════════════════
;; 第3层：按键体系（同步加载 — 用户可感知）
;; ═══════════════════════════════════════════
(require 'init-global-keys)
(require 'init-general-keys)
(require 'init-tools)
(require 'init-snippets)

;; ═══════════════════════════════════════════
;; 第4层：Org 与 编程模块（after-init-hook）
;; ═══════════════════════════════════════════
(add-hook 'after-init-hook
          (lambda ()
            (require 'init-org-core)
            (require 'init-org-agenda)
            (require 'init-org-roam)
            (require 'init-org-export)
            (require 'init-org-appearance)
            (require 'init-journal)
            (require 'init-lsp)
            (require 'init-lisp)
            (require 'init-python)
            (require 'init-programming)
            (require 'init-syntaxcheck)
            (require 'init-ctags)))

;; ═══════════════════════════════════════════
;; 第5层：非关键模块（idle-timer 后台加载）
;; ═══════════════════════════════════════════
(run-with-idle-timer 2 nil
                     (lambda ()
                       (require 'init-git)
                       (require 'init-tabspace)
                       (require 'init-dired)
                       (require 'init-write)
                       (require 'init-read)
                       (require 'init-dict)
                       (require 'init-translate-region)
                       ;; AI 模块进一步延迟
                       (run-with-idle-timer 5 nil
                                            (lambda ()
                                              (require 'init-aidermacs)
                                              (require 'init-gptel)))))

;;; init.el ends here
