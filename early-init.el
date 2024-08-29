;;; early-init.el --- earliest birds               -*- lexical-binding: t -*-

;;; early-init.el --- earliest birds -*- lexical-binding: t -*-

;; Author: shaowen he

;;; Commentary:

;; Emacs 27 introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.

;;; Code:

(require 'cl-lib)

;; 总是加载文件的最新版本
(setq load-prefer-newer t)
;; (setq load-prefer-newer noninteractive)

;; 优化加载自动编译包：在保存和加载时自动编译
;; (add-to-list 'load-path
;;              (expand-file-name
;;               "lib/auto-compile"
;;               (file-name-directory (or load-file-name buffer-file-name))))
;; (require 'auto-compile)
;; (auto-compile-on-load-mode)
;; (auto-compile-on-save-mode)

;; 启动期间延迟垃圾收集，启动后需恢复
;; (setq gc-cons-threshold most-positive-fixnum)

;; 禁用本地编译
(setq native-comp-deferred-compilation nil ;; obsolete since 29.1
      native-comp-jit-compilation nil)

;; 禁止过早执行 `package-initialization'
(setq package-enable-at-startup nil)


;; 设置package源 -- 用得不多，后续再看需不需要更换国内源
;; (with-eval-after-load 'package
;;   (add-to-list 'package-archives
;;                (cons "melpa" "https://melpa.org/packages/")
;;                t))



;; 指定编码为utf-8  -- macOS上不需要
;; (prefer-coding-system 'utf-8)

;; Inhibit resizing frame
;; 禁止框架调整，加速启动，不要缩放frame
(setq frame-inhibit-implied-resize t)

;; Faster to disable these here (before they've been initialized)
;; 菜单栏的禁用，在macOS上不起作用，考虑注释掉
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; 把标题栏设置为透明，仅在macOS上可用 -- 有效
;; borg配置中不生效，不知为何
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))

;; Speed Up Startup
;; Prevent flashing of unstyled modeline at startup
(setq-default mode-line-format nil)


;;; early-init.el ends here
