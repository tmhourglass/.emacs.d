;;; init-rime.el --- rime -*- lexical-binding: t -*-

;; Author: shaowen he

;;; Commentary:

;; Configuration for emacs-rime.

;;; Code:

(use-package rime
  :defer t
  :init
  ;; ── 路径配置 ──
  ;; 共享 Squirrel（鼠须管）的用户数据目录
  (setq rime-user-data-dir (expand-file-name "~/Library/Rime"))
  ;; Apple Silicon Homebrew 路径
  (setq rime-librime-root "/opt/homebrew")
  (setq rime-emacs-module-header-root "/opt/homebrew/bin/emacs/include")

  ;; ── 候选窗口配置 ──
  (setq rime-show-candidate 'posframe)
  (setq rime-posframe-properties
        (list :background-color "#333333"
              :foreground-color "#dcdccc"
              :internal-border-width 10))

  ;; 设为默认输入法
  (setq default-input-method "rime")

  :config
  ;; ── 激活模式下快捷键 ──
  ;; M-o: 退格删除 | M-m: 确认上屏 | M-h: 取消输入
  (define-key rime-active-mode-map (kbd "M-o") 'rime--backspace)
  (define-key rime-active-mode-map (kbd "M-m") 'rime--return)
  (define-key rime-active-mode-map (kbd "M-h") 'rime--escape))

(provide 'init-rime)
;;; init-rime.el ends here
