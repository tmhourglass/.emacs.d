;;; init-translate-region.el --- Configuration for translate-region -*- lexical-binding: t -*-

;; Author: shaowen he

;;; Commentary:

;; Configuration for translate-region - use-package版本

;;; Code:

(use-package translate-region
  :after gptel
  :commands (translate-region translate-region-zh-en translate-region-naming
                              translate-region-to-snake-case translate-region-to-pascal-case
                              translate-region-to-camel-case)
  :init
  ;; ==============================
  ;; 翻译后端设置
  ;; ==============================

  ;; 设置默认翻译后端
  ;; 选项: 'translate-shell 或 'gptel
  (setq translate-region-backend 'translate-shell)

  ;; ==============================
  ;; 命名风格设置
  ;; ==============================

  ;; 设置默认命名风格
  ;; 选项: 'camelCase, 'PascalCase, 'snake_case, 'kebab-case, 'SCREAMING_SNAKE_CASE
  (setq translate-region-naming-style 'camelCase)

  ;; ==============================
  ;; 高级设置
  ;; ==============================

  ;; 调整中文检测阈值（默认为 0.1）
  ;; 如果中文字符占比超过此值，则认为文本是中文
  ;; (setq translate-region-chinese-threshold 0.1)

  :config
  ;; ==============================
  ;; 代理设置
  ;; ==============================

  ;; translate-shell 已默认使用了代理

  ;; ==============================
  ;; 自定义便捷函数
  ;; ==============================

  ;; 创建函数，直接将选定文本翻译为特定命名风格
  (defun translate-region-to-snake-case ()
    "将选定区域翻译并转换为 snake_case 命名风格。"
    (interactive)
    (translate-region-naming 'snake_case))

  (defun translate-region-to-pascal-case ()
    "将选定区域翻译并转换为 PascalCase 命名风格。"
    (interactive)
    (translate-region-naming 'PascalCase))

  (defun translate-region-to-camel-case ()
    "将选定区域翻译并转换为 camelCase 命名风格。"
    (interactive)
    (translate-region-naming 'camelCase))

  ;; ==============================
  ;; 额外的便捷函数
  ;; ==============================

  (defun my/translate-region-to-kebab-case ()
    "将选定区域翻译并转换为 kebab-case 命名风格。"
    (interactive)
    (translate-region-naming 'kebab-case))

  (defun my/translate-region-to-screaming-snake-case ()
    "将选定区域翻译并转换为 SCREAMING_SNAKE_CASE 命名风格。"
    (interactive)
    (translate-region-naming 'SCREAMING_SNAKE_CASE))

  ;; 快捷键设定
  (+general-global-menu! "trans-region" "tr"
    "t" 'translate-region-zh-en
    "b" 'translate-region-toggle-backend
    "n" 'translate-region-naming
    "i" 'translate-region-naming-interactive
    "s" 'translate-region-to-snake-case
    "p" 'translate-region-to-pascal-case
    "k" 'my/translate-region-to-kebab-case
    "S" 'my/translate-region-to-screaming-snake-case
    "c" 'translate-region-to-camel-case)


  ;; ==============================
  ;; 按键绑定
  ;; ==============================

  :bind (("C-c t r" . translate-region-zh-en)
         ;; 新增一个快捷键
         ("s-." . translate-region-zh-en))
  )


(provide 'init-translate-region)

;;; init-translate-region.el ends here
