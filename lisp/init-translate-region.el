;;; init-translate-region.el --- Configuration for translate-region -*- lexical-binding: t -*-

;; Author: shaowen he

;;; Commentary:

;; Configuretion for translate-region

;;; Code:

(require 'translate-region)

;; ==============================
;; 翻译后端设置
;; ==============================

;; 设置默认翻译后端
;; 选项: 'translate-shell 或 'gptel
(setq translate-region-backend 'translate-shell)

;; gptel使用默认的，即本地配置的,见init-gptel.el

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


;; ==============================
;; 代理设置
;; ==============================

;; translate-shell 已默认使用了代理

;; 创建一个函数，直接将选定文本翻译为特定命名风格
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


(provide 'init-translate-region)

;;; init-translate-region.el ends here
