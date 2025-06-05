;;; init-theme-switch.el --- theme-switch -*- lexical-binding: t -*-

;; Author: shaowen he

;;; Commentary:

;; Theme Switch

;;; Code:

(require 'theme-switch)

;;; 主题设置
;; 设置喜欢的主题列表
(setq theme-switch-favorite-themes '(solarized-dark solarized-light leuven modus-vivendi modus-operandi))

;; 设置不想使用的主题
(setq theme-switch-excluded-themes '(adwaita))

;;; 护眼模式设置
;; 设置日间模式主题列表（亮色主题）
(setq theme-switch-day-themes '(leuven solarized-light modus-operandi))

;; 设置夜间模式主题列表（暗色主题）
(setq theme-switch-night-themes '(zenburn solarized-dark modus-vivendi))

;; 设置日间/夜间模式的时间
(setq theme-switch-day-start "06:30")
(setq theme-switch-night-start "18:30")

;; 启用自动切换
(setq theme-switch-auto-switch-enabled t)
(setq theme-switch-auto-switch-interval 1800) ;; 30分钟检查一次


;;; 启用主题切换模式
(theme-switch-mode 1)

;;; 启动时根据当前时间自动选择模式（可选）
(theme-switch-auto)

(provide 'init-theme-switch)

;;; init-theme-switch.el ends here
