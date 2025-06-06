;;; init-theme-switch.el --- theme-switch -*- lexical-binding: t -*-

;; Author: shaowen he

;;; Commentary:

;; Theme Switch

;;; Code:

(require 'theme-switch)

;;; 主题设置
;; 设置喜欢的主题列表 （可自定义-custom）
(setq theme-switch-favorite-themes '(sanityinc-solarized-dark
                                     sanityinc-solarized-light
                                     doom-oksolar-dark
                                     doom-oksolar-light
                                     doom-monokai-pro
                                     doom-tomorrow-day
                                     doom-tomorrow-night
                                     sanityinc-tomorrow-day
                                     sanityinc-tomorrow-night
                                     sanityinc-tomorrow-eighties
                                     ))

;; 设置不想使用的主题 （可自定义-custom）
(setq theme-switch-excluded-themes '(adwaita))

;;; 护眼模式设置
;; 设置日间模式主题列表（亮色主题） - 需在此处修改
(setq theme-switch-day-themes '(tango
                                leuven
                                sanityinc-solarized-light
                                sanityinc-tomorrow-day
                                modus-operandi
                                doom-oksolar-light
                                ))

;; 设置夜间模式主题列表（暗色主题） - 需在此处修改
(setq theme-switch-night-themes '(tango-dark
                                  zenburn
                                  sanityinc-solarized-dark
                                  sanityinc-tomorrow-bright
                                  sanityinc-tomorrow-eighties
                                  modus-vivendi
                                  doom-monokai-pro
                                  doom-one
                                  doom-tomorrow-night
                                  doom-oksolar-dark
                                  ))

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
