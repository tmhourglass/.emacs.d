;;; init-theme-switch.el --- theme-switch -*- lexical-binding: t -*-

;; Author: shaowen he

;;; Commentary:

;; Theme Switch - use-package版本

;;; Code:

(use-package theme-switch
  :defer t                              ; 延迟加载以提升启动速度
  :commands (theme-switch-mode theme-switch-auto theme-switch-menu)
  :init
  ;; 主题设置 - 在包加载前设置变量
  ;; 设置喜欢的主题列表（可自定义）
  (setq theme-switch-favorite-themes '(sanityinc-solarized-dark
                                       sanityinc-solarized-light
                                       doom-oksolar-dark
                                       doom-oksolar-light
                                       doom-monokai-pro
                                       doom-tomorrow-day
                                       doom-tomorrow-night
                                       sanityinc-tomorrow-day
                                       sanityinc-tomorrow-night
                                       sanityinc-tomorrow-eighties))

  ;; 设置不想使用的主题（可自定义）
  (setq theme-switch-excluded-themes '(adwaita))

  ;; 护眼模式设置
  ;; 设置日间模式主题列表（亮色主题）
  (setq theme-switch-day-themes '(tango
                                  leuven
                                  sanityinc-solarized-light
                                  sanityinc-tomorrow-day
                                  modus-operandi
                                  doom-oksolar-light))

  ;; 设置夜间模式主题列表（暗色主题）
  (setq theme-switch-night-themes '(tango-dark
                                    zenburn
                                    sanityinc-solarized-dark
                                    sanityinc-tomorrow-bright
                                    sanityinc-tomorrow-eighties
                                    modus-vivendi
                                    doom-monokai-pro
                                    doom-one
                                    doom-tomorrow-night
                                    doom-oksolar-dark))

  ;; 设置日间/夜间模式的时间
  (setq theme-switch-day-start "06:30"
        theme-switch-night-start "18:30")

  ;; 启用自动切换
  (setq theme-switch-auto-switch-enabled t
        theme-switch-auto-switch-interval 1800) ; 30分钟检查一次

  :config
  ;; 启用主题切换模式
  (theme-switch-mode 1)

  ;; 启动时根据当前时间自动选择模式
  (theme-switch-auto)

  ;; 可选：添加一些便捷函数
  (defun my/toggle-theme-mode ()
    "快速切换日间/夜间模式."
    (interactive)
    (if (member (car custom-enabled-themes) theme-switch-day-themes)
        (theme-switch-night)
      (theme-switch-day)))

  ;; 快捷键设定
  (+general-global-menu! "theme-switch" "tt"
    "r" 'theme-switch-random-favorites
    "R" 'theme-switch-random-available
    "p" 'theme-switch-previous
    "l" 'theme-switch-load-theme
    "e" 'theme-switch-toggle-eye-care
    "d" 'theme-switch-day-mode
    "n" 'theme-switch-night-mode
    "a" 'theme-switch-toggle-auto-switch
    "+" 'theme-switch-add-to-favorites
    "-" 'theme-switch-remove-from-favorites
    "f" 'theme-switch-list-favorites
    "]" 'theme-switch-add-to-excluded
    "[" 'theme-switch-remove-from-excluded
    "x" 'theme-switch-list-excluded
    "v" 'theme-switch-preview
    "m" 'theme-switch-menu)


  ;; 主题切换菜单
  :bind ("s-\\" . theme-switch-menu)
  )

(provide 'init-theme-switch)

;;; init-theme-switch.el ends here
