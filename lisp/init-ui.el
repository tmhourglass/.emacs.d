;;; init-ui.el --- ui -*- lexical-binding: t -*-

;; Author: shaowen he

;;; Commentary:

;; commentary


;;; Code:

;; 使用doom-one主题
;; doom-one / doom-tomorrow-night
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t))


;; macos中设置标题栏匹配当前系统主题
(use-package ns-auto-titlebar
  :when sys/macp
  :config (ns-auto-titlebar-mode))



(use-package valign
  :hook ((markdown-mode org-mode) . valign-mode))


(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-minor-modes t)
  :custom-face
  (mode-line ((t (:height 0.95))))
  (mode-line-inactive ((t (:height 0.95)))))

(use-package hide-mode-line
  :hook (((treemacs-mode
           eshell-mode shell-mode
           term-mode vterm-mode
           embark-collect-mode
           lsp-ui-imenu-mode
           pdf-annot-list-mode) . turn-on-hide-mode-line-mode)
         (dired-mode . (lambda()
                         (and (bound-and-true-p hide-mode-line-mode)
                              (turn-off-hide-mode-line-mode))))))

;; A minor-mode menu for mode-line
;; 嵌套菜单，将所有次要模式收入右下角的齿轮图标中，点击后显示
;; 避免像之前一样全部显示出来，又显示不全，占用空间
(use-package minions
  :hook (doom-modeline-mode . minions-mode))

;; Icons
(use-package nerd-icons
  :config
  (when (and (display-graphic-p)
             (not (font-installed-p nerd-icons-font-family)))
    (nerd-icons-install-fonts t)))


;; Show line numbers
;; 显示行号设置：针对特定模式显示，也可使用快捷键显示
(use-package display-line-numbers
  :hook ((prog-mode yaml-mode conf-mode) . display-line-numbers-mode)
  :init (setq display-line-numbers-width-start t))


;; Suppress GUI features
;; (setq inhibit-startup-echo-area-message user-login-name
;;       inhibit-default-init t)
;; (unless (daemonp)
;;   (advice-add #'display-startup-echo-area-message :override #'ignore))

;; Display dividers between windows
;; (setq window-divider-default-places t
;;       window-divider-default-bottom-width 1
;;       window-divider-default-right-width 1)
;; (add-hook 'window-setup-hook #'window-divider-mode)


;; Mouse & Smooth Scroll
;; Scroll one line at a time (less jumpy" than defaults)
(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . hscroll))
        mouse-wheel-scroll-amount-horizontal 1
        mouse-wheel-progressive-speed nil))
(setq scroll-step 1
      scroll-margin 0
      scroll-conservatively 100000
      auto-window-vscroll nil
      scroll-preserve-screen-position t)

;; Good pixel line scrolling
(if (fboundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode t)
  (unless sys/macp
    (use-package good-scroll
      :diminish
      :hook (after-init . good-scroll-mode)
      :bind (([remap next] . good-scroll-up-full-screen)
             ([remap prior] . good-scroll-down-full-screen)))))

;; Use fixed pitch where it's sensible
(use-package mixed-pitch
  :diminish)

;; Display ugly ^L page breaks as tidy horizontal lines
(use-package page-break-lines
  :diminish
  :hook (after-init . global-page-break-lines-mode))

(with-no-warnings
  (when sys/macp
    ;; Render thinner fonts
    (setq ns-use-thin-smoothing t)
    ;; Don't open a file in a new frame
    (setq ns-pop-up-frames nil)))



(use-package visual-fill-column
  :init
  ;; Configure fill width
  (setq visual-fill-column-width 110
        visual-fill-column-center-text t))


;; 再也不会丢失光标：在移动时，光标顶部会闪烁一盏灯，显示光标位置
(use-package beacon
  :custom
  (beacon-lighter "")
  (beacon-size 25)
  (beacon-color "green")
  (beacon-blink-when-window-scrolls nil)
  :hook (after-init . beacon-mode))


;; 在 Emacs 中突出显示转义序列
(use-package highlight-escape-sequences
  :hook (after-init . hes-mode))


;; 使用彩虹猫来显示当前位置百分比 （可单击该区域来滚动）
;; 可定义窗口宽度，当多窗口下，窗口太窄，会直接禁用掉不显示
;; 放在init中才行
(use-package nyan-mode
  :init
  (setq nyan-animate-nyancat t
        nyan-wavy-trail t)
  :hook (after-init . nyan-mode))



(provide 'init-ui)

;;; init-ui.el ends here
