;;; init-generic.el --- generic -*- lexical-binding: t -*-

;; Author: shaowen he

;;; Commentary:

;; commentary

;;; Code:


;;; Code:

;; 通用设置

;; Restore emacs session.
;; (setq initial-buffer-choice t)
;; (run-with-timer 1 nil #'(lambda () (bury-buffer)))


;; 按键修改 meta/super
(cond
 (sys/win32p
  ;; make PC keyboard's Win key or other to type Super or Hyper
  ;; (setq w32-pass-lwindow-to-system nil)
  (setq w32-lwindow-modifier 'super   ; Left Windows key
        w32-apps-modifier 'super)     ; Menu/App key
  (w32-register-hot-key [s-t]))
 (sys/macp
  ;; Compatible with Emacs Mac port
  (setq mac-option-modifier 'meta
        mac-command-modifier 'super)))


(setq package-check-signature nil)      ;安装包时不检查签名
(setq auto-mode-case-fold nil)          ;加速启动
(setq system-time-locale "C")           ;星期格式由中文改为英文
(setq command-line-ns-option-alist nil) ;针对大文件及IO的优化
(setq read-process-output-max (* 1024 1024)) ;增加chunk，默认4k，加到64kb
;; (set-default-coding-systems 'utf-8)     ;默认utf-8编码 -- 系统默认不需设置
(setq process-adaptive-read-buffering nil) ;增加IO性能
(setq read-process-output-max (* 1024 1024)) ;增加IO性能
;; (fset 'yes-or-no-p 'y-or-n-p)           ;以 y/n代表 yes/no
(setq use-short-answers t)              ;以 y/n代表 yes/no
(blink-cursor-mode -1)                  ;指针不闪动
(transient-mark-mode 1)                 ;标记高亮
(global-subword-mode 1)                 ;Word移动支持 FooBar 的格式
(setq use-dialog-box nil)               ;never pop dialog
(setq use-file-dialog nil)              ;never pop dialog
(setq inhibit-startup-screen t)         ;禁止开始屏幕，加速启动
;; (setq-default cursor-type 'bar)         ;设置光标类型

;; (setq initial-scratch-message "") ;关闭启动空白buffer, 这个buffer会干扰session恢复
(setq initial-scratch-message
      (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n\n"))

(setq-default comment-style 'indent)    ;设定自动缩进的注释风格
(setq ring-bell-function 'ignore)       ;关闭烦人的出错时的提示声 (macos中会在屏幕中间出现警告的标志，然后消失）
(setq mouse-yank-at-point t)            ;粘贴于光标处,而不是鼠标指针处
(setq select-enable-clipboard t)        ;支持emacs和外部程序的粘贴
(setq split-width-threshold nil)        ;分屏的时候使用上下分屏
(setq inhibit-compacting-font-caches t) ;使用字体缓存，避免卡顿
(setq confirm-kill-processes nil)       ;退出自动杀掉进程
(setq word-wrap-by-category t)          ;按照中文折行
(setq completion-auto-select nil)       ;避免默认自动选择
(setq ad-redefinition-action 'accept)   ;不要烦人的 redefine warning
(setq frame-resize-pixelwise t) ;设置缩放的模式,避免Mac平台最大化窗口以后右边和下边有空隙
(setq save-abbrevs nil)                 ;文件保存时不保存单词缩写
(setq-default abbrev-mode t)            ;打开缩写模式 - 全局
(setq find-program "fd")                ;设置查找程序
(setq recenter-positions '(top middle bottom)) ;最近位置循环顺序
(setq create-lockfiles nil)             ;不使用lockfile
(setq show-paren-mode t)                ;显示括号模式（似乎有重复）
(global-hl-line-mode 1)                 ;高亮当前行（与beacon重复） -- setq不生效

(setq auto-save-default nil             ;禁止自动保存
      make-backup-files nil             ;禁止创建备份文件
      )


;; fill-column标线，可以看到是否超范围，有点碍眼，去掉
;; (setq display-fill-column-indicator-character ?\u254e
;;       display-fill-column-indicator t)


;; 功能不清楚，待定
;; (set indicate-buffer-boundaries 'left)

;; 对大文件或超长行提供性能优化
(setq-default bidi-display-reordering nil)
(setq bidi-inhibit-bpa t)               ;禁止双向括号计算，增加长行处理性能
(setq-default bidi-paragraph-direction 'left-to-right) ;增加长行处理性能
(setq bidi-inhibit-bpa t
      long-line-threshold 1000
      large-hscroll-threshold 1000
      syntax-wholeline-max 1000)

;; 平滑地进行半屏滚动，避免滚动后recenter操作
(setq scroll-step 1
      scroll-conservatively 10000)


;; 全屏快捷键（当前默认全屏）- 暂时没用到
(when (display-graphic-p)
  (bind-keys ("C-<f11>" . toggle-frame-fullscreen)
             ("C-s-f" . toggle-frame-fullscreen) ; Compatible with macOS
             ("S-s-<return>" . toggle-frame-fullscreen)
             ("M-S-<return>" . toggle-frame-fullscreen)))


;; 通过这两个变更控制magit弹出窗口的位置
;; 原先在右边，突然变到下边，可能是因为调整了字体及大小 2->1.5
(setq-default split-height-threshold nil)
;; prevent dired window split 3 columns
(setq-default split-width-threshold (* 1.5 (window-width)))


(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(setq fast-but-imprecise-scrolling t)
(setq redisplay-skip-fontification-on-input t)


;; Initial frame
;; 全屏显示
(setq  initial-frame-alist (quote ((fullscreen . maximized))))
;; 居中显示
;; (setq initial-frame-alist '((top . 0.5)
;;                             (left . 0.5)
;;                             (width . 0.628)
;;                             (height . 0.8)
;;                             (fullscreen)))

;; Optimization
(setq idle-update-delay 1.0)


;; 只显示文件名，不显示路径
(setq frame-title-format '("Tmhourglass's Emacs - %b")
      icon-title-format frame-title-format)


;; 设置字体及大小 -- init-font中单独设置
;; (set-face-attribute 'default nil :height 150)

;; --debug-init implies `debug-on-error'.
;; 出错时默认进入调试模式
(setq debug-on-error init-file-debug)

;; 自动重载文件 （文件修改后显示修改后的内容，自动刷新）
;; auto-revert-interval 默认为5s
(global-auto-revert-mode t)


(provide 'init-generic)

;;; init-generic.el ends here
