;;; backup.el --- backup -*- lexical-binding: t -*-

;; Author: shaowen he

;;; Commentary:

;; commentary

;;; Code:

;; =============== function ==============

;; 试用新主题
(defun my/load-doom-theme (theme)
  "Disable active themes and load a Doom theme."
  (interactive (list (intern (completing-read "Theme: "
                                              (->> (custom-available-themes)
                                                   (-map #'symbol-name)
                                                   (--select (string-prefix-p "doom-" it)))))))
  (ap/switch-theme theme)
  (set-face-foreground 'org-indent (face-background 'default)))

(defun my/switch-theme (theme)
  "Disable active themes and load THEME."
  (interactive (list (intern (completing-read "Theme: "
                                              (->> (custom-available-themes)
                                                   (-map #'symbol-name))))))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme 'no-confirm))


;; 若elisp是emacs的一部分时，不允许编辑
;; 暂时未使用，若需要使用，需绑定到emacs-lisp-mode中
(defun sanityinc/maybe-set-bundled-elisp-readonly ()
  "If this elisp appears to be part of Emacs, then disallow editing."
  (when (and (buffer-file-name)
             (string-match-p "\\.el\\.gz\\'" (buffer-file-name)))
    (setq buffer-read-only t)
    (view-mode 1)))


;; 不显示 *scratch*
(defun remove-scratch-buffer ()
  (if (get-buffer "*scratch*")
      (kill-buffer "*scratch*")))
(add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)


;; Don't ask me when close emacs with process is running
;; noflet 局部函数装饰，暂时没有，可使用use-package安装
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (require 'noflet)
  (noflet ((process-list ())) ad-do-it))

;; Don't ask me when kill process buffer
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; 查找项目目录
(setq project-find-functions '(my/project-try-local project-try-vc))


;; 禁用本地编译
;; (when (not (version< emacs-version "29.0"))
;;   (setq package-native-compile nil))



;; Theme
;; 当前设置的不生效，似乎只是加载了，未启用，仍然使用的是default(default竟然用了这么久而不自知)
;; 设置一个白天，一个默认，可通过函数调用切换
;; (use-package color-theme-sanityinc-tomorrow
;;   :custom
;;   (custom-safe-themes t)
;;   (custom-enabled-themes '(sanityinc-tomorrow-day))
;;   :hook (after-init . reapply-themes)
;;   :config
;;   (defun reapply-themes ()
;;     "Forcibly load the themes listed in `custom-enabled-themes'."
;;     (dolist (theme custom-enabled-themes)
;;       (unless (custom-theme-p theme)
;;         (load-theme theme)))
;;     (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

;;   (defun light ()
;;     "Activate a light color theme."
;;     (interactive)
;;     (setq custom-enabled-themes '(sanityinc-tomorrow-day))
;;     (reapply-themes))

;;   (defun dark ()
;;     "Activate a dark color theme."
;;     (interactive)
;;     (setq custom-enabled-themes '(sanityinc-tomorrow-bright))
;;     (reapply-themes)))



;; 在调试时，尝试包而不安装它
;; Try out emacs package without installing
(use-package try
  :commands try try-and-refresh)


;; emacs 调试技巧
;; https://whatacold.io/zh-cn/blog/2022-07-17-emacs-elisp-debug/
;; 调试时使用
;; Incremental complete in minibuffer
(use-package icomplete
  :hook (emacs-startup . icomplete-mode)
  :custom
  (icomplete-vertical-mode t)
  (icomplete-prospects-height 10)
  (icomplete-hide-common-prefix nil)
  (icomplete-show-matches-on-no-input nil))



;; 加载个人配置
(progn                                  ;     personalize
  (let ((file (expand-file-name (concat (user-real-login-name) ".el")
                                user-emacs-directory)))
    (when (file-exists-p file)
      (load file))))


;; 优化启动时间
;; 必须放在最前面才能收集到（use-package之后）
;; (use-package benchmark-init
;;   :demand t
;;   :config
;;   ;; To disable collection of benchmark data after init is done.
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate))


;; 测试某段代码的执行时间，使用这个包裹执行
(defmacro my/timer (&rest body)
  "Measure and return the time it takes evaluating BODY."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))



(with-eval-after-load 'org
  (dolist (lang '("C" "C++"))
    (eval `(eglot-org-babel-enable ,lang))))


;; Start server  若未启动服务端，则启动
;; (use-package server
;;   :hook (after-init . (lambda ()
;;                         (unless (server-running-p)
;;                           (server-start)))))

;; Server mode.
;; Use emacsclient to connect
;; (use-package server
;;   :hook (after-init . server-mode))



;; 显示文件名及路径
;; (setq frame-title-format
;;       `((buffer-file-name "%f" "%b")
;;         ,(format " - GNU Emacs %s" emacs-version)))
