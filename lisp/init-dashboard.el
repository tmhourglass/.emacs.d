;;; init-dashboard.el --- dashboard延迟加载优化 -*- lexical-binding: t -*-

;; Author: shaowen he

;;; Commentary:

;; Dashboard配置 - 优化启动速度版本
;; 完全延迟加载，只在需要时才初始化

;;; Code:

(use-package dashboard
  :defer t  ; 完全延迟加载
  :commands (dashboard-open open-dashboard dashboard-refresh-buffer)
  :diminish dashboard-mode
  :custom-face
  (dashboard-heading ((t (:inherit (font-lock-string-face bold)))))
  (dashboard-items-face ((t (:weight normal))))
  (dashboard-no-items-face ((t (:weight normal))))

  :bind (("<f3>" . open-dashboard)
         :map dashboard-mode-map
         ("H" . browse-homepage)
         ("R" . restore-session)
         ("C" . find-custom-file)
         ("U" . update-config-and-packages)
         ("q" . quit-dashboard)
         ("h" . dashboard-hydra/body)
         ("?" . dashboard-hydra/body))
  :hook (dashboard-mode . (lambda ()
                            ;; No title
                            (setq-local frame-title-format nil)
                            ;; Enable `page-break-lines-mode'
                            (when (fboundp 'page-break-lines-mode)
                              (page-break-lines-mode 1))))
  :init
  ;; 最小化启动时配置，只设置必要的变量
  (setq dashboard-banner-logo-title "TMHOURGLASS EMACS - Enjoy Programming & Writing"
        dashboard-center-content t
        dashboard-vertically-center-content t
        dashboard-show-shortcuts t
        dashboard-items '((recents . 7)
                          (bookmarks . 5)
                          (projects . 5)))

  ;; 延迟设置复杂配置的函数
  (defun my/setup-dashboard-config ()
    "延迟设置dashboard的复杂配置."
    (setq dashboard-startup-banner (expand-file-name "logo.png" user-emacs-directory)
          dashboard-page-separator "\n\f\n"
          dashboard-projects-backend 'project-el
          dashboard-path-style 'truncate-middle
          dashboard-path-max-length 60)

    ;; 设置基本布局（延迟设置复杂图标）
    (setq dashboard-startupify-list '(dashboard-insert-banner
                                      dashboard-insert-newline
                                      dashboard-insert-banner-title
                                      dashboard-insert-newline
                                      dashboard-insert-navigator
                                      dashboard-insert-newline
                                      dashboard-insert-init-info
                                      dashboard-insert-items
                                      dashboard-insert-newline
                                      dashboard-insert-footer)))

  ;; 延迟设置图标和按钮的函数
  (defun my/setup-dashboard-icons ()
    "延迟设置dashboard的图标配置."
    (when (fboundp 'icons-displayable-p)
      (setq dashboard-display-icons-p #'icons-displayable-p
            dashboard-set-file-icons t
            dashboard-set-heading-icons t
            dashboard-heading-icons '((recents . "nf-oct-history")
                                      (bookmarks . "nf-oct-bookmark")
                                      (agenda . "nf-oct-calendar")
                                      (projects . "nf-oct-briefcase")
                                      (registers . "nf-oct-database"))

            ;; 设置标题下的按钮（延迟加载图标）
            dashboard-navigator-buttons
            `(((,(when (icons-displayable-p)
                   (nerd-icons-mdicon "nf-md-github" :height 1.4))
                "Homepage" "Browse homepage"
                (lambda (&rest _) (browse-url "https://github.com/tmhourglass/.emacs.d")))
               (,(when (icons-displayable-p)
                   (nerd-icons-mdicon "nf-md-backup_restore" :height 1.5))
                "Restore" "Restore previous session"
                (lambda (&rest _) (restore-session)))
               (,(when (icons-displayable-p)
                   (nerd-icons-mdicon "nf-md-tools" :height 1.3))
                "Settings" "Open custom file"
                (lambda (&rest _) (find-file custom-file)))
               (,(when (icons-displayable-p)
                   (nerd-icons-mdicon "nf-md-help" :height 1.2) "?")
                "" "Help (?/h)"
                (lambda (&rest _) (dashboard-hydra/body)))))

            ;; 设置脚注图标
            dashboard-footer-icon
            (if (icons-displayable-p)
                (nerd-icons-octicon "nf-oct-heart" :height 1.2 :face 'nerd-icons-lred)
              (propertize ">" 'face 'dashboard-footer)))))

  ;; 移除立即启动，改为按需启动
  ;; (dashboard-setup-startup-hook)  ; 注释掉立即启动

  :config
  ;; 智能初始化：只在实际需要时设置复杂配置
  (defun my/ensure-dashboard-ready ()
    "确保dashboard已完全配置."
    (unless (bound-and-true-p my/dashboard-configured)
      (my/setup-dashboard-config)
      (my/setup-dashboard-icons)
      (setq my/dashboard-configured t)))

  ;; 在dashboard实际显示前进行配置
  (advice-add #'dashboard-refresh-buffer :before #'my/ensure-dashboard-ready)

  ;; Insert copyright
  ;; @see https://github.com/emacs-dashboard/emacs-dashboard/issues/219
  (defun my-dashboard-insert-copyright ()
    "Insert copyright in the footer."
    (dashboard-insert-center
     (propertize (format "\nPowered by TMHOURGLASS, %s\n" (format-time-string "%Y"))
                 'face 'font-lock-comment-face)))
  ;; 插入脚注之后插入版权
  (advice-add #'dashboard-insert-footer :after #'my-dashboard-insert-copyright)


  ;; 恢复先前的会话
  (defun restore-session ()
    "Restore the previous session."
    (interactive)
    (message "Restoring previous session...")
    (quit-window t)
    (cond
     ((bound-and-true-p tabspaces-mode)
      (tabspaces-restore-session))
     ((bound-and-true-p desktop-save-mode)
      (desktop-read)))
    (message "Restoring previous session...done"))


  ;; 以下快捷键暂时没用到，使用默认的即可
  ;; ;; 定义跳转到recent的快捷键
  ;; (defun dashboard-goto-recent-files ()
  ;;   "Go to recent files."
  ;;   (interactive)
  ;;   (let ((func (local-key-binding "r")))
  ;;     (and func (funcall func))))

  ;; ;; 定义跳转到project的快捷键
  ;; (defun dashboard-goto-projects ()
  ;;   "Go to projects."
  ;;   (interactive)
  ;;   (let ((func (local-key-binding "p")))
  ;;     (and func (funcall func))))

  ;; ;; 定义跳转到bookmark的快捷键
  ;; (defun dashboard-goto-bookmarks ()
  ;;   "Go to bookmarks."
  ;;   (interactive)
  ;;   (let ((func (local-key-binding "m")))
  ;;     (and func (funcall func))))


  ;; 是否恢复布局，用来进入与退出该页面
  (defvar dashboard-recover-layout-p nil
    "Wether recovers the layout.")

  ;; 优化的dashboard打开函数
  (defun open-dashboard ()
    "Open the *dashboard* buffer and jump to the first widget."
    (interactive)
    ;; 确保dashboard已配置
    (my/ensure-dashboard-ready)

    ;; 如果还没有设置startup hook，现在设置
    (unless (bound-and-true-p my/dashboard-startup-setup)
      (dashboard-setup-startup-hook)
      (setq my/dashboard-startup-setup t))

    ;; Check if need to recover layout
    (if (length> (window-list-1)
                 ;; exclude `treemacs' window
                 (if (and (fboundp 'treemacs-current-visibility)
                          (eq (treemacs-current-visibility) 'visible))
                     2
                   1))
        (setq dashboard-recover-layout-p t))

    ;; Display dashboard in maximized window
    (delete-other-windows)

    ;; Refresh dashboard buffer
    (dashboard-refresh-buffer)

    ;; Jump to the first section
    (dashboard-goto-recent-files))

  ;; 添加一个快速启动dashboard的函数（用于启动时）
  (defun my/maybe-show-dashboard ()
    "在启动时可能显示dashboard（仅在没有打开文件时）."
    (when (and (< (length command-line-args) 2)  ; 没有命令行参数
               (not (buffer-file-name))          ; 当前buffer没有文件
               (string= (buffer-name) "*scratch*")) ; 在scratch buffer中
      (run-with-timer 0.1 nil #'open-dashboard)))

  ;; 退出
  (defun quit-dashboard ()
    "Quit dashboard window."
    (interactive)
    (quit-window t)
    (and dashboard-recover-layout-p
         (and (bound-and-true-p winner-mode) (winner-undo))
         (setq dashboard-recover-layout-p nil)))

  ;; 性能优化：添加一些便捷函数
  (defun my/dashboard-quick-setup ()
    "快速设置dashboard（用于测试）."
    (interactive)
    (my/ensure-dashboard-ready)
    (message "Dashboard配置已完成"))

  ;; 优化按键绑定
  :bind (("C-c d d" . open-dashboard)
         ("C-c d s" . my/dashboard-quick-setup)))

;; 延迟启动机制：在dashboard模块加载时检查是否需要显示
(with-eval-after-load 'init-dashboard
  (run-with-timer 0.5 nil
                  (lambda ()
                    (when (featurep 'dashboard)
                      (my/maybe-show-dashboard)))))

(provide 'init-dashboard)

;;; init-dashboard.el ends here
