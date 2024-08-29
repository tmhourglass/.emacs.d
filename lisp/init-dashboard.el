;;; init-dashboard.el --- dashboard -*- lexical-binding: t -*-

;; Author: shaowen he

;;; Commentary:

;; commentary

;;; Code:

(use-package dashboard
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
  (setq dashboard-banner-logo-title "TMHOURGLASS EMACS - Enjoy Programming & Writing"
        dashboard-startup-banner (expand-file-name "logo.png" user-emacs-directory)
        dashboard-page-separator "\n\f\n"
        dashboard-projects-backend 'project-el
        dashboard-path-style 'truncate-middle
        dashboard-path-max-length 60
        dashboard-center-content t
        dashboard-vertically-center-content t
        ;; 显示快捷键
        dashboard-show-shortcuts t
        dashboard-items '((recents . 7)
                          (bookmarks . 5)
                          (projects . 5))

        ;; 各区域整体布局顺序
        dashboard-startupify-list '(dashboard-insert-banner
                                    dashboard-insert-newline
                                    dashboard-insert-banner-title
                                    dashboard-insert-newline
                                    dashboard-insert-navigator
                                    dashboard-insert-newline
                                    dashboard-insert-init-info
                                    dashboard-insert-items
                                    dashboard-insert-newline
                                    dashboard-insert-footer)

        dashboard-display-icons-p #'icons-displayable-p
        dashboard-set-file-icons t
        dashboard-set-heading-icons t
        dashboard-heading-icons '((recents . "nf-oct-history")
                                  (bookmarks . "nf-oct-bookmark")
                                  (agenda . "nf-oct-calendar")
                                  (projects . "nf-oct-briefcase")
                                  (registers . "nf-oct-database"))

        ;; 设置标题下的按钮
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
          (propertize ">" 'face 'dashboard-footer)))

  ;; 启动
  (dashboard-setup-startup-hook)

  :config
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

  ;; 进入
  (defun open-dashboard ()
    "Open the *dashboard* buffer and jump to the first widget."
    (interactive)
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

  ;; 退出
  (defun quit-dashboard ()
    "Quit dashboard window."
    (interactive)
    (quit-window t)
    (and dashboard-recover-layout-p
         (and (bound-and-true-p winner-mode) (winner-undo))
         (setq dashboard-recover-layout-p nil))))



(provide 'init-dashboard)

;;; init-dashboard.el ends here
