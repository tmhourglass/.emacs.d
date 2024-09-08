;;; init-dired.el --- dired -*- lexical-binding: t -*-

;; Author: shaowen he

;;; Commentary:

;; commentary

;;; Code:

;; 类似包：dirvish，可完全替换

;; 定制dired文件管理器
;; C-c C-p 可写模式 -- 待定
;; dired-hide-details-mode 去掉注释会隐藏详情
(use-package dired
  ;; :demand t
  :bind (:map dired-mode-map
              ("C-c C-p" . wdired-change-to-wdired-mode))
  ;; :hook (dired-mode . dired-hide-details-mode)
  :custom
  (dired-dwim-target t)
  (dired-listing-switches "-alGhv --group-directories-first")
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-by-moving-to-trash t)
  (dired-kill-when-opening-new-dired-buffer t)
  :config
  (when sys/macp
    (if (executable-find "gls")
        (progn
          ;; Use GNU ls as `gls' from `coreutils' if available.
          (setq insert-directory-program "gls")
          ;; Using `insert-directory-program'
          (setq ls-lisp-use-insert-directory-program t))
      (progn
        ;; Suppress the warning: `ls does not support --dired'.
        (setq dired-use-ls-dired nil)
        (setq dired-listing-switches "-alh"))))
  ;; (when (executable-find "/opt/homebrew/opt/coreutils/libexec/gnubin/ls")
  ;;   (if (string-match-p "x86_64" system-configuration)
  ;;       (setq insert-directory-program "/usr/local/opt/coreutils/libexec/gnubin/ls")
  ;;     (setq insert-directory-program "/opt/homebrew/opt/coreutils/libexec/gnubin/ls"))
  ;;   (dired-quick-sort-setup))
  )

;;额外的字体锁定规则，使dired更丰富多彩
(use-package diredfl
  :hook (dired-mode . diredfl-global-mode))



;; 通过Hydra以各种方式快速排序，dired模式下绑定“S”键调用
;; 小写s似乎也行，不需要绑定按键
;; 需配置到init中，若配置到config中，则进入后无法激活，需先执行命令才能用
(use-package dired-quick-sort
  :init
  (dired-quick-sort-setup)
  (setq dired-quick-sort-suppress-setup-warning t))


;; 在dired中展示git 信息
;; 这个在melpa中找不到 -- 待定
(use-package dired-git-info
  :bind (:map dired-mode-map
              (")" . dired-git-info-mode)))


;; 允许用户通过`rsync'命令复制dired中标记的文件，尤其是大文件
;; 复制发生在后台，不会锁定emacs
(use-package dired-rsync
  :bind (:map dired-mode-map
              ("C-c C-r" . dired-rsync)))


;; 额外的dired功能
;; 指定不同文件的打开命令，以及忽略的文件
(use-package dired-aux :ensure nil)
(use-package dired-x
  :demand t
  :commands (dired-jump)
  :config
  (let ((cmd (cond (sys/macp "open")
                   (sys/linuxp "xdg-open")
                   (sys/win32p "start")
                   (t ""))))
    (setq dired-guess-shell-alist-user
          `(("\\.pdf\\'" ,cmd)
            ("\\.docx\\'" ,cmd)
            ("\\.\\(?:djvu\\|eps\\)\\'" ,cmd)
            ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" ,cmd)
            ("\\.\\(?:xcf\\)\\'" ,cmd)
            ("\\.csv\\'" ,cmd)
            ("\\.tex\\'" ,cmd)
            ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
            ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
            ("\\.html?\\'" ,cmd)
            ("\\.md\\'" ,cmd))))

  (setq dired-omit-files
        (concat dired-omit-files
                "\\|^.DS_Store$\\|^.projectile$\\|^.git*\\|^.svn$\\|^.vscode$\\|\\.js\\.meta$\\|\\.meta$\\|\\.elc$\\|^.emacs.*")))

;; 使用`fd'来替换`find-dired'
;; 不能使用，会导致启动时间无限长，dire使用的是ls
;; (use-package fd-dired
;;   :when (executable-find "fd"))


;; Shows icons
(use-package nerd-icons-dired
  :diminish
  :when (icons-displayable-p)
  :custom-face
  (nerd-icons-dired-dir-face ((t (:inherit nerd-icons-dsilver :foreground unspecified))))
  :hook (dired-mode . nerd-icons-dired-mode))


;; 在dired中禁用该操作
(put 'dired-find-alternate-file 'disabled nil)


(provide 'init-dired)

;;; init-dired.el ends here
