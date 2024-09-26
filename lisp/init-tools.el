;;; init-tools.el --- tools -*- lexical-binding: t -*-

;; Author: shaowen he

;;; Commentary:

;; commentary

;;; Code:

;; 全局高亮，可跨越所有文件和buffer，当前绑定到一个自定义函数
;; 快捷键 `SPC h h`
;; quelpa 去掉，考虑重新管理这些包  -- 暂时注释掉
;; (use-package highlight-global
;;   :commands (highlight-frame-toggle)
;;   :quelpa (highlight-global :fetcher github :repo "glen-dai/highlight-global")
;;   :config
;;   (progn
;;     (setq-default highlight-faces
;;                   '(('hi-red-b . 0)
;;                     ('hi-aquamarine . 0)
;;                     ('hi-pink . 0)
;;                     ('hi-blue-b . 0)))))


;; 待明确
(use-package symbol-overlay
  :config
  (define-key symbol-overlay-map (kbd "h") 'nil))

;; 辅助正则编写，正则可视化
(use-package visual-regexp
  :commands (vr/replace vr/query-replace))

(use-package visual-regexp-steroids
  :commands (vr/select-replace vr/select-query-replace)
  :init
  (progn
    (define-key global-map (kbd "C-c C-r") 'vr/replace)
    (define-key global-map (kbd "C-c C-q") 'vr/query-replace)))

(use-package discover-my-major)


;; 去掉有道词典，解决启动警告：Edebug: names-edebug-anon0 一并从elpa下删除
;; 暂时用得少，后面再考虑加上
;; (use-package youdao-dictionary
;;   :commands (youdao-dictionary-search-at-point+)
;;   :init
;;   (global-set-key (kbd "C-c y") 'youdao-dictionary-search-at-point+))


;; 多点编辑
(use-package iedit
  :init
  (setq iedit-toggle-key-default nil)
  :config
  (define-key iedit-mode-keymap (kbd "M-h") 'iedit-restrict-function)
  (define-key iedit-mode-keymap (kbd "M-i") 'iedit-restrict-current-line))


;; 配合多点编辑， mc/edit-lines  mc/edit-ends-of-lines 开头和结尾添加光标，以便编辑
;; 可替换vim模式下的C-v列编辑模式
;; 发现个新用法：在区域开头列编辑，按$选中到末尾，再按S "，即可使所有行使用引号包裹
;; 所有功能以mc/开头，有很多功能，没完全使用到，其他包有依赖使用
;; 已通过borg同化内置，相关命令可用，若没有配置，下面这行可不用
;; (use-package multiple-cursors)


;; 启用evil模式下多点编辑的默认绑定按键
;; 当前多点编辑使用这个
(use-package evil-multiedit
  :commands (evil-multiedit-default-keybinds)
  :init
  (evil-multiedit-default-keybinds))


;; 扩展选区
;; 这个也可用于多点编辑，有多种按键 s-d或SPC v 或C-=
;; 进入该模式时，按e即可进入iedit-mode
(use-package expand-region
  :config
  (defadvice er/prepare-for-more-expansions-internal
      (around helm-ag/prepare-for-more-expansions-internal activate)
    ad-do-it
    (let ((new-msg (concat (car ad-return-value)
                           ", H to highlight in buffers"
                           ", / to search in project, "
                           "e iedit mode in functions"
                           "f to search in files, "
                           "b to search in opened buffers"))
          (new-bindings (cdr ad-return-value)))
      (cl-pushnew
       '("H" (lambda ()
               (interactive)
               (call-interactively
                'zilongshanren/highlight-dwim)))
       new-bindings)
      (cl-pushnew
       '("/" (lambda ()
               (interactive)
               (call-interactively
                'my/search-project-for-symbol-at-point)))
       new-bindings)
      (cl-pushnew
       '("e" (lambda ()
               (interactive)
               (call-interactively
                'evil-multiedit-match-all)))
       new-bindings)
      (cl-pushnew
       '("f" (lambda ()
               (interactive)
               (call-interactively
                'find-file)))
       new-bindings)
      (cl-pushnew
       '("b" (lambda ()
               (interactive)
               (call-interactively
                'consult-line)))
       new-bindings)
      (setq ad-return-value (cons new-msg new-bindings)))))

;; 定义服务，以便快速启动  hugo
(use-package prodigy
  :commands (prodigy)
  :config
  (progn
    ;; define service
    (prodigy-define-service
      :name "Hugo Server"
      :command "hugo"
      :args '("server" "-D" "--navigateToChanged" "-t" "even")
      :cwd blog-admin-dir
      :tags '(hugo server)
      :kill-signal 'sigkill
      :kill-process-buffer-on-stop t)

    (prodigy-define-service
      :name "hugo Deploy"
      :command "bash"
      :args '("./deploy.sh")
      :cwd blog-admin-dir
      :tags '(hugo deploy)
      :kill-signal 'sigkill
      :kill-process-buffer-on-stop t)))



;; 使用喜欢的模式在单独的缓冲区编辑注释/字符串/文档字符串或代码
;; 快捷键 C-c '
(use-package separedit
  :init
  (define-key prog-mode-map (kbd "C-c '") #'separedit)
  (define-key minibuffer-local-map (kbd "C-c '") #'separedit)
  (define-key help-mode-map (kbd "C-c '") #'separedit)
  ;; Default major-mode for edit buffer
  ;; can also be other mode e.g. ‘markdown-mode’.
  (setq separedit-default-mode 'org-mode))


;; (use-package protobuf-mode
;;   :config
;;   (define-key protobuf-mode-map (kbd "RET") 'av/auto-indent-method-maybe))


;;cost time
;; https://github.com/emacsorphanage/quickrun
(use-package quickrun
  :commands (quickrun)
  :init
  (quickrun-add-command "c++/c1z"
    '((:command . "g++")
      (:exec . ("%c -std=c++1z %o -o %e %s"
                "%e %a"))
      (:remove . ("%e")))
    :default "c++")
  (quickrun-add-command "python"
    '((:command . "python3"))
    :default "python"))

(use-package uuidgen
  :commands (uuidgen))

;; 链接上进行相关操作，如打开等
(use-package link-hint)

(use-package keycast
  :commands (toggle-keycast)
  :config
  (defun toggle-keycast()
    (interactive)
    (if (member '("" keycast-mode-line " ") global-mode-string)
        (progn (setq global-mode-string (delete '("" keycast-mode-line " ") global-mode-string))
               (remove-hook 'pre-command-hook 'keycast--update)
               (message "Keycast OFF"))
      (add-to-list 'global-mode-string '("" keycast-mode-line " "))
      (add-hook 'pre-command-hook 'keycast--update t)
      (message "Keycast ON"))))

(use-package sudo-edit)

;; todo-borg
;; (use-package vterm)

;; (use-package multi-vterm
;;   :config
;;   (add-hook 'vterm-mode-hook
;;             (lambda ()
;;               (setq-local evil-insert-state-cursor 'box)
;;               (evil-insert-state)))
;;   (define-key vterm-mode-map [return] #'vterm-send-return)

;;   (setq vterm-keymap-exceptions nil)
;;   (evil-define-key 'insert vterm-mode-map (kbd "C-e") #'vterm--self-insert)
;;   (evil-define-key 'insert vterm-mode-map (kbd "C-f") #'vterm--self-insert)
;;   (evil-define-key 'insert vterm-mode-map (kbd "C-a") #'vterm--self-insert)
;;   (evil-define-key 'insert vterm-mode-map (kbd "C-v") #'vterm--self-insert)
;;   (evil-define-key 'insert vterm-mode-map (kbd "C-b") #'vterm--self-insert)
;;   (evil-define-key 'insert vterm-mode-map (kbd "C-w") #'vterm--self-insert)
;;   (evil-define-key 'insert vterm-mode-map (kbd "C-u") #'vterm--self-insert)
;;   (evil-define-key 'insert vterm-mode-map (kbd "C-d") #'vterm--self-insert)
;;   (evil-define-key 'insert vterm-mode-map (kbd "C-n") #'vterm--self-insert)
;;   (evil-define-key 'insert vterm-mode-map (kbd "C-m") #'vterm--self-insert)
;;   (evil-define-key 'insert vterm-mode-map (kbd "C-p") #'vterm--self-insert)
;;   (evil-define-key 'insert vterm-mode-map (kbd "C-j") #'vterm--self-insert)
;;   (evil-define-key 'insert vterm-mode-map (kbd "C-k") #'vterm--self-insert)
;;   (evil-define-key 'insert vterm-mode-map (kbd "C-r") #'vterm--self-insert)
;;   (evil-define-key 'insert vterm-mode-map (kbd "C-t") #'vterm--self-insert)
;;   (evil-define-key 'insert vterm-mode-map (kbd "C-c") #'vterm--self-insert)
;;   (evil-define-key 'insert vterm-mode-map (kbd "C-SPC") #'vterm--self-insert)
;;   (evil-define-key 'normal vterm-mode-map (kbd "C-d") #'vterm--self-insert)
;;   (evil-define-key 'normal vterm-mode-map (kbd ",c") #'multi-vterm)
;;   (evil-define-key 'normal vterm-mode-map (kbd ",n") #'multi-vterm-next)
;;   (evil-define-key 'normal vterm-mode-map (kbd ",p") #'multi-vterm-prev)
;;   (evil-define-key 'normal vterm-mode-map (kbd "i") #'evil-insert-resume)
;;   (evil-define-key 'normal vterm-mode-map (kbd "o") #'evil-insert-resume)
;;   (evil-define-key 'normal vterm-mode-map (kbd "<return>") #'evil-insert-resume))

;; (setq tramp-adb-program "/Users/lionqu/Library/Android/sdk/platform-tools/adb")

(use-package ws-butler
  :init
  (add-hook 'prog-mode-hook #'ws-butler-mode))

(use-package pinyinlib
  :after orderless
  :autoload pinyinlib-build-regexp-string
  :init
  (defun completion--regex-pinyin (str)
    (orderless-regexp (pinyinlib-build-regexp-string str)))
  (add-to-list 'orderless-matching-styles 'completion--regex-pinyin))


;; cost time
;; tramp 远程编辑
(use-package tramp
  :defer t
  :custom
  ;; Always use file cache when using tramp
  (remote-file-name-inhibit-cache nil)
  (tramp-default-method "ssh"))

(use-package tramp-sh
  :config (cl-pushnew 'tramp-own-remote-path tramp-remote-path))


;; 浏览unix命令手册页 -- 用于查看linux命令帮助
(use-package man
  :defer t
  :config (setq Man-width 80))



(provide 'init-tools)

;;; init-tools.el ends here
