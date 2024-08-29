;;; init-general-keys.el --- general-keys -*- lexical-binding: t -*-

;; Author: shaowen he

;;; Commentary:

;; general绑定的快捷键，延迟加载

;;; Code:


;; emacs中方便的按键绑定
(use-package general
  :init
  (general-emacs-define-key 'global [remap imenu] 'consult-imenu)
  (general-emacs-define-key 'global [remap apropos] 'consult-apropos)
  (global-definer
    "!" 'shell-command
    ":" 'eval-expression
    "SPC" 'execute-extended-command
    "x" 'switch-to-scratch-buffer
    "TAB" 'spacemacs/alternate-buffer
    "'" 'vertico-repeat
    "=" 'indent-buffer
    "+" 'text-scale-increase
    "-" 'text-scale-decrease
    "u" 'universal-argument
    "v" 'er/expand-region
    "0" 'select-window-0
    "1" 'select-window-1
    "2" 'select-window-2
    "3" 'select-window-3
    "4" 'select-window-4
    "5" 'select-window-5
    ";" 'vterm
    "`" 'multi-vterm-project
    "hf" 'describe-function             ;包含函数和宏
    "hv" 'describe-variable
    "hk" 'describe-key
    "qq" 'save-buffers-kill-terminal
    "qR" 'restart-emacs
    "hh" 'zilongshanren/highlight-dwim
    "hc" 'zilongshanren/clearn-highlight
    "en" 'my-goto-next-error
    "ry" 'consult-yank-pop              ;M-y
    "R" 'my/run-current-file            ;在shell中运行当前文件
    "ep" 'my-goto-previous-error
    "el" 'my-list-errors)

  (+general-global-menu! "git/goto" "g"
    ;; =========git============
    "s" 'magit-status
    "l" 'git-link
    "h" 'git-link-homepage
    "c" 'git-link-commit
    ;; =========goto============
    "d" 'vc-diff
    "g" 'xref-find-definitions
    "r" 'xref-find-references
    "m" 'consult-mark
    "M" 'consult-global-mark
    )

  (+general-global-menu! "open" "o"
    "o" 'zilongshanren/hotspots
    "r" 'org-roam-node-find
    "c" 'org-capture
    "l" 'org-store-link
    "t" 'ansi-term
    "y" 'my/eudic)


  (+general-global-menu! "file" "f"
    "f" 'find-file
    "F" 'find-file-other-window
    "/" 'find-file-other-window
    "C" 'my/copy-current-file
    "D" 'my/delete-current-file
    "y" 'my/copy-current-filename
    "R" 'my/rename-current-file
    "r" 'consult-recent-file
    ;; "r" 'recentf-open-files
    "l" 'find-file-literally
    "j" 'dired-jump
    "J" 'dired-jump-other-window
    "d" 'consult-dir
    "L" 'consult-locate
    "ed" 'open-my-init-file
    "s" 'save-buffer
    "w" 'sudo-edit
    "S" 'save-some-buffers
    "!" 'my/exec-shell-on-buffer)

  (+general-global-menu! "bufmark" "b"
    ;; =========buffer============
    "b" '(consult-buffer :which-key "consult buffer")
    "B" 'switch-to-buffer-other-window
    "c" 'clone-indirect-buffer
    "C" 'clone-indirect-buffer-other-window
    "y" 'my/copy-current-buffername
    "v" 'revert-buffer-quick
    "x" 'scratch-buffer
    "z" 'bury-buffer
    ;; "d" 'kill-current-buffer
    "n" 'next-buffer
    "p" 'previous-buffer
    "R" 'rename-buffer
    "f" 'my-open-current-directory
    ;; "k" 'kill-buffer
    "k" 'kill-current-buffer
    "K" 'kill-other-buffers
    ;; =========bookmark============
    "m" 'bookmark-set
    "M" 'bookmark-set-no-overwrite
    "i" 'bookmark-insert
    "r" 'bookmark-rename
    "d" 'bookmark-delete
    "w" 'bookmark-write
    "j" 'bookmark-jump
    "J" 'bookmark-jump-other-window
    "l" 'bookmark-bmenu-list
    "s" 'bookmark-save)

  (+general-global-menu! "code" "c"
    "c" 'compile
    "C" 'recompile
    "k" 'kill-compilation
    "w" 'delete-trailing-whitespace
    "x" 'quickrun                       ;直接运行当前文件，需配置执行命令
    )

  (+general-global-menu! "search" "s"
    "i" 'my/imenu
    "s" 'consult-line
    "p" 'consult-ripgrep
    "j" 'evil-show-jumps
    "m" 'evil-show-marks
    "r" 'evil-show-registers
    ;; "k" 'consult-keep-lines
    ;; "f" 'consult-focus-lines
    )

  (+general-global-menu! "layout" "l"
    "l" 'tabspaces-switch-or-create-workspace
    "L" 'tabspaces-restore-session
    "p" 'tabspaces-open-or-create-project-and-workspace
    "f" 'tabspaces-project-switch-project-open-file
    "s" 'tabspaces-save-session
    "B" 'tabspaces-switch-buffer-and-tab
    "b" 'tabspaces-switch-to-buffer
    "R" 'tab-rename
    "TAB" 'tab-bar-switch-to-recent-tab
    "r" 'tabspaces-remove-current-buffer
    "k" 'tabspaces-close-workspace)

  (+general-global-menu! "window" "w"
    "/" 'split-window-right
    "-" 'split-window-below
    "m" 'delete-other-windows
    "u" 'winner-undo
    "z" 'winner-redo
    "w" 'esw/select-window
    "s" 'esw/swap-two-windows
    "d" 'esw/delete-window
    "=" 'balance-windows-area
    "r" 'esw/move-window
    "x" 'resize-window
    "H" 'buf-move-left
    "L" 'buf-move-right
    "J" 'buf-move-down
    "K" 'buf-move-up)

  (+general-global-menu! "toggle" "t"
    "s" 'flycheck-mode
    "S" 'flyspell-prog-mode
    "e" 'toggle-corfu-english-helper    ;英文编写助手
    "r" 'read-only-mode                 ;只读模式
    "n" 'my-toggle-line-numbber         ;触发行号
    ;; "w" 'writeroom-mode
    "w" 'prose-mode                     ;启动专注写作模式
    "k" '+toggle-keycast                ;keycast
    "c" 'global-corfu-mode              ;全局corfu模式
    "m" 'consult-minor-mode-menu)

  (+general-global-menu! "project" "p"
    "f" 'project-find-file
    "r" 'consult-recent-file
    "s" 'project-find-regexp
    "d" 'project-dired
    "b" 'consult-project-buffer
    "e" 'project-eshell
    "m" 'my/project-run-makefile-target
    "c" 'project-compile
    "t" 'my/project-citre
    "p" 'project-switch-project
    "i" 'my/project-info
    "a" 'project-remember-projects-under
    "x" 'project-forget-project)




  (evil-define-key 'normal dired-mode-map
    (kbd "<RET>") 'dired-find-alternate-file
    (kbd "C-k") 'dired-up-directory
    "`" 'dired-open-term
    "o" 'dired-find-file-other-window
    "s" 'hydra-dired-quick-sort/body
    "z" 'dired-get-size
    ")" 'dired-omit-mode)
  )



(provide 'init-general-keys)

;;; init-general-keys.el ends here
