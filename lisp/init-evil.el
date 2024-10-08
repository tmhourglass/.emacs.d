;;; init-evil.el --- evil -*- lexical-binding: t -*-

;;; init-evil.el --- evil-anzu -*- lexical-binding: t -*-

;; Author: shaowen he

;;; Commentary:

;; commentary

;;; Code:

(use-package evil
  ;; :hook (after-init . evil-mode)
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (evil-mode)

  ;; https://emacs.stackexchange.com/questions/46371/how-can-i-get-ret-to-follow-org-mode-links-when-using-evil-mode
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd "RET") nil))

  :config
  (progn
    (setcdr evil-insert-state-map nil)
    (define-key evil-insert-state-map [escape] 'evil-normal-state)

    (setq-default evil-ex-search-persistent-highlight nil)



    (define-key evil-visual-state-map "p" 'evil-paste-after)
    (define-key evil-insert-state-map (kbd "C-r") 'evil-paste-from-register)
    (define-key evil-insert-state-map (kbd "C-;") 'flyspell-correct-previous)


    ;;mimic "nzz" behaviou in vim
    ;; 模仿nzz行为，跳转到某一行，并在中间位置显示
    ;; n为具体数字，normal模式下直接按，快速跳转到某一行
    (defadvice evil-search-next (after advice-for-evil-search-next activate)
      (evil-scroll-line-to-center (line-number-at-pos)))

    (defadvice evil-search-previous (after advice-for-evil-search-previous activate)
      (evil-scroll-line-to-center (line-number-at-pos)))


    ;; 拷贝时保持光标位置不变
    (defun my-evil-yank ()
      (interactive)
      (save-excursion
        (call-interactively 'evil-yank))
      (backward-char))

    (define-key evil-visual-state-map (kbd "y") 'my-evil-yank)

    ;; 大写Y：从当前拷贝到行尾
    (define-key evil-normal-state-map
                (kbd "Y") 'my/yank-to-end-of-line)

    (define-key evil-normal-state-map (kbd "[ SPC") (lambda () (interactive) (evil-insert-newline-above) (forward-line)))
    (define-key evil-normal-state-map (kbd "] SPC") (lambda () (interactive) (evil-insert-newline-below) (forward-line -1)))

    (define-key evil-normal-state-map (kbd "g[")
                (lambda () (interactive) (beginning-of-defun)))

    (define-key evil-normal-state-map (kbd "g]")
                (lambda () (interactive) (end-of-defun)))

    (define-key evil-normal-state-map (kbd "[ b") 'previous-buffer)
    (define-key evil-normal-state-map (kbd "] b") 'next-buffer)
    (define-key evil-motion-state-map (kbd "[ b") 'previous-buffer)
    (define-key evil-motion-state-map (kbd "] b") 'next-buffer)
    (define-key evil-normal-state-map (kbd "M-y") 'consult-yank-pop)

    ;; 在Normal模式下， s-f/s-F用于isearch-forward/isearch-backward, 适配Mac的操作
    ;; 替换原始的C-s/C-r

    ;; 前进后退一个单词和删除删除，适配Mac，使用command按键操作
    (define-key evil-emacs-state-map (kbd "s-f") 'forward-word)
    (define-key evil-insert-state-map (kbd "s-f") 'forward-word)
    (define-key evil-insert-state-map (kbd "C-w") 'evil-delete-backward-word)
    (define-key evil-emacs-state-map (kbd "s-b") 'backward-word)
    (define-key evil-insert-state-map (kbd "s-b") 'backward-word)



    (define-key evil-ex-completion-map "\C-a" 'move-beginning-of-line)
    (define-key evil-ex-completion-map "\C-b" 'backward-char)
    (define-key evil-ex-completion-map "\C-f" 'forward-char)
    (define-key evil-ex-completion-map "\C-k" 'kill-line)

    (define-key minibuffer-local-map (kbd "C-w") 'evil-delete-backward-word)

    (define-key evil-visual-state-map (kbd "C-r") 'zilongshanren/evil-quick-replace)


    ;; Don't move back the cursor one position when exiting insert mode
    (setq evil-move-cursor-back nil)

    (define-key evil-emacs-state-map (kbd "C-w") 'evil-delete-backward-word)

    (evil-define-key 'emacs term-raw-map (kbd "C-w") 'evil-delete-backward-word)


    (setq evil-normal-state-tag (propertize "[N]" 'face '((:background "DarkGoldenrod2" :foreground "black")))
          evil-emacs-state-tag (propertize "[E]" 'face '((:background "SkyBlue2" :foreground "black")))
          evil-insert-state-tag (propertize "[I]" 'face '((:background "chartreuse3") :foreground "white"))
          evil-motion-state-tag (propertize "[M]" 'face '((:background "plum3") :foreground "white"))
          evil-visual-state-tag (propertize "[V]" 'face '((:background "gray" :foreground "black")))
          evil-operator-state-tag (propertize "[O]" 'face '((:background "purple"))))

    ;; evil模式下光标的颜色调整 （不是这个原因，而是sis-default-cursor-color导致）
    ;; (set-cursor-color "red") 也可一试
    ;; https://emacs-china.org/t/cursor/18531/3
    ;; (setq evil-insert-state-cursor '("chartreuse3" bar))
    (setq evil-normal-state-cursor '("DarkGoldenrod2" box)
          evil-insert-state-cursor '("chartreuse3" (bar . 2))
          evil-emacs-state-cursor '("SkyBlue2" box)
          evil-replace-state-cursor '("chocolate" (hbar . 2))
          evil-visual-state-cursor '("red" hbar) ;不太明显：修改不生效，似乎后面有覆盖 （默认为灰色 gray）
          evil-motion-state-cursor '("plum3" box))

    (define-key evil-insert-state-map (kbd "C-z") 'evil-emacs-state)))


(use-package evil-anzu
  :after evil
  :diminish
  :hook (after-init . global-anzu-mode))


(use-package evil-collection
  :hook (after-init . evil-collection-init)
  :config
  (setq evil-collection-mode-list (remove 'lispy evil-collection-mode-list))

  (cl-loop for (mode . state) in
           '((org-agenda-mode . normal)
             (Custom-mode . emacs)
             (eshell-mode . emacs)
             (makey-key-mode . motion))
           do (evil-set-initial-state mode state))


  (when sys/win32p
    (evil-define-key 'normal dired-mode-map
      "s" 'dired-sort-toggle-or-edit))

  ;; 打开链接 o
  (evil-define-key 'normal help-mode-map
    "o" 'link-hint-open-link))


;; 使用vim的surround来包围字符串
;; 用法：yss)  ds)  cs'{  ysiw  csiw  S
(use-package evil-surround
  :hook (after-init . global-evil-surround-mode))


;; evil 延迟启动，在evil之后再启动
(use-package evil-nerd-commenter
  :after evil
  :init
  (define-key evil-normal-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)
  (define-key evil-visual-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)

  ;; (evilnc-default-hotkeys)
  )

(use-package evil-snipe
  :hook ((after-init . evil-snipe-mode)
         (after-init . evil-snipe-override-mode))
  :diminish)


(use-package evil-matchit
  :hook (after-init . global-evil-matchit-mode ))


(provide 'init-evil)

;;; init-evil.el ends here
