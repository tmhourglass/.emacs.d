;;; init-completion.el --- completion -*- lexical-binding: t -*-

;; Author: shaowen he

;;; Commentary:

;; commentary

;;; Code:


(use-package company)

(defun nasy/orderless-dispatch-flex-first (_pattern index _total)
  "orderless-flex for corfu."
  (and (eq index 0) 'orderless-flex))

(defun nasy/setup-corfu ()
  "Setup corfu."
  (setq-local orderless-matching-styles '(orderless-flex)
              orderless-style-dispatchers nil)
  (add-hook 'orderless-style-dispatchers #'nasy/orderless-dispatch-flex-first nil 'local))

;; use corfu instead
(when (display-graphic-p)
  (use-package corfu
    :init
    (setq corfu-cycle t)
    (setq corfu-auto t)
    (setq corfu-quit-at-boundary t)
    (setq corfu-quit-no-match t)
    (setq corfu-preview-current nil)
    (setq corfu-min-width 80)
    (setq corfu-max-width 100)
    (setq corfu-auto-delay 0.1)
    (setq corfu-auto-prefix 1)
    (setq corfu-on-exact-match nil)
    (global-corfu-mode)
    ;; 自动完成时，弹出帮助文档
    (corfu-popupinfo-mode)

    :hook (prog-mode . nasy/setup-corfu)
    :config
    ;; (defun corfu-enable-in-minibuffer ()
    ;;   "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    ;;   (when (where-is-internal #'completion-at-point (list (current-local-map)))
    ;;     ;; (setq-local corfu-auto nil) Enable/disable auto completion
    ;;     (corfu-mode 1)))
    ;; (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)

    (defun corfu-move-to-minibuffer ()
      (interactive)
      (let ((completion-extra-properties corfu--extra)
            completion-cycle-threshold completion-cycling)
        (toggle-chinese-search)
        (apply #'consult-completion-in-region completion-in-region--data)))
    (define-key corfu-map "\M-m" #'corfu-move-to-minibuffer)

    (define-key corfu-map (kbd "C-j") 'corfu-next)
    (define-key corfu-map (kbd "C-k") 'corfu-previous)
    (setq corfu-popupinfo-delay 0.4)
    (setq corfu-popupinfo-max-width 120)
    (setq corfu-popupinfo-max-height 40)
    (define-key corfu-map (kbd "s-d") 'corfu-popupinfo-toggle)
    (define-key corfu-map (kbd "s-p") #'corfu-popupinfo-scroll-down) ;; corfu-next
    (define-key corfu-map (kbd "s-n") #'corfu-popupinfo-scroll-up) ;; corfu-previous
    )


  ;; Use dabbrev with Corfu!
  (use-package dabbrev
    ;; Swap M-/ and C-M-/
    :bind (("M-/" . dabbrev-completion)
           ("C-M-/" . dabbrev-expand)))

  ;; A few more useful configurations...
  (use-package emacs
    :init
    ;; TAB cycle if there are only few candidates
    (setq completion-cycle-threshold 3)

    ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
    ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
    ;; (setq read-extended-command-predicate
    ;;       #'command-completion-default-include-p)

    ;; Enable indentation+completion using the TAB key.
    ;; `completion-at-point' is often bound to M-TAB.
    (setq tab-always-indent 'complete))

  ;; Add extensions
  (use-package cape
    ;; Bind dedicated completion commands
    :bind (("C-c p p" . completion-at-point) ;; capf
           ("C-c p t" . complete-tag)        ;; etags
           ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
           ("C-c p f" . cape-file)
           ("C-c p k" . cape-keyword)
           ("C-c p s" . cape-symbol)
           ("C-c p a" . cape-abbrev)
           ("C-c p i" . cape-ispell)
           ("C-c p l" . cape-line)
           ("C-c p w" . cape-dict)
           ("C-c p \\" . cape-tex)
           ("C-c p _" . cape-tex)
           ("C-c p ^" . cape-tex)
           ("C-c p &" . cape-sgml)
           ("C-c p r" . cape-rfc1345))
    :init
    (setq cape-dabbrev-min-length 3)
    ;; Add `completion-at-point-functions', used by `completion-at-point'.
    (add-to-list 'completion-at-point-functions #'cape-file)
    (add-to-list 'completion-at-point-functions #'cape-tex)
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    (setq cape-dabbrev-check-other-buffers nil)
    (add-to-list 'completion-at-point-functions #'cape-keyword)
    (defun my/eglot-capf ()
      (setq-local completion-at-point-functions
                  (list (cape-capf-super
                         #'eglot-completion-at-point
                         (cape-company-to-capf #'company-yasnippet)))))

    (add-hook 'eglot-managed-mode-hook #'my/eglot-capf)))


(use-package nerd-icons-corfu
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))



;; 路径中按删除键，可以一个区块一个区块的删除
;; cleans ~/foo/bar/// to /, and ~/foo/bar/~/ to ~/.
;; 定制移动选择的快捷键
(use-package vertico
  :hook (after-init . vertico-mode)
  :config
  (setq vertico-resize nil
        vertico-count 17
        vertico-cycle t)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (define-key vertico-map (kbd "C-j") 'vertico-next)
  (define-key vertico-map (kbd "C-'") 'vertico-quick-jump)
  (define-key vertico-map (kbd "C-k") 'vertico-previous)
  (define-key vertico-map [backspace] #'vertico-directory-delete-char)
  ;; (define-key vertico-map (kbd "s-SPC") #'+vertico/embark-preview)

  )


(use-package orderless
  :demand t
  ;;       orderless-component-separator "[ &]")
  ;; ...otherwise find-file gets different highlighting than other commands
  ;; (set-face-attribute 'completions-first-difference nil :inherit nil)
  :config
  (defvar +orderless-dispatch-alist
    '((?% . char-fold-to-regexp)
      (?! . orderless-without-literal)
      (?`. orderless-initialism)
      (?= . orderless-literal)
      (?~ . orderless-flex)))

  (defun +orderless-dispatch (pattern index _total)
    (cond
     ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" pattern)
      `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x100000-\x10FFFD]*$")))
     ;; File extensions
     ((and
       ;; Completing filename or eshell
       (or minibuffer-completing-file-name
           (derived-mode-p 'eshell-mode))
       ;; File extension
       (string-match-p "\\`\\.." pattern))
      `(orderless-regexp . ,(concat "\\." (substring pattern 1) "[\x100000-\x10FFFD]*$")))
     ;; Ignore single !
     ((string= "!" pattern) `(orderless-literal . ""))
     ;; Prefix and suffix
     ((if-let (x (assq (aref pattern 0) +orderless-dispatch-alist))
          (cons (cdr x) (substring pattern 1))
        (when-let (x (assq (aref pattern (1- (length pattern))) +orderless-dispatch-alist))
          (cons (cdr x) (substring pattern 0 -1)))))))

  ;; Define orderless style with initialism by default
  (orderless-define-completion-style +orderless-with-initialism
                                     (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))

  (setq completion-styles '(orderless partial-completion)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)) ;; partial-completion is tried first
                                        (command (styles +orderless-with-initialism))
                                        (variable (styles +orderless-with-initialism))
                                        (symbol (styles +orderless-with-initialism)))
        orderless-component-separator #'orderless-escapable-split-on-space ;; allow escaping space with backslash!
        orderless-style-dispatchers '(+orderless-dispatch))
  )


;; 参考eason0210
(use-package consult
  :defer 0.5
  ;; 重新映射相关命令，统一使用consult的
  :bind (([remap repeat-complex-command] . consult-complex-command)
         ([remap switch-to-buffer] . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
         ([remap project-switch-to-buffer] . consult-project-buffer)
         ([remap bookmark-jump] . consult-bookmark)
         ([remap goto-line] . consult-goto-line)
         ([remap imenu] . consult-imenu)
         ([remap yank-pop] . consult-yank-pop)
         ([remap Info-search] . consult-info))

  :custom
  (register-preview-delay 0.5)
  (register-preview-function #'consult-register-format)
  (consult-narrow-key "<")
  (consult-line-numbers-widen t)
  (consult-async-min-input 2)
  (consult-async-refresh-delay 0.15)
  (consult-async-input-throttle 0.2)
  (consult-async-input-debounce 0.1)
  (consult-line-start-from-top t)       ;从最上面开始搜索
  ;; 重定义xref相关行为
  (xref-search-program 'ripgrep)
  ;; 更改为consult-xref后，样式变了
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  :commands consult--customize-put
  :init
  ;; 使用consult修改相关函数行为
  (advice-add #'register-preview :override #'consult-register-window)
  (advice-add #'multi-occur :override #'consult-multi-occur)
  ;; 最后一个参数 '((name . "wrapper")) 暂时没用
  (advice-add #'consult-line :around #'my/consult-line '((name . "wrapper")))
  :config
  (consult-customize
   ;; 预览主题已可用：删除之前绑定的按键，解决key-valid-p
   consult-theme :preview-key '(:debounce 0.2 any)
   ;; 以下这些没有配置
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file
   ;; 仅针对项目搜索时 `SPC s p`，不会实时预览，若需要预览按以下绑定键即可进行预览
   ;; M-. 另一个绑定操作为evil-repeat-pop-next （暂时未用到）
   consult--source-project-recent-file :preview-key "M-."))


;; 目录搜索spc f d (s-d按键冲突）
(use-package consult-dir
  :bind (([remap list-directory] . consult-dir)
         :map vertico-map
         ("s-d" . consult-dir)))

(use-package consult-flycheck
  :after (consult flycheck))


(use-package embark
  :defer t
  :init
  (setq which-key-use-C-h-commands nil
        ;; press C-h after a prefix key, it shows all the possible key bindings and let you choose what you want
        prefix-help-command #'embark-prefix-help-command)

  (setq
   embark-verbose-indicator-display-action
   '((display-buffer-at-bottom)
     (window-parameters (mode-line-format . none))
     (window-height . fit-window-to-buffer)))

  (define-key minibuffer-local-map (kbd "C-;") 'embark-act)
  (define-key minibuffer-local-map (kbd "C-c C-;") 'embark-export)
  (define-key minibuffer-local-map (kbd "C-c C-e") '+vertico/embark-export-write)

  (with-eval-after-load 'popwin
    (progn
      (push '(occur-mode :position right :width 100) popwin:special-display-config)
      (push '(grep-mode :position right :width 100) popwin:special-display-config)
      (push '(special-mode :position right :width 100) popwin:special-display-config)))

  (global-set-key (kbd "C-;") 'embark-act)

  :config
  (define-key minibuffer-local-map (kbd "C-'") #'embark-become)
  ;; list all the keybindings in this buffer
  (global-set-key (kbd "C-h B") 'embark-bindings)
  ;; add the package! target finder before the file target finder,
  ;; so we don't get a false positive match.
  :config
  (define-key embark-identifier-map "R" #'consult-ripgrep)
  (define-key embark-identifier-map (kbd "C-s") #'consult-line)
  ;; (define-key embark-region-map "D" 'youdao-dictionary-search-async)

  (define-key embark-file-map (kbd "E") #'consult-directory-externally)
  (define-key embark-file-map (kbd "U") #'consult-snv-unlock)
  (define-key embark-file-map (kbd "H") #'my-calculate-file-md5)
  )


(use-package marginalia
  :hook (after-init . marginalia-mode))


(use-package embark-consult
  :after (embark consult)
  :demand
  :config
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))


(use-package wgrep
  :commands wgrep-change-to-wgrep-mode
  :config (setq wgrep-auto-save-buffer t))



(provide 'init-completion)

;;; init-completion.el ends here
