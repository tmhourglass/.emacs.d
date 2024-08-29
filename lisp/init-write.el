;;; init-write.el --- write -*- lexical-binding: t -*-

;; Author: shaowen he

;;; Commentary:

;; commentary

;;; Code:


;; 输入法切换 sis/rime用其一
(use-package sis
  :demand t
  ;; :bind ("<f10>" . sis-switch)   ; s-e global keybings
  :config
  (add-to-list 'sis-prefix-override-keys "M-s")
  (add-to-list 'sis-prefix-override-keys "M-g")
  (when sys/macp
    (sis-ism-lazyman-config "com.apple.keylayout.ABC" "im.rime.inputmethod.Squirrel.Hans"))
  (when (eq system-type 'gnu/linux)
    (sis-ism-lazyman-config "1" "2" 'fcitx5))

  ;; 默认英文的光标颜色为黑色，看不清 -- 修改
  (setq sis-default-cursor-color "#5f9ea0")

  ;; 非英文的光标颜色为桔色
  (setq sis-other-cursor-color "orange")
  (sis-global-cursor-color-mode t)
  (sis-global-respect-mode t))


;; minibuffer中s-p启用拼音搜索，退出时禁用
(use-package pyim
  :commands (pyim-cregexp-build)
  :init
  (defun eh-orderless-regexp (orig_func component)
    (let ((result (funcall orig_func component)))
      (pyim-cregexp-build result)))

  ;; 切换为中文拼音搜索 -- 失效中
  (defun toggle-chinese-search ()
    (interactive)
    (if (not (advice-member-p #'eh-orderless-regexp 'orderless-regexp))
        (advice-add 'orderless-regexp :around #'eh-orderless-regexp)
      (advice-remove 'orderless-regexp #'eh-orderless-regexp)))

  (defun disable-py-search (&optional args)
    (if (advice-member-p #'eh-orderless-regexp 'orderless-regexp)
        (advice-remove 'orderless-regexp #'eh-orderless-regexp)))

  ;; 退出minibuffer后，禁用拼音搜索
  (add-hook 'minibuffer-exit-hook 'disable-py-search))


;; 智能选择区域
;; Smartly select region, rectangle, multi cursors
(use-package smart-region
  :hook (after-init . smart-region-on))

;; 这个centaur的配置不会卡起，正常启动
;; On-the-fly spell checker
(use-package flyspell
  :diminish
  :if (executable-find "aspell")
  :hook (((text-mode outline-mode) . flyspell-mode)
         ;; (prog-mode . flyspell-prog-mode)
         (flyspell-mode . (lambda ()
                            (dolist (key '("C-;" "C-," "C-."))
                              (unbind-key key flyspell-mode-map)))))
  :init (setq flyspell-issue-message-flag nil
              ispell-program-name "aspell"
              ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together")))


;; 将ispell换成enchant （这个是aspell的升级版，使用brew安装）
;; 以下代码：会让启动时卡起，实际启动时间并没有增加，加载字典？？？
;; 参考eason0210
;; (use-package flyspell
;;   :diminish
;;   :hook ((prog-mode . flyspell-prog-mode)
;;          ((git-commit-mode markdown-mode) . flyspell-mode)
;;          ;; 拼写检查模式下，禁用以下两个按键
;;          (flyspell-mode . (lambda ()
;;                             (dolist (key '("C-;" "C-."))
;;                               (unbind-key key flyspell-mode-map)))))
;;   :custom
;;   (flyspell-issue-message-flag nil)
;;   (ispell-program-name "enchant-2")
;;   (ispell-dictionary "english")
;;   (text-mode-ispell-word-completion nil))


(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map
              ("C-," . flyspell-correct-wrapper)))


;; 英文助手
;; 通过url安装同化 https://github.com/manateelazycat/corfu-english-helper
(use-package corfu-english-helper
  :commands (toggle-corfu-english-helper))


(use-package ox-hugo
  :after ox
  :commands org-hugo-export-to-md)


;; 适用于 Org Export 引擎的 Github 风味 Markdown 后端
(use-package ox-gfm
  :after org
  :custom (org-export-with-toc nil))


;; markdown模式
(use-package markdown-mode
  :mode (("\\.md\\.html\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode)))


(use-package pangu-spacing
  ;; 在org-mode中总是插入真正的空格
  :hook ((org-mode-hook . (lambda ()
                            (setq-local pangu-spacing-real-insert-separtor t)))
         (after-init . global-pangu-spacing-mode)))


(provide 'init-write)

;;; init-write.el ends here
