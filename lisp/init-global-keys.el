;;; init-global-keys.el --- global-keys -*- lexical-binding: t -*-

;; Author: shaowen he

;;; Commentary:

;; 全局通用快捷键

;;; Code:


(use-package which-key
  :hook (after-init . which-key-mode)
  :init
  (setq which-key-side-window-location 'bottom))



;; 以下有另一种按键
(global-set-key "\C-s" 'consult-line)
(global-set-key (kbd "M-y") 'consult-yank-pop)
(global-set-key (kbd "<f2>") 'open-my-init-file)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)


;; 帮助文档 (helpful有另一种按键及方法）
(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)
(global-set-key (kbd "C-h C-k") 'find-function-on-key)
(global-set-key (kbd "s-1") 'lispy-describe-inline)

;; helpful
;; 替换原生的，describe-function包含函数和宏，而helpful-callable只包含函数
(global-set-key (kbd "C-h f") #'helpful-callable) ;仅函数
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
;; 查找当前位置的symbol，在lisp mode中很常用
(global-set-key (kbd "C-c C-d") #'helpful-at-point)
;; 查找函数，排除宏
(global-set-key (kbd "C-h F") #'helpful-function)
(global-set-key (kbd "C-h C") #'helpful-command)


;; 使用拼音来进行中文搜索  -- 之前生效，borg中不生效了
(global-set-key (kbd "s-p") 'toggle-chinese-search)


;; mimic macos keybindgs 模仿macOS按键
(global-set-key (kbd "s-x") 'kill-region)
(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-z") 'undo)
(global-set-key (kbd "s-l") 'goto-line)
(global-set-key (kbd "s-q") 'save-buffers-kill-emacs)
(global-set-key (kbd "s-a") 'mark-whole-buffer)
(global-set-key (kbd "s-s") 'save-buffer)


;; agenda
(global-set-key (kbd "C-c a") 'org-agenda)

;; 若选中区域，则对区域缩进，否则对整个buffer缩进
(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)

;; 输入法中英文切换
;; (define-key global-map (kbd "s-e") 'rime-inline-ascii)
(define-key global-map (kbd "s-e") 'sis-switch)

;; 扩展完成：可使用tab，也可使用 s-/
(global-set-key (kbd "s-/") 'hippie-expand)


;; r aka remember
(global-set-key (kbd "C-c r") 'org-capture)


;; 这个使用方式待定
(global-set-key (kbd "M-s e") 'iedit-mode)


;; 向前删除一个单词，可替换M-del
(global-set-key (kbd "C-w") 'backward-kill-word)
;; 恰当的，后续remap为consult-apropos
(global-set-key (kbd "C-h a") 'apropos)


;; 还原整个buffer
(global-set-key (kbd "s-r") #'my/revert-this-buffer)

;; 扩展选区 (很好用，可替换v来选择，可一层层扩展选区，可多按几次)
(global-set-key (kbd "s-d") 'my/my-mc-mark-next-like-this)
;; 有另一个按键 s-d
(global-set-key (kbd "C-=") 'er/expand-region)

;; 插入当前chrome的tab url
(global-set-key (kbd "C-c l") 'my/insert-chrome-current-tab-url)




(provide 'init-global-keys)

;;; init-global-keys.el ends here
