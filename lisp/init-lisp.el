;;; init-lisp.el --- lisp -*- lexical-binding: t -*-

;; Author: shaowen he

;;; Commentary:

;; commentary

;;; Code:

;; 使用e直接执行表达式，elisp使用lispy-mode
;; 主要的操作方法在这个里面
;; 功能很多，常用的：移动hljk 对应括号跳转-d 向内或向外移动-f（d到尾部按f进入内层括号，d到开头按f相反）
;; m-标记区块，可hljk上下移动，< or > 可调整当前代码块的层级，b-返回之前操作
;; r-提高，只保留当前块，其他删除 i-美化代码0-格式化为一行 M-格式化为多行
(use-package lispy
  :hook (emacs-lisp-mode . lispy-mode)
  :config
  (lispy-define-key lispy-mode-map "e" 'eval-last-sexp))


;; 使用evil的快捷键来操作lisp，只是优化了相关的evil操作
;; 比如注释合并为一行，或者内容合并，能够识别代码与注释
;; lispy + evil = lispyville
(use-package lispyville
  :hook (lispy-mode . lispyville-mode))


;; Emacs 的交互式宏扩展器
(use-package macrostep)


;; 更好的 Emacs *帮助* 缓冲区
;; 快捷键统一维护到keybindings中
(use-package helpful)


;; (use-package help
;;   :defer t
;;   :custom (help-window-select t)
;;   :config (temp-buffer-resize-mode))


;; 演示 Emacs Lisp API，即help帮助文档
;; Demos
(use-package elisp-demos
  :init
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))


;; 突出elisp的引用，比如函数前缀 #'，有效果的
(use-package highlight-quoted
  :hook (emacs-lisp-mode . highlight-quoted-mode))



(provide 'init-lisp)

;;; init-lisp.el ends here
