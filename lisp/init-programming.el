;;; init-programming.el --- programming -*- lexical-binding: t -*-

;; Author: shaowen he

;;; Commentary:

;; commentary

;;; Code:

;; treesit 是对tree-sitter的实现，后者是一个解析器生成工具和增量解析库
;; 实时分析代码，构建语法树，提供语法高亮及结构化编辑
(use-package treesit-auto
  :demand t
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

;; todo-borg
(setq treesit-language-source-alist
      '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
        (c . ("https://github.com/tree-sitter/tree-sitter-c"))
        (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
        (css . ("https://github.com/tree-sitter/tree-sitter-css"))
        (cmake . ("https://github.com/uyha/tree-sitter-cmake"))
        (csharp     . ("https://github.com/tree-sitter/tree-sitter-c-sharp.git"))
        (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
        (elisp . ("https://github.com/Wilfred/tree-sitter-elisp"))
        (go . ("https://github.com/tree-sitter/tree-sitter-go"))
        (gomod      . ("https://github.com/camdencheek/tree-sitter-go-mod.git"))
        (html . ("https://github.com/tree-sitter/tree-sitter-html"))
        (java       . ("https://github.com/tree-sitter/tree-sitter-java.git"))
        (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
        (json . ("https://github.com/tree-sitter/tree-sitter-json"))
        (lua . ("https://github.com/Azganoth/tree-sitter-lua"))
        (make . ("https://github.com/alemuller/tree-sitter-make"))
        (markdown . ("https://github.com/MDeiml/tree-sitter-markdown" nil "tree-sitter-markdown/src"))
        (ocaml . ("https://github.com/tree-sitter/tree-sitter-ocaml" nil "ocaml/src"))
        (org . ("https://github.com/milisims/tree-sitter-org"))
        (python . ("https://github.com/tree-sitter/tree-sitter-python"))
        (php . ("https://github.com/tree-sitter/tree-sitter-php"))
        (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
        (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
        (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
        (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
        (sql . ("https://github.com/m-novikov/tree-sitter-sql"))
        (vue . ("https://github.com/merico-dev/tree-sitter-vue"))
        (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))
        (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
        (zig . ("https://github.com/GrayJack/tree-sitter-zig"))))


;;; Tree-sitter support
;; https://git.savannah.gnu.org/cgit/emacs.git/tree/admin/notes/tree-sitter/starter-guide?h=emacs-29
(use-package treesit
  :when (and (fboundp 'treesit-available-p)
             (treesit-available-p))
  :custom
  (major-mode-remap-alist
   '((c-mode          . c-ts-mode)
     (c++-mode        . c++-ts-mode)
     (csharp-mode     . csharp-ts-mode)
     (conf-toml-mode  . toml-ts-mode)
     (css-mode        . css-ts-mode)
     (java-mode       . java-ts-mode)
     (javascript-mode . js-ts-mode)
     (js-json-mode    . json-ts-mode)
     (python-mode     . python-ts-mode)
     (ruby-mode       . ruby-ts-mode)
     (sh-mode         . bash-ts-mode)))
  (c-ts-mode-indent-style 'linux)
  (c-ts-mode-indent-offset 8)
  :config
  (add-to-list 'auto-mode-alist '("\\(?:CMakeLists\\.txt\\|\\.cmake\\)\\'" . cmake-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode)))


;; 格式化命令
(use-package reformatter
  :config
  (reformatter-define black :program "black" :args '("-") :group 'reformatter)
  (reformatter-define blue :program "blue" :args '("-") :group 'reformatter)
  (reformatter-define js-beautify :program "js-beautify" :group 'reformatter)
  (reformatter-define html-beautify :program "html-beautify" :group 'reformatter)
  (reformatter-define css-beautify :program "css-beautify" :group 'reformatter)
  (reformatter-define hindent :program "hindent" :lighter " Hin" :group 'reformatter)
  (reformatter-define ormolu :program "ormolu" :lighter " Orm"
    :args `("--stdin-input-file" ,buffer-file-name) :group 'reformatter))


;; 使用 RCS 补丁和动态编程在缓冲区内容上运行代码格式化程序，无需移动点
(use-package apheleia
  :bind ("C-c f" . apheleia-format-buffer)
  :config
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(isort black)))


;; 编码过程中可能用到的几个模式，根据使用情况添加，没用到的去掉，减少加载

(use-package yaml-mode)


(use-package json-mode
  :init
  ;; https://www.emacswiki.org/emacs/AutoModeAlist
  ;; \\' means the end of the file
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
  :hook (json-mode . flycheck-mode))


;; 一种次要模式：可猜测最初创建源代码文件的缩进偏移量，并透明调整设置
(use-package dtrt-indent
  :diminish
  :hook (prog-mode . dtrt-indent-mode))


;; 指定不同文件类型对应的mode (auto-mode-alist)
(setq auto-mode-alist
      (append
       '(
         ("\\.js\\'" . js-mode)
         ;; ("\\.vue\\'" . web-mode)
         ("\\.html\\'" . web-mode)
         ("\\.wxml\\'" . web-mode)
         ("\\.html.eex\\'" . web-mode))
       auto-mode-alist))


;; web相关
(use-package web-mode
  :init
  ;; config for web mode
  (defun my-web-mode-indent-setup ()
    (setq web-mode-markup-indent-offset 2) ; web-mode, html tag in html file
    (setq web-mode-css-indent-offset 2)    ; web-mode, css in html file
    (setq web-mode-code-indent-offset 2)   ; web-mode, js code in html file
    )
  :config
  (add-hook 'web-mode-hook 'my-web-mode-indent-setup))


;; Emmet
(use-package emmet-mode)




(provide 'init-programming)

;;; init-programming.el ends here
