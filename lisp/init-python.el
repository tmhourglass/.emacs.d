;;; init-python.el --- python -*- lexical-binding: t -*-

;; Author: shaowen he

;;; Commentary:

;; commentary

;;; Code:

;; Python Mode
;; Install: pip install pyflakes autopep8
(use-package python
  :hook (inferior-python-mode . (lambda ()
                                  (process-query-on-exit-flag
                                   (get-process "Python"))))
  :init
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil)
  :config
  (global-leader
    :major-modes
    '(python-mode t)
    ;;and the keymaps:
    :keymaps
    '(python-mode-map)
    "e" 'live-py-set-version)

  ;; tmhourglass add : fix python-add-import
  (setq python-interpreter "python3")
  (setq python-shell-interpreter "python3")
  (add-hook 'python-ts-mode-hook #'eglot-ensure)

  ;; 当缩进猜测失败时发出警告 nil-不发出警告
  (setq python-indent-guess-indent-offset-verbose nil))

;; Live Coding in Python
;; https://www.emacswiki.org/emacs/PythonProgrammingInEmacs
;; 暂时没用到，功能待定，安装包也没找到，也未报错
;; todo-borg
;; (use-package live-py-mode)

;; virtual envrionment
(use-package auto-virtualenvwrapper
  :hook (python-mode-hook . #'auto-virtualenvwrapper-activate))


;; python虚拟环境
(use-package pyvenv
  :hook (python-mode . pyvenv-mode)
  :config
  (setenv "WORKON_HOME" (expand-file-name "~/workspace/code/python/envs")))




(provide 'init-python)

;;; init-python.el ends here
