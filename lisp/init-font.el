;;; init-font.el --- font -*- lexical-binding: t -*-

;; Author: shaowen he

;;; Commentary:

;; commentary

;;; Code:

(defun load-font-setup()
  (cond ((eq window-system 'pgtk)
         (set-face-attribute 'default nil :height 140 :family "WenQuanYi Micro Hei Mono"))
        (t
         (let ((emacs-font-size 16)
               (chinese-font-name  "TsangerJinKai03-6763")
               english-font-name)
           (cond
            ((featurep 'cocoa)
             (setq english-font-name "Monaco"))
            ((string-equal system-type "gnu/linux")
             (setq english-font-name "WenQuanYi Micro Hei Mono")))
           (when (display-grayscale-p)
             (set-frame-font (format "%s-%s" (eval english-font-name) (eval emacs-font-size)))
             (set-fontset-font (frame-parameter nil 'font) 'unicode (eval english-font-name))

             (dolist (charset '(kana han symbol cjk-misc bopomofo))
               (set-fontset-font (frame-parameter nil 'font) charset (font-spec :family (eval chinese-font-name))))
             )))))

;; (load-font-setup)

;; This is hacking to fix Emacs 29 will decrease font after standby.
;; (add-function :after after-focus-change-function #'load-font-setup)

(dolist (hook (list
               'c-mode-common-hook
               'c-mode-hook
               'c++-mode-hook
               'java-mode-hook
               'haskell-mode-hook
               'emacs-lisp-mode-hook
               'lisp-interaction-mode-hook
               'lisp-mode-hook
               'maxima-mode-hook
               'ielm-mode-hook
               'sh-mode-hook
               'makefile-gmake-mode-hook
               'php-mode-hook
               'python-mode-hook
               'js-mode-hook
               'go-mode-hook
               'qml-mode-hook
               'jade-mode-hook
               'css-mode-hook
               'ruby-mode-hook
               'coffee-mode-hook
               'rust-mode-hook
               'qmake-mode-hook
               'lua-mode-hook
               'swift-mode-hook
               'web-mode-hook
               'markdown-mode-hook
               'llvm-mode-hook
               'conf-toml-mode-hook
               'nim-mode-hook
               'typescript-mode-hook
               ))
  (add-hook hook #'(lambda () (load-font-setup))))


(provide 'init-font)

;;; init-font.el ends here
