;;; init-javascript.el --- javascript -*- lexical-binding: t -*-

;; Author: shaowen he

;;; Commentary:

;; commentary

;;; Code:

(use-package json-mode
  :init
  ;; https://www.emacswiki.org/emacs/AutoModeAlist
  ;; \\' means the end of the file
  (add-to-list 'auto-mode-alist '("Puffer\\'" . json-mode))
  :hook (json-mode . flycheck-mode))



;; 有点耗时，并且暂时基本用不到，先注释掉
;; 用于编写javascript的模式
;; (use-package js2-mode
;;   :config
;;   (defun js2-imenu-make-index ()
;;     (interactive)
;;     (save-excursion
;;       ;; (setq imenu-generic-expression '((nil "describe\\(\"\\(.+\\)\"" 1)))
;;       (imenu--generic-function '(("describe" "\\s-*describe\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
;;                                  ("it" "\\s-*it\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
;;                                  ("test" "\\s-*test\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
;;                                  ("before" "\\s-*before\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
;;                                  ("after" "\\s-*after\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
;;                                  ("Function" "function[ \t]+\\([a-zA-Z0-9_$.]+\\)[ \t]*(" 1)
;;                                  ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*=[ \t]*function[ \t]*(" 1)
;;                                  ("Function" "^var[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*=[ \t]*function[ \t]*(" 1)
;;                                  ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*()[ \t]*{" 1)
;;                                  ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*:[ \t]*function[ \t]*(" 1)
;;                                  ("Task" "[. \t]task([ \t]*['\"]\\([^'\"]+\\)" 1)))))
;;   (add-hook 'js2-mode-hook
;;             (lambda ()
;;               (setq imenu-create-index-function 'js2-imenu-make-index)))



;;   (add-hook 'js2-mode-hook 'flycheck-mode))


;; (use-package typescript-ts-mode
;;   :init
;;   ;; Associate cts files with `typescript-ts-mode'.
;;   (add-to-list 'auto-mode-alist (cons "\\.cts\\'" 'typescript-ts-mode))
;;   (add-hook 'typescript-ts-mode-hook #'eglot-ensure)
;;   :custom (typescript-ts-mode-indent-offset 4)
;;   :config
;;   (define-key typescript-ts-mode-map (kbd "RET") 'av/auto-indent-method-maybe))


(provide 'init-javascript)

;;; init-javascript.el ends here
