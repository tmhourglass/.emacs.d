;;; init-web.el --- web -*- lexical-binding: t -*-

;; Author: shaowen he

;;; Commentary:

;; commentary

;;; Code:


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

(use-package emmet-mode)




;; 设置各个web模式下的缩进偏移量，统一 （暂时未用到）
(defun my-toggle-web-indent ()
  (interactive)
  ;; web development
  (if (or (eq major-mode 'js-mode) (eq major-mode 'js2-mode))
      (progn
	(setq js-indent-level (if (= js-indent-level 2) 4 2))
	(setq js2-basic-offset (if (= js2-basic-offset 2) 4 2))))

  (if (eq major-mode 'web-mode)
      (progn (setq web-mode-markup-indent-offset (if (= web-mode-markup-indent-offset 2) 4 2))
	     (setq web-mode-css-indent-offset (if (= web-mode-css-indent-offset 2) 4 2))
	     (setq web-mode-code-indent-offset (if (= web-mode-code-indent-offset 2) 4 2))))
  (if (eq major-mode 'css-mode)
      (setq css-indent-offset (if (= css-indent-offset 2) 4 2)))
)




(provide 'init-web)

;;; init-web.el ends here
