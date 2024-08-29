;;; init-eaf.el --- eaf -*- lexical-binding: t -*-

;; Author: shaowen he

;;; Commentary:

;; Configuration for eaf

;;; Code:

;; EAF的安装与使用情况：
;; 1. 完美支持情况：Linux-》Windows-》macOS （macOS下是最差的，焦点问题及其他问题）
;; 2. 试用了下，因为平台问题，并不好用，只能勉强打开，还找不到 eaf-evil，无法更换绑定
;; 3. 显示错位的问题，在哪一个应用中都存在，需要切换一下应用再回来才显示正常，之前错位时可以按键，位置正常后，按键失效
;; 4. 文件管理：打开时位置错位，可以选择，也快速，还可预览。切换下位置正常了，但无法选择，按键失效
;; 5. 基于以下原因：只能暂时去掉 （从submodule中删除）


;; (use-package eaf
;;   :load-path "~/.emacs.d/site-lisp/emacs-application-framework"
;;   :custom
;;   ; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
;;   ;; Make `eaf-browser-restore-buffers' restore last close browser buffers.
;;   (eaf-browser-continue-where-left-off t)
;;   (eaf-browser-enable-adblocker t)
;;   (browse-url-browser-function 'eaf-open-browser)
;;   :config
;;   (defalias 'browse-web #'eaf-open-browser)
;;   (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
;;   (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
;;   (eaf-bind-key take_photo "p" eaf-camera-keybinding)
;;   (eaf-bind-key nil "M-q" eaf-browser-keybinding)) ;; unbind, see more in the Wiki


;; 放在此处添加路径，不会拖慢启动速度，可正常启动与使用
;; 功能已经正常，但是适配性有点差
(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
(require 'eaf)

;; (require 'eaf-evil)
;; (setq eaf-evil-leader-key "SPC")

(require 'eaf-demo)
(require 'eaf-browser)
(require 'eaf-pdf-viewer)
;; (require 'eaf-markdown-previewer)
;; (require 'eaf-markmap)
;; (require 'eaf-js-video-player)
;; (require 'eaf-video-player)
;; (require 'eaf-image-viewer)
;; (require 'eaf-org-previewer)
;; (require 'eaf-mindmap)
;; (require 'eaf-mail)
;; (require 'eaf-camera)
;; (require 'eaf-jupyter)
;; (require 'eaf-music-player)
;; (require 'eaf-system-monitor)
(require 'eaf-file-manager)
;; (require 'eaf-file-browser)
;; (require 'eaf-rss-reader)
(require 'eaf-git)
;; (require 'eaf-map)
(require 'eaf-pyqterminal)

;; (require 'popweb-dict)

;;; Code:

;; You need configuration your own local proxy program first.
(setq eaf-proxy-type "socks5")
(setq eaf-proxy-host "127.0.0.1")
(setq eaf-proxy-port "7890")

;; browser 键设定
(eaf-bind-key undo_action "C-/" eaf-browser-keybinding)
(eaf-bind-key redo_action "C-?" eaf-browser-keybinding)
(eaf-bind-key scroll_up "M-j" eaf-browser-keybinding)
(eaf-bind-key scroll_down "M-k" eaf-browser-keybinding)
(eaf-bind-key scroll_up_page "M-n" eaf-browser-keybinding)
(eaf-bind-key scroll_down_page "M-p" eaf-browser-keybinding)
(eaf-bind-key open_link "M-h" eaf-browser-keybinding)
(eaf-bind-key open_link_new_buffer "M-H" eaf-browser-keybinding)
(eaf-bind-key insert_or_open_link_new_buffer "D" eaf-browser-keybinding)
(eaf-bind-key insert_or_open_link_background_buffer "F" eaf-browser-keybinding)
(eaf-bind-key watch-other-window-up-line "M-<" eaf-browser-keybinding)
(eaf-bind-key watch-other-window-down-line "M->" eaf-browser-keybinding)
(eaf-bind-key emacs-session-save "<f5>" eaf-browser-keybinding)
(eaf-bind-key refresh_page "M-r" eaf-browser-keybinding)

(setq eaf-webengine-default-zoom 1.5)
(setq eaf-browser-aria2-proxy-host "127.0.0.1")
(setq eaf-browser-aria2-proxy-port "7890")
(setq eaf-browser-enable-adblocker nil)
(setq eaf-browser-enable-autofill t)
(setq eaf-marker-letters "JKHLNMUIOYPFDSAVCRREW")
;; (setq eaf-terminal-font-size 18)
(setq eaf-webengine-font-family "WenQuanYi Micro Hei Mono")
(setq eaf-webengine-fixed-font-family "WenQuanYi Micro Hei Mono")
(setq eaf-webengine-serif-font-family "TsangerJinKai03-6763")
(setq eaf-webengine-font-size 18)
(setq eaf-webengine-fixed-font-size 18)
;; (setq eaf-terminal-font-family "WenQuanYi Micro Hei Mono")
;; (setq eaf-jupyter-font-family "WenQuanYi Micro Hei Mono")
(setq eaf-file-manager-show-hidden-file nil)
(setq eaf-pyqterminal-font-family "FiraCode Nerd Font Mono")
(setq eaf-pyqterminal-font-size 24)
;; (setq eaf-jupyter-font-family "FiraCode Nerd Font Mono")
(setq eaf-rebuild-buffer-after-crash nil)
(setq eaf-pyqterminal-refresh-ms 16)
(setq eaf-pdf-show-progress-on-page nil)

(setq eaf-pdf-dark-mode "ignore")

(setq eaf-goto-right-after-close-buffer t)

;; (setq eaf-enable-debug t)
;; (global-set-key (kbd "s-x s-x") (lambda () (interactive) (message "%s" (with-current-buffer eaf-name (buffer-string)))))

;; (defun eaf-goto-left-tab ()
;;   (interactive)
;;   (sort-tab-select-prev-tab))

;; (defun eaf-goto-right-tab ()
;;   (interactive)
;;   (sort-tab-select-next-tab))

;; (defun eaf-translate-text (text)
;;   (popweb-dict-bing-input text))


;; (one-key-create-menu
;;  "GIT"
;;  '(
;;    (("s" . "Git status") . eaf-open-git)
;;    (("u" . "Git push to remote") . eaf-git-push)
;;    (("i" . "Git pull") . eaf-git-pull)
;;    (("b" . "Git submodule pull") . eaf-git-submodule-pull)
;;    (("c" . "Git clone") . eaf-git-clone)
;;    (("h" . "Git history") . eaf-git-show-history)
;;    )
;;  t)

;; pyqterminal 键设定
(eaf-bind-key eaf-send-backspace-key "M-o" eaf-pyqterminal-keybinding)
(eaf-bind-key scroll_up "M-," eaf-pyqterminal-keybinding)
(eaf-bind-key scroll_down "M-." eaf-pyqterminal-keybinding)
(eaf-bind-key eaf-open-in-file-manager "M-j" eaf-pyqterminal-keybinding)

(defun eaf-open-terminal ()
  "Try to open fish if fish exist, otherwise use default shell."
  (interactive)
  (eaf-pyqterminal-run-command-in-dir
   (if (executable-find "fish")
       "fish"
     (eaf--generate-terminal-command))
   (eaf--non-remote-default-directory)
   t))


(provide 'init-eaf)

;;; init-eaf.el ends here
