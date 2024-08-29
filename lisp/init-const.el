;;; init-const.el --- const -*- lexical-binding: t -*-

;; Author: shaowen he

;;; Commentary:

;; define constants

;;; Code:

;; 常用目录定义
(setq org-directory "~/org-notes/")
(defvar blog-admin-dir "~/org-notes/my-blog/" "blog-admin files location")


;; 系统类型变量
(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

;; windows-system 由之前的mac，变为了ns
(defconst sys/macp
  (or (eq system-type 'darwin)
      (eq window-system 'ns))
  "Are we running on a Mac system?")

;; 自定义变量，展示图标与否，后面函数有判断
(defcustom display-icon-p t
  "Display icons or not."
  :group 'tmhourglass
  :type 'boolean)


(provide 'init-const)

;;; init-const.el ends here
