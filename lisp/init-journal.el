:;;; init-journal.el --- journal -*- lexical-binding: t -*-

;; Author: shaowen he

;;; Commentary:

;; 写日记，按月存放

;;; Code:

(use-package org-journal
  :defer t
  :init
  ;; 基础设置
  (setq org-journal-file-type 'monthly
        org-journal-dir (expand-file-name "~/Documents/journal/")
        org-journal-file-format "%Y-%m.org"
        org-journal-date-format "%Y-%m-%d %A"
        org-journal-date-prefix "* "
        ;; org-journal-time-format "%H:%M "
        org-journal-time-format " "
        org-journal-time-prefix "** "

        ;; 启用自动完成标签
        org-journal-enable-tag-auto-completion t

        ;; 启用搜索功能
        org-journal-enable-search t
        org-journal-search-backend 'rg  ; 或 'swiper, 'helm 等

        ;; 设置日记文件模板
        org-journal-file-header
        (lambda (time)
          (let* ((year (format-time-string "%Y" time))
                 (month (format-time-string "%m" time)))
            (format "#+TITLE: %s 年 %s 月日记\n#+STARTUP: overview\n#+CATEGORY: Journal\n#+OPTIONS: ^:nil toc:t\n\n* "
                    year month)))

        ;; 设置时区为中国时区
        org-journal-time-zone +8)

  ;; 快捷键设置
  ;; calendar中选择日期，+n -n / shift+方向键 / c-f c-b c-a m-a c-e m-e m-{} . o
  (global-set-key (kbd "C-c j d") 'org-journal-new-date-entry)
  (global-set-key (kbd "C-c j n") 'org-journal-new-entry)
  ;; 在org-journal-mode中，可直接使用C-c C-s
  (global-set-key (kbd "C-c j s") 'org-journal-search)
  (global-set-key (kbd "C-c j c") 'org-journal-calendar)
  (global-set-key (kbd "C-c j o") 'org-journal-open-current-journal-file)
  (global-set-key (kbd "C-c j p") 'org-journal-previous-entry)
  (global-set-key (kbd "C-c j f") 'org-journal-next-entry)

  :config
  ;; 自定义函数：快速插入模板
  (defun my/org-journal-insert-template ()
    "插入自定义的日记模板。"
    (interactive)
    (insert "\n\n")
    (insert "** 今日计划\n")
    (insert "- [ ] \n")
    (insert "\n** 工作记录\n")
    (insert "- \n")
    (insert "\n** 学习笔记\n")
    (insert "- \n")
    (insert "\n** 生活琐事\n")
    (insert "- \n")
    (insert "\n** 感悟与思考\n")
    (insert "\n")
    (insert "** 明日计划\n")
    (insert "- \n"))

  (define-key org-journal-mode-map (kbd "C-c i") 'my/org-journal-insert-template)

  ;; 自定义函数：打开本月日记
  (defun my/org-journal-open-monthly ()
    "打开本月的日记文件。"
    (interactive)
    (let ((month-file (format-time-string "%Y-%m.org")))
      (find-file (expand-file-name month-file org-journal-dir))))

  ;; 自定义函数：打开上月日记
  (defun my/org-journal-open-last-month ()
    "打开上月的日记文件。"
    (interactive)
    (let* ((time (time-subtract (current-time) (days-to-time 31)))
           (month-file (format-time-string "%Y-%m.org" time)))
      (find-file (expand-file-name month-file org-journal-dir))))

  ;; 添加到快捷键
  (global-set-key (kbd "C-c j m") 'my/org-journal-open-monthly)
  (global-set-key (kbd "C-c j l") 'my/org-journal-open-last-month)

  ;; 设置每周回顾提醒（周一早上9点）
  (run-at-time "09:00" (* 60 60 24 7)
               (lambda ()
                 (when (and (equal (format-time-string "%u") "1") ; 周一
                            (y-or-n-p "新的一周开始了，是否要写上周回顾？"))
                   (org-journal-new-entry)
                   (insert "\n** 上周回顾\n")
                   (insert "- 成就：\n")
                   (insert "- 不足：\n")
                   (insert "- 改进：\n"))))

  ;; 美化 org-journal 缓冲区
  ;; (add-hook 'org-journal-mode-hook
  ;;           (lambda ()
  ;;             (visual-line-mode 1)  ; 启用自动换行
  ;;             (org-indent-mode 1)   ; 启用缩进
  ;;             (hl-line-mode 1)      ; 高亮当前行
  ;;             (setq truncate-lines nil)  ; 不截断长行
  ;;             (setq line-spacing 2)))    ; 增加行间距

  ;; 设置日记文件加密（可选）
  (defun my/org-journal-encrypt-entry ()
    "加密当前日记条目。"
    (interactive)
    (when (org-at-heading-p)
      (org-encrypt-entry)))

  (defun my/org-journal-decrypt-entry ()
    "解密当前日记条目。"
    (interactive)
    (when (org-at-heading-p)
      (org-decrypt-entry)))

  ;; 如果使用 GPG 加密
  (when org-journal-enable-encryption
    (setq org-journal-encrypt-journal t)
    (add-hook 'org-journal-mode-hook
              (lambda ()
                (define-key org-journal-mode-map (kbd "C-c e") 'my/org-journal-encrypt-entry)
                (define-key org-journal-mode-map (kbd "C-c d") 'my/org-journal-decrypt-entry))))

  ;; 集成 org-agenda
  (setq org-journal-enable-agenda-integration t)
  (when org-journal-enable-agenda-integration
    (add-to-list 'org-agenda-files org-journal-dir))

  ;; 集成 org-capture（可选）
  (when (boundp 'org-capture-templates)
    (add-to-list 'org-capture-templates
                 '("j" "Journal Entry" entry (file+headline (lambda () (org-journal--get-entry-path))
                                                            (lambda () (format-time-string "%Y-%m-%d %A")))
                   "** %?"
                   :empty-lines 1))))

(provide 'init-journal)

;;; init-journal.el ends here
