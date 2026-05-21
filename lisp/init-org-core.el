;;; init-org-core.el --- Org-mode core behavior -*- lexical-binding: t -*-

;; Author: shaowen he

;;; Commentary:

;; Org-mode basic behavior: indentation, emphasis, folding, RET rewrite,
;; babel, encrypt, link, clock, todo keywords, refile.

;;; Code:

(require 'init-const)

;; 加速org的启动
;;https://emacs-china.org/t/org/15884/8
(setq org-modules-loaded t)
;; https://emacs-china.org/t/org/31278/8
(setq org-modules '(org-tempo))

(with-eval-after-load 'org
  (progn

    (setq org-startup-indented t
          org-pretty-entities t
          org-hide-emphasis-markers t
          org-fontify-emphasized-text t
          org-link-descriptive t
          org-log-done 'time
          org-catch-invisible-edits 'smart
          org-fontify-quote-and-verse-blocks t
          org-startup-with-inline-images t
          org-image-actual-width '(300))


    ;; 设置折叠样式
    (setq org-ellipsis "⤵")

    ;; 设置强调标记
    (setq org-emphasis-alist
          '(("*" my-org-emphasis-bold)
            ("/" my-org-emphasis-italic)
            ("_" underline)
            ("=" org-verbatim verbatim)
            ("~" org-code verbatim)
            ("+" (:strike-through t))))

    ;; 定义粗体样式
    (defface my-org-emphasis-bold
      '((default :inherit bold)
        (((class color) (min-colors 88) (background light))
         :foreground "#a60000")
        (((class color) (min-colors 88) (background dark))
         :foreground "#ff8059"))
      "My bold emphasis for Org.")

    ;; 定义斜体样式
    (defface my-org-emphasis-italic
      '((default :inherit italic)
        (((class color) (min-colors 55) (background light))
         :foreground "#972500")
        (((class color) (min-colors 55) (background dark))
         :foreground "#ef8b50"))
      "My italic emphasis for Org.")


    ;; 定义两个变量，后续使用，指定org位置目录
    (defvar org-agenda-dir ""
      "gtd org files location")

    (defvar deft-dir ""
      "deft org files locaiton")

    (setq org-agenda-dir "~/org-notes/agenda")
    (setq deft-dir  "~/org-notes/")


    (require 'org-tempo)
    ;; Allow multiple line Org emphasis markup.
    ;; http://emacs.stackexchange.com/a/13828/115
    (setcar (nthcdr 4 org-emphasis-regexp-components) 20) ;Up to 20 lines, default is just 1
    ;; Below is needed to apply the modified `org-emphasis-regexp-components'
    ;; settings from above.
    (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

    (setq org-agenda-log-mode-items '(clock closed state))

    ;; 重新定义org-mode中return的行为
    (defun my/org-return (&optional indent)
      "Goto next table row or insert a newline.
Calls `org-table-next-row' or `newline', depending on context.
When optional INDENT argument is non-nil, call
`newline-and-indent' instead of `newline'.
When `org-return-follows-link' is non-nil and point is on
a timestamp or a link, call `org-open-at-point'.  However, it
will not happen if point is in a table or on a \"dead\"
object (e.g., within a comment).  In these case, you need to use
`org-open-at-point' directly."
      (interactive)
      (let ((context (if org-return-follows-link (org-element-context)
                       (org-element-at-point))))
        (cond
         ;; In a table, call `org-table-next-row'.  However, before first
         ;; column or after last one, split the table.
         ((or (and (eq 'table (org-element-type context))
                   (not (eq 'table.el (org-element-property :type context)))
                   (>= (point) (org-element-property :contents-begin context))
                   (< (point) (org-element-property :contents-end context)))
              (org-element-lineage context '(table-row table-cell) t))
          (if (or (looking-at-p "[ \t]*$")
                  (save-excursion (skip-chars-backward " \t") (bolp)))
              (insert "\n")
            (org-table-justify-field-maybe)
            (call-interactively #'org-table-next-row)))
         ;; On a link or a timestamp, call `org-open-at-point' if
         ;; `org-return-follows-link' allows it.  Tolerate fuzzy
         ;; locations, e.g., in a comment, as `org-open-at-point'.
         ((and org-return-follows-link
               (or (and (eq 'link (org-element-type context))
                        ;; Ensure point is not on the white spaces after
                        ;; the link.
                        (let ((origin (point)))
                          (org-with-point-at (org-element-property :end context)
                            (skip-chars-backward " \t")
                            (> (point) origin))))
                   (org-in-regexp org-ts-regexp-both nil t)
                   (org-in-regexp org-tsr-regexp-both nil t)
                   (org-in-regexp org-any-link-re nil t)))
          (call-interactively #'org-open-at-point))
         ;; Insert newline in heading, but preserve tags.
         ((and (not (bolp))
               (let ((case-fold-search nil))
                 (org-match-line org-complex-heading-regexp)))
          ;; At headline.  Split line.  However, if point is on keyword,
          ;; priority cookie or tags, do not break any of them: add
          ;; a newline after the headline instead.
          (let ((tags-column (and (match-beginning 5)
                                  (save-excursion (goto-char (match-beginning 5))
                                                  (current-column))))
                (string
                 (when (and (match-end 4) (org-point-in-group (point) 4))
                   (delete-and-extract-region (point) (match-end 4)))))
            ;; Adjust tag alignment.
            (cond
             ((not (and tags-column string)))
             (org-auto-align-tags (org-align-tags))
             (t (org--align-tags-here tags-column))) ;preserve tags column
            (end-of-line)
            (org-show-entry)
            (if indent (newline-and-indent) (newline))
            (when string (save-excursion (insert (org-trim string))))))
         ;; In a list, make sure indenting keeps trailing text within.
         ((and indent
               (not (eolp))
               (org-element-lineage context '(item)))
          (let ((trailing-data
                 (delete-and-extract-region (point) (line-end-position))))
            (newline-and-indent)
            (save-excursion (insert trailing-data))))
         ((and (eolp) (org-at-item-p))
          (end-of-visible-line)
          (org-insert-item (org-at-item-checkbox-p)))
         (t
          ;; Do not auto-fill when point is in an Org property drawer.
          (let ((auto-fill-function (and (not (org-at-property-p))
                                         auto-fill-function)))
            (if indent
                (newline-and-indent)
              (newline)))))))

    ;; 重新定义org-mode中return的行为
    (define-key org-mode-map (kbd "RET") 'my/org-return)

    ;; 日志中手动插入日期
    ;; 放在init-general-keys中不生效，可能因为org未加载
    ;; (define-key org-mode-map  (kbd "C-c j") 'my/insert-journal-date)


    ;; 新发现的功能：在org-mode中的nornal下，若当前是列表，按 + 可循环切换列表的编号样式
    (evil-define-key 'normal org-mode-map
      "+" #'org-cycle-list-bullet)


    (setq org-complete-tags-always-offer-all-agenda-tags t)

    (require 'org-compat)
    (require 'org)
    (add-to-list 'org-modules 'org-habit)
    (require 'org-habit)

    ;; 调整orghabit 的显示长度
    (setq org-habit-graph-column 60)

    (setq org-refile-use-outline-path 'file)
    (setq org-outline-path-complete-in-steps nil)
    (setq org-refile-targets
          '((nil :maxlevel . 4)
            (org-agenda-files :maxlevel . 4)))
    ;; config stuck project
    (setq org-stuck-projects
          '("TODO={.+}/-DONE" nil nil "SCHEDULED:\\|DEADLINE:"))

    (setq org-agenda-inhibit-startup t) ;; ~50x speedup
    (setq org-agenda-span 'day)
    (setq org-agenda-use-tag-inheritance nil) ;; 3-4x speedup
    (setq org-agenda-window-setup 'current-window)
    ;; (setq org-log-done t)

    (setq
     org-agenda-current-time-string
     "⭠ now ─────────────────────────────────────────────────"
     )

    ;; 加密文章
    ;; "http://coldnew.github.io/blog/2013/07/13_5b094.html"
    ;; org-mode 設定
    (require 'org-crypt)

    ;; 當被加密的部份要存入硬碟時，自動加密回去
    (org-crypt-use-before-save-magic)

    ;; 設定要加密的 tag 標籤為 secret
    (setq org-crypt-tag-matcher "secret")

    ;; 避免 secret 這個 tag 被子項目繼承 造成重複加密
    ;; (但是子項目還是會被加密喔)
    (setq org-tags-exclude-from-inheritance (quote ("secret")))

    (setq org-columns-default-format "%60ITEM(Task) %6Effort(Estim){:}")

    ;; 在agenda里面显示efforts
    (require 'cl-lib)


    (add-hook 'org-agenda-finalize-hook 'my/org-agenda-insert-efforts)

    ;; 用於加密的 GPG 金鑰
    ;; 可以設定任何 ID 或是設成 nil 來使用對稱式加密 (symmetric encryption)
    (setq org-crypt-key nil)

    (require 'cal-china)
    ;; diary for chinese birthday
    ;; https://emacs-china.org/t/topic/2119/14
    (defun my--diary-chinese-anniversary (lunar-month lunar-day &optional year mark)
      (if year
          (let* ((d-date (diary-make-date lunar-month lunar-day year))
                 (a-date (calendar-absolute-from-gregorian d-date))
                 (c-date (calendar-chinese-from-absolute a-date))
                 (date a-date)
                 (cycle (car c-date))
                 (yy (cadr c-date))
                 (y (+ (* 100 cycle) yy)))
            (diary-chinese-anniversary lunar-month lunar-day y mark))
        (diary-chinese-anniversary lunar-month lunar-day year mark)))


    (setq org-todo-keywords
          (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
                  (sequence "WAITING(w@/!)" "SOMEDAY(S)" "|" "CANCELLED(c@/!)" "MEETING(m)" "PHONE(p)"))))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Org clock
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (setq org-agenda-start-day "+0d")
    ;; Change task state to STARTED when clocking in
    (setq org-clock-in-switch-to-state "STARTED")
    ;; Save clock data and notes in the LOGBOOK drawer
    (setq org-clock-into-drawer t)
    ;; Removes clocked tasks with 0:00 duration
    (setq org-clock-out-remove-zero-time-clocks t) ;; Show the clocked-in task - if any - in the header line

    (setq org-tags-match-list-sublevels nil)

    (setq org-image-actual-width nil)

    (plist-put org-format-latex-options :scale 1.5)


    ;; 交互式接入代码块，并在单独窗口编辑，C-c '结束或再进入
    ;; keybinding for inserting code blocks
    (local-set-key (kbd "C-c i s") 'my/org-insert-src-block)

    ;; todo-borg
    ;; (require 'ox-publish)
    ;; 有如下报错，暂时注释掉
    ;; File mode specification error: (void-variable org-latex-classes)
    ;; (add-to-list 'org-latex-classes '("ctexart" "\\documentclass[11pt]{ctexart}
    ;;                                     [NO-DEFAULT-PACKAGES]
    ;;                                     \\usepackage[utf8]{inputenc}
    ;;                                     \\usepackage[T1]{fontenc}
    ;;                                     \\usepackage{fixltx2e}
    ;;                                     \\usepackage{graphicx}
    ;;                                     \\usepackage{longtable}
    ;;                                     \\usepackage{float}
    ;;                                     \\usepackage{wrapfig}
    ;;                                     \\usepackage{rotating}
    ;;                                     \\usepackage[normalem]{ulem}
    ;;                                     \\usepackage{amsmath}
    ;;                                     \\usepackage{textcomp}
    ;;                                     \\usepackage{marvosym}
    ;;                                     \\usepackage{wasysym}
    ;;                                     \\usepackage{amssymb}
    ;;                                     \\usepackage{booktabs}
    ;;                                     \\usepackage[colorlinks,linkcolor=black,anchorcolor=black,citecolor=black]{hyperref}
    ;;                                     \\tolerance=1000
    ;;                                     \\usepackage{listings}
    ;;                                     \\usepackage{xcolor}
    ;;                                     \\lstset{
    ;;                                     %行号
    ;;                                     numbers=left,
    ;;                                     %背景框
    ;;                                     framexleftmargin=10mm,
    ;;                                     frame=none,
    ;;                                     %背景色
    ;;                                     %backgroundcolor=\\color[rgb]{1,1,0.76},
    ;;                                     backgroundcolor=\\color[RGB]{245,245,244},
    ;;                                     %样式
    ;;                                     keywordstyle=\\bf\\color{blue},
    ;;                                     identifierstyle=\\bf,
    ;;                                     numberstyle=\\color[RGB]{0,192,192},
    ;;                                     commentstyle=\\it\\color[RGB]{0,96,96},
    ;;                                     stringstyle=\\rmfamily\\slshape\\color[RGB]{128,0,0},
    ;;                                     %显示空格
    ;;                                     showstringspaces=false
    ;;                                     }
    ;;                                     "
    ;;                                   ("\\section{%s}" . "\\section*{%s}")
    ;;                                   ("\\subsection{%s}" . "\\subsection*{%s}")
    ;;                                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
    ;;                                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
    ;;                                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


    (defun org-random-entry (&optional arg)
      "Select and goto a random todo item from the global agenda"
      (interactive "P")
      (if org-agenda-overriding-arguments
          (setq arg org-agenda-overriding-arguments))
      (if (and (stringp arg) (not (string-match "\\S-" arg))) (setq arg nil))
      (let* ((today (org-today))
             (date (calendar-gregorian-from-absolute today))
             (kwds org-todo-keywords-for-agenda)
             (lucky-entry nil)
             (completion-ignore-case t)
             (org-agenda-buffer (when (buffer-live-p org-agenda-buffer)
                                  org-agenda-buffer))
             (org-select-this-todo-keyword
              (if (stringp arg) arg
                (and arg (integerp arg) (> arg 0)
                     (nth (1- arg) kwds))))
             rtn rtnall files file pos marker buffer)
        (when (equal arg '(4))
          (setq org-select-this-todo-keyword
                (org-icompleting-read "Keyword (or KWD1|K2D2|...): "
                                      (mapcar 'list kwds) nil nil)))
        (and (equal 0 arg) (setq org-select-this-todo-keyword nil))
        (catch 'exit
          (org-compile-prefix-format 'todo)
          (org-set-sorting-strategy 'todo)
          (setq files (org-agenda-files nil 'ifmode)
                rtnall nil)
          (while (setq file (pop files))
            (catch 'nextfile
              (org-check-agenda-file file)
              (setq rtn (org-agenda-get-day-entries file date :todo))
              (setq rtnall (append rtnall rtn))))

          (when rtnall
            (setq lucky-entry
                  (nth (random
                        (safe-length
                         (setq entries rtnall)))
                       entries))

            (setq marker (or (get-text-property 0 'org-marker lucky-entry)
                             (org-agenda-error)))
            (setq buffer (marker-buffer marker))
            (setq pos (marker-position marker))
            (org-pop-to-buffer-same-window buffer)
            (widen)
            (goto-char pos)
            (when (derived-mode-p 'org-mode)
              (org-show-context 'agenda)
              (save-excursion
                (and (outline-next-heading)
                     (org-flag-heading nil))) ; show the next heading
              (when (outline-invisible-p)
                (show-entry))           ; display invisible text
              (run-hooks 'org-agenda-after-show-hook))))))

    ;;reset subtask
    (setq org-default-properties (cons "RESET_SUBTASKS" org-default-properties))

    ;; (add-hook 'org-after-todo-state-change-hook 'org-subtask-reset)
    (setq org-return-follows-link t)

    ;; 画UML的工具，暂时未使用到，这两个包也未下载
    ;; (setq org-plantuml-jar-path
    ;;       (expand-file-name "~/.doom.d/plantuml.jar"))
    ;; (setq org-ditaa-jar-path "~/.doom.d/ditaa.jar")

    (require 'org-protocol)
    ;; https://chenzaichun.github.io/post/2021-10-04-org-roam-research-start/


    ;; https://github.com/hlissner/doom-emacs/blob/develop/docs/getting_started.org#configuring-doom
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((perl . t)
       (ruby . t)
       (shell . t)
       (dot . t)
       ;; (typescript . t)
       (js . t)
       (latex .t)
       (python . t)
       (emacs-lisp . t)
       (plantuml . t)
       (C . t)
       (ditaa . t)))

    (setq org-babel-default-header-args:cpp
          '((:eval . "never-export")
            (:flags . "-std=c++11")))

    (advice-add 'org-babel-C-execute
                :filter-args
                (defun sloth/org-babel-C-execute/filter-args (args)
                  (when-let* ((params (cadr args))
                              (stdin (cdr (assoc :stdin params)))
                              (res (org-babel-ref-resolve stdin))
                              (stdin (org-babel-temp-file "c-stdin-")))
                    (with-temp-file stdin (insert res))
                    (let* ((cmdline (assoc :cmdline params))
                           (cmdline-val (or (cdr cmdline) "")))
                      (when cmdline (setq params (delq cmdline params)))
                      (setq params
                            (cons (cons :cmdline (concat cmdline-val " <" stdin))
                                  params))
                      (setf (cadr args) params)))
                  args))

    (setq org-babel-python-command "python3")

    (progn

      (use-package cal-china-x
        :demand t)
      (setq mark-holidays-in-calendar t)
      (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
      (setq cal-china-x-general-holidays '((holiday-lunar 1 15 "元宵节")))
      (setq calendar-holidays
            (append cal-china-x-important-holidays
                    cal-china-x-general-holidays)))
    (require 'ox-md nil t)

    ;; define the refile targets
    (setq org-agenda-file-note (expand-file-name "notes.org" org-agenda-dir))
    (setq org-agenda-file-gtd (expand-file-name "gtd.org" org-agenda-dir))
    (setq org-agenda-file-work (expand-file-name "work.org" org-agenda-dir))
    (setq org-agenda-file-journal (expand-file-name "journal.org" org-agenda-dir))
    (setq org-agenda-file-code-snippet (expand-file-name "snippet.org" org-agenda-dir))
    ;; (setq org-default-notes-file (expand-file-name "gtd.org" org-agenda-dir))
    (setq org-agenda-file-blogposts (expand-file-name "all-posts.org" org-agenda-dir))
    (setq org-agenda-files (list org-agenda-file-gtd org-agenda-file-journal org-agenda-file-blogposts org-agenda-file-work org-agenda-file-note))

    (add-hook 'org-after-todo-statistics-hook 'zilong/org-summary-todo)
    ;; used by zilong/org-clock-sum-today-by-tags

    (define-key org-mode-map (kbd "s-p") 'org-priority)

    (define-key evil-normal-state-map (kbd "C-c C-w") 'org-refile)

    (use-package evil-org
      :hook (org-mode . evil-org-mode)
      :after org
      :config
      (require 'evil-org-agenda)
      (evil-org-agenda-set-keys))
    ))

(provide 'init-org-core)
;;; init-org-core.el ends here
