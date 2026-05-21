;;; init-org-agenda.el --- Org agenda/GTD configuration -*- lexical-binding: t -*-

;; Author: shaowen he

;;; Commentary:

;; Org Agenda, GTD, capture templates, pomodoro, super-agenda.

;;; Code:

(require 'init-const)
(require 'init-funcs-org)

(with-eval-after-load 'org
  (progn

    ;; C-n for the next org agenda item
    (define-key org-agenda-mode-map (kbd "C-p") 'org-agenda-previous-item)

    ;; mode specific major key
    (global-leader
      :major-modes
      '(org-mode t)
      ;;and the keymaps:
      :keymaps
      '(org-mode-map)
      "p" 'org-pomodoro
      "t" 'org-todo
      "e" 'org-set-effort
      ">" 'org-metaright
      "<" 'org-metaleft
      "J" 'org-metadown
      "K" 'org-metaup
      "Ts" 'org-set-tags-command
      "l" 'org-toggle-link-display
      "L" 'org-toggle-inline-images
      "I" 'org-clock-in
      "O" 'org-clock-out
      "P" 'org-set-property
      "s" 'org-schedule
      "+" 'org-increase-number-at-point
      "-" 'org-decrease-number-at-point
      "n" 'org-narrow-to-subtree
      "dc" 'org-download-clipboard
      "ds" 'org-download-screenshot
      "Tl" 'org-latex-preview
      "w" 'widen
      "of" 'org-roam-node-find
      "og" 'org-roam-graph
      "os" 'org-roam-db-sync
      "or" 'org-roam-node-random
      "oc" 'org-roam-capture
      "oi" 'org-roam-node-insert
      "ot" 'org-roam-tag-add
      "oa" 'org-roam-alias-add
      "oT" 'org-roam-buffer-toggle
      "oe" 'org-roam-extract-subtree
      "oI" 'org-id-get-create
      "od" 'org-roam-dailies-capture-today
      "oD" 'org-roam-dailies-find-today)

    (global-leader
      :major-modes
      '(org-agenda-mode t)
      ;;and the keymaps:
      :keymaps
      '(org-agenda-mode-map)
      "d" 'org-agenda-day-view
      "w" 'org-agenda-week-view
      "," 'org-agenda-priority
      "e" 'org-agenda-set-effort
      ":" 'org-agenda-set-tags
      "T" 'org-agenda-show-tags
      "p" 'org-pomodoro)


    (with-eval-after-load 'org-agenda
      (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro)
      ;; 默认显示节假日
      (setq org-agenda-include-diary t)
      )
    ;; the %i would copy the selected text into the template
    ;;http://www.howardism.org/Technical/Emacs/journaling-org.html
    ;;add multi-file journal
    (setq org-capture-templates
          '(("t" "Todo" entry (file+headline org-agenda-file-gtd "Workspace")
             "* TODO [#B] %?\n  %i\n %U"
             :empty-lines 1)
            ("n" "notes" entry (file+headline org-agenda-file-note "Quick notes")
             "* %?\n  %i\n %U"
             :empty-lines 1)
            ("b" "Blog Ideas" entry (file+headline org-agenda-file-note "Blog Ideas")
             "* TODO [#B] %?\n  %i\n %U"
             :empty-lines 1)
            ("s" "Slipbox" entry  (file "~/org-notes/roam/inbox.org")
             "* %?\n")
            ("S" "Code Snippet" entry
             (file org-agenda-file-code-snippet)
             "* %?\t%^g\n#+BEGIN_SRC %^{language}\n\n#+END_SRC")
            ("w" "work" entry (file+headline org-agenda-file-work "Work")
             "* TODO [#A] %?\n  %i\n %U"
             :empty-lines 1)
            ("x" "Web Collections" entry
             (file+headline org-agenda-file-note "Web")
             "* %U %:annotation\n\n%:initial\n\n%?")
            ("p" "Protocol" entry (file+headline org-agenda-file-note "Inbox")
             "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
	        ("L" "Protocol Link" entry (file+headline org-agenda-file-note "Inbox")
             "* %? [[%:link][%:description]] \nCaptured On: %U")
            ("c" "Chrome" entry (file+headline org-agenda-file-note "Quick notes")
             "* TODO [#C] %?\n %(zilongshanren/retrieve-chrome-current-tab-url)\n %i\n %U"
             :empty-lines 1)
            ("l" "links" entry (file+headline org-agenda-file-note "Quick notes")
             "* TODO [#C] %?\n  %i\n %a \n %U"
             :empty-lines 1)
            ;; ("j" "Journal Entry"
            ;;  entry (file+datetree org-agenda-file-journal)
            ;;  "* %?"
            ;;  :empty-lines 1)
            ))

    (with-eval-after-load 'org-capture
      (defun org-hugo-new-subtree-post-capture-template ()
        "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
        (let* ((title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
               (fname (org-hugo-slug title)))
          (mapconcat #'identity
                     `(
                       ,(concat "* TODO " title)
                       ":PROPERTIES:"
                       ,(concat ":EXPORT_FILE_NAME: " fname)
                       ":END:"
                       "\n\n")          ;Place the cursor here finally
                     "\n")))

      (add-to-list 'org-capture-templates
                   '("h"                ;`org-capture' binding + h
                     "Hugo post"
                     entry
                     ;; It is assumed that below file is present in `org-directory'
                     ;; and that it has a "Blog Ideas" heading. It can even be a
                     ;; symlink pointing to the actual location of all-posts.org!
                     (file+headline org-agenda-file-blogposts "Blog Ideas")
                     (function org-hugo-new-subtree-post-capture-template))))

    ;;An entry without a cookie is treated just like priority ' B '.
    ;;So when create new task, they are default 重要且紧急
    (setq org-agenda-custom-commands
          '(
            ("w" . "任务安排")
            ("wa" "重要且紧急的任务" tags-todo "+PRIORITY=\"A\"")
            ("wb" "重要且不紧急的任务" tags-todo "-Weekly-Monthly-Daily+PRIORITY=\"B\"")
            ("wc" "不重要且紧急的任务" tags-todo "+PRIORITY=\"C\"")
            ("b" "Blog" tags-todo "BLOG")
            ("p" . "项目安排")
            ("pw" tags-todo "PROJECT+WORK+CATEGORY=\"work\"")
            ("pl" tags-todo "PROJECT+DREAM+CATEGORY=\"zilongshanren\"")
            ("W" "Weekly Review"
             ((stuck "") ;; review stuck projects as designated by org-stuck-projects
              (tags-todo "PROJECT") ;; review all projects (assuming you use todo keywords to designate projects)
              ))))

    (add-to-list 'org-agenda-custom-commands
                 '("r" "Daily Agenda Review"
                   ((agenda "" ((org-agenda-overriding-header "今日记录")
                                (org-agenda-span 'day)
                                (org-agenda-show-log 'clockcheck)
                                (org-agenda-start-with-log-mode nil)
                                (org-agenda-log-mode-items '(closed clock state))
                                (org-agenda-clockreport-mode t))))))

    (use-package org-pomodoro
      :commands org-pomodoro
      :after org)


    (use-package org-super-agenda
      :after org
      :init
      (setq org-super-agenda-header-map (make-sparse-keymap))
      (define-key org-super-agenda-header-map (kbd "q") 'org-agenda-quit)
      (setq org-super-agenda-groups
            '((:name "Important"
                     :priority "A")
              (:name "Quick Picks"
                     :effort< "0:30")
              (:name "Next Items"
                     :tag ("NEXT" "outbox"))
              (:priority<= "B"
                           :scheduled future)))
      (add-hook 'org-agenda-mode-hook
                'org-super-agenda-mode))

    ))

(provide 'init-org-agenda)
;;; init-org-agenda.el ends here
