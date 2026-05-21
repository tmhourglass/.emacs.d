;;; init-org-roam.el --- Org Roam configuration -*- lexical-binding: t -*-

;; Author: shaowen he

;;; Commentary:

;; Org Roam: nodes, templates, UI, dailies, consult integration.

;;; Code:

(require 'init-const)

(with-eval-after-load 'org
  (progn

    (use-package org-roam
      :if (file-exists-p org-directory)
      :init
      (defun jethro/org-capture-slipbox ()
        (interactive)
        (org-capture nil "s"))
      :custom
      ;; (org-roam-directory (file-truename org-directory))
      (org-roam-directory (file-truename "~/org-notes/roam"))
      :config
      (cl-defmethod org-roam-node-type ((node org-roam-node))
        "Return the TYPE of NODE."
        (condition-case nil
            (file-name-nondirectory
             (directory-file-name
              (file-name-directory
               (file-relative-name (org-roam-node-file node) org-roam-directory))))
          (error "")))
      ;; If you're using a vertical completion framework, you might want a more informative completion interface
      (setq org-roam-node-display-template (concat "${type:10} ${title:20} " (propertize "${tags:40}" 'face 'org-tag)))
      (org-roam-db-autosync-mode)
      (setq org-roam-dailies-directory "daily/")

      ;; https://systemcrafters.net/build-a-second-brain-in-emacs/5-org-roam-hacks/
      (defun org-roam-node-insert-immediate (arg &rest args)
        (interactive "P")
        (let ((args (cons arg args))
              (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                        '(:immediate-finish t)))))
          (apply #'org-roam-node-insert args)))

      (defun my/org-roam-filter-by-tag (tag-name)
        (lambda (node)
          (member tag-name (org-roam-node-tags node))))

      (defun my/org-roam-list-notes-by-tag (tag-name)
        (mapcar #'org-roam-node-file
                (seq-filter
                 (my/org-roam-filter-by-tag tag-name)
                 (org-roam-node-list))))

      (defun my/org-roam-refresh-agenda-list ()
        (interactive)
        (setq org-agenda-files (my/org-roam-list-notes-by-tag "Unity")))


      (setq org-roam-capture-templates
            '(("m" "main" plain
               "%?"
               :if-new (file+head "main/${slug}.org"
                                  "#+title: ${title}\n")
               :immediate-finish t
               :unnarrowed t)
              ("r" "reference" plain "%?"
               :if-new
               (file+head "reference/${title}.org" "#+title: ${title}\n")
               :immediate-finish t
               :unnarrowed t)
              ("a" "article" plain "%?"
               :if-new
               (file+head "articles/${title}.org" "#+title: ${title}\n#+filetags: :article:\n")
               :immediate-finish t
               :unnarrowed t)
              ("b" "book" plain "%?"
               :if-new
               (file+head "books/${title}.org" "#+title: ${title}\n#+filetags: :book:\n")
               :immediate-finish t
               :unnarrowed t)))
      ;; If using org-roam-protocol
      ;; todo-borg
      ;; (require 'org-roam-protocol)
      :bind (("C-c n f" . org-roam-node-find)
             ("C-c n g" . org-roam-graph)
             ("C-c n r" . org-roam-node-random)
             ("C-c n c" . org-roam-capture)
             ;; Dailies
             ("C-c n j" . org-roam-dailies-capture-today)
             (:map org-mode-map
                   (("C-c n i" . org-roam-node-insert)
                    ("C-c n I" . org-roam-node-insert-immediate)
                    ("C-c n o" . org-id-get-create)
                    ("C-c n t" . org-roam-tag-add)
                    ("C-c n E" . org-roam-extract-subtree)
                    ("C-c n a" . org-roam-alias-add)
                    ("C-c n l" . org-roam-buffer-toggle)))))

    (use-package org-roam-ui
      :commands (org-roam-ui-mode)
      :after org
      )

    (use-package consult-org-roam
      :init
      :commands (consult-org-roam-forward-links)
      :custom
      (consult-org-roam-grep-func #'consult-ripgrep)
      :config
      ;; Eventually suppress previewing for certain functions
      (consult-customize
       consult-org-roam-forward-links
       :preview-key (kbd "s-."))
      :bind
      ("C-c n e" . consult-org-roam-forward-links)
      ("C-c n b" . consult-org-roam-backlinks)
      ("C-c n s" . consult-org-roam-search))

    (use-package org-contrib
      :demand t
      :init
      (require 'org-checklist))

    ))

(provide 'init-org-roam)
;;; init-org-roam.el ends here
