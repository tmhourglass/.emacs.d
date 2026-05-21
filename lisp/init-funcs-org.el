;;; init-funcs-org.el --- Org-related helper functions -*- lexical-binding: t -*-

;; Author: shaowen he

;;; Commentary:

;; Org-mode related helper functions: agenda, journal, clock,
;; capture, roam integration.

;;; Code:

(require 'init-const)

;; ── Archive ──
(defun zilongshanren/org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/DONE" 'file))

(defun zilongshanren/org-archive-cancel-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/CANCELLED" 'file))

;; ── Stuck project detection ──
(defun zilongshanren/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  (bh/list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (and (eq (point) (bh/find-project-task))
               (bh/is-project-p))
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end)
                         (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next next-headline nil))
        next-headline))))

;; ── Src block insertion ──
(defun my/org-insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
  (interactive
   (let ((src-code-types
          '("emacs-lisp" "typescript" "python" "C" "sh" "java" "js" "clojure" "C++" "css"
            "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
            "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
            "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
            "scheme" "sqlite")))
     (list (completing-read "Source code type: " src-code-types))))
  (progn
    (newline-and-indent)
    (insert (format "#+BEGIN_SRC %s :results output\n" src-code-type))
    (newline-and-indent)
    (insert "#+END_SRC\n")
    (previous-line 2)
    (org-edit-src-code)))

;; ── Summary todo ──
(defun zilong/org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(defun zilong/filter-by-tags ()
  (let ((head-tags (org-get-tags-at)))
    (member current-tag head-tags)))

(defun zilong/org-clock-sum-today-by-tags (timerange &optional tstart tend noinsert)
  (interactive "P")
  (let* ((timerange-numeric-value (prefix-numeric-value timerange))
         (files (org-add-archive-files (org-agenda-files)))
         (include-tags '("WORK" "EMACS" "DREAM" "WRITING" "MEETING"
                         "LIFE" "PROJECT" "OTHER"))
         (tags-time-alist (mapcar (lambda (tag) `(,tag . 0)) include-tags))
         (output-string "")
         (tstart (or tstart
                     (and timerange (equal timerange-numeric-value 4)
                          (- (org-time-today) 86400))
                     (and timerange (equal timerange-numeric-value 16)
                          (org-read-date nil nil nil "Start Date/Time:"))
                     (org-time-today)))
         (tend (or tend
                   (and timerange (equal timerange-numeric-value 16)
                        (org-read-date nil nil nil "End Date/Time:"))
                   (+ tstart 86400)))
         h m file item prompt donesomething)
    (while (setq file (pop files))
      (setq org-agenda-buffer (if (file-exists-p file)
                                  (org-get-agenda-file-buffer file)
                                (error "No such file %s" file)))
      (with-current-buffer org-agenda-buffer
        (dolist (current-tag include-tags)
          (org-clock-sum tstart tend 'zilong/filter-by-tags)
          (setcdr (assoc current-tag tags-time-alist)
                  (+ org-clock-file-total-minutes
                     (cdr (assoc current-tag tags-time-alist)))))))
    (while (setq item (pop tags-time-alist))
      (unless (equal (cdr item) 0)
        (setq donesomething t)
        (setq h (/ (cdr item) 60)
              m (- (cdr item) (* 60 h)))
        (setq output-string (concat output-string
                                    (format "[-%s-] %.2d:%.2d\n" (car item) h m)))))
    (unless donesomething
      (setq output-string (concat output-string "[-Nothing-] Done nothing!!!\n")))
    (unless noinsert (insert output-string))
    output-string))

;; ── Hotspots (launcher) ──
(defun zilongshanren/hotspots ()
  (interactive)
  (require 'consult)
  (setq-local source
              '(("Calendar" . (lambda () (browse-url "https://www.google.com/calendar/render")))
                ("RSS" . elfeed)
                ("Blog" . browse-hugo-maybe)
                ("Search" . (lambda () (call-interactively #'engine/search-google)))
                ("Random Todo" . org-random-entry)
                ("string edit" . separedit)
                ("Org Roam" . org-roam-find-file)
                ("Github" . (lambda() (helm-github-stars)))
                ("Prodigy" . (lambda() (prodigy)))
                ("Run current file" . (lambda () (my/run-current-file)))
                ("Agenda" . (lambda () (org-agenda "" "a")))
                ("sicp" . (lambda() (browse-url "http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-4.html#%_toc_start")))))
  (let* ((result (consult--read (mapcar 'car source) :prompt "zilong's hotpot ")))
    (when result
      (funcall (cdr (assoc result source))))))

;; ── Journal date insertion ──
(defun my/insert-journal-date ()
  "Use Emacs calendar to select a date, insert it in YYYY-MM-DD Day format."
  (interactive)
  (let* ((selected-date (org-read-date nil t))
         (timestamp (format-time-string "%Y-%m-%d %A" selected-date)))
    (insert timestamp)))

;; ── Agenda effort calculation ──
(defun my/org-agenda-calculate-efforts (limit)
  "Sum the efforts of scheduled entries up to LIMIT in the agenda buffer."
  (when limit
    (let (total)
      (save-excursion
        (while (< (point) limit)
          (when (member (org-get-at-bol 'type) '("scheduled" "past-scheduled"))
            (push (org-entry-get (org-get-at-bol 'org-hd-marker) "Effort") total))
          (forward-line)))
      (org-duration-from-minutes
       (cl-reduce #'+
                  (mapcar #'org-duration-to-minutes
                          (cl-remove-if-not 'identity total)))))))

(defun my/org-agenda-insert-efforts ()
  "Insert the efforts for each day inside the agenda buffer."
  (save-excursion
    (let (pos)
      (while (setq pos (text-property-any
                        (point) (point-max) 'org-agenda-date-header t))
        (goto-char pos)
        (end-of-line)
        (insert-and-inherit (concat " ("
                                    (my/org-agenda-calculate-efforts
                                     (next-single-property-change (point) 'day))
                                    ")"))
        (forward-line)))))

(provide 'init-funcs-org)
;;; init-funcs-org.el ends here
