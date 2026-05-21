;;; init-funcs-edit.el --- Editing helper functions -*- lexical-binding: t -*-

;; Author: shaowen he

;;; Commentary:

;; Editing enhancement functions: indent, expand-region, highlight,
;; multi-edit, text transformation.

;;; Code:

;; ── Indentation ──
(defun indent-buffer()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun indent-region-or-buffer()
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indent selected region."))
      (progn
        (indent-buffer)
        (message "Indent buffer.")))))

;; ── Case conversion dwim ──
(defmacro dakra-define-up/downcase-dwim (case)
  (let ((func (intern (concat "dakra-" case "-dwim")))
        (doc (format "Like `%s-dwim' but %s from beginning when no region is active." case case))
        (case-region (intern (concat case "-region")))
        (case-word (intern (concat case "-word"))))
    `(defun ,func (arg)
       ,doc
       (interactive "*p")
       (save-excursion
         (if (use-region-p)
             (,case-region (region-beginning) (region-end))
           (beginning-of-thing 'symbol)
           (,case-word arg))))))

(dakra-define-up/downcase-dwim "upcase")
(dakra-define-up/downcase-dwim "downcase")
(dakra-define-up/downcase-dwim "capitalize")

;; ── Double capitals fix ──
(defun dcaps-to-scaps ()
  "Convert word in DOuble CApitals to Single Capitals."
  (interactive)
  (and (= ?w (char-syntax (char-before)))
       (save-excursion
         (and (if (called-interactively-p)
                  (skip-syntax-backward "w")
                (= -3 (skip-syntax-backward "w")))
              (let (case-fold-search)
                (looking-at "\\b[[:upper:]]\\{2\\}[[:lower:]]"))
              (capitalize-word 1)))))

;; ── Smart open line ──
(defun my/smart-open-line ()
  "Insert an empty line after the current line."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

;; ── Yank to end of line ──
(defun my/yank-to-end-of-line ()
  "Yank to end of line."
  (interactive)
  (evil-yank (point) (point-at-eol)))

;; ── Highlight dwim ──
(defun my/highlight-dwim ()
  (interactive)
  (if (use-region-p)
      (progn (deactivate-mark))
    (symbol-overlay-put)))

(defun my/search-project-for-symbol-at-point ()
  (interactive)
  (if (use-region-p)
      (progn
        (consult-ripgrep (project-root (project-current))
                         (buffer-substring (region-beginning) (region-end))))))

(defun my/clearn-highlight ()
  (interactive)
  (symbol-overlay-remove-all))

;; ── Expand region integration ──
(defun my/my-mc-mark-next-like-this ()
  (interactive)
  (er/expand-region 1))

;; ── Sexp wrapping ──
(defun wrap-sexp-with-new-round-parens ()
  (interactive)
  (insert "()")
  (backward-char)
  (sp-forward-slurp-sexp))

;; ── Evil paste from register 0 ──
(defun evil-paste-after-from-0 ()
  (interactive)
  (let ((evil-this-register ?0))
    (call-interactively 'evil-paste-after)))

;; ── Evil quick replace ──
(defun zilongshanren/evil-quick-replace (beg end )
  (interactive "r")
  (when (evil-visual-state-p)
    (evil-exit-visual-state)
    (let ((selection (regexp-quote (buffer-substring-no-properties beg end))))
      (setq command-string (format "%%s /%s//g" selection))
      (minibuffer-with-setup-hook
          (lambda () (backward-char 2))
        (evil-ex command-string)))))

;; ── Unfill paragraph ──
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;; ── Delete all empty lines ──
(defun delete-all-empty-lines ()
  (interactive)
  (flush-lines "^$"))

;; ── Reverse region ──
(defun my-reverse-region (beg end)
  "Reverse characters between BEG and END."
  (interactive "r")
  (let ((region (buffer-substring beg end)))
    (delete-region beg end)
    (insert (nreverse region))))

;; ── JSON to single line ──
(defun json-to-single-line (beg end)
  "Collapse prettified json in region between BEG and END to a single line"
  (interactive "r")
  (if (use-region-p)
      (save-excursion
        (save-restriction
          (narrow-to-region beg end)
          (goto-char (point-min))
          (while (re-search-forward "[[:space:]\n]+" nil t)
            (replace-match " "))))
    (print "This function operates on a region")))

;; ── Convert to quoted symbols ──
(defun tmhourglass/convert-to-quoted-symbols (str)
  "Convert input string STR to quoted symbols."
  (interactive
   (if (use-region-p)
       (list (buffer-substring-no-properties (region-beginning) (region-end)))
     (list (read-string "Please input need convert string: "))))
  (let* ((input str)
         (words (split-string input))
         (quoted-words (mapcar (lambda (word) (format "'%s'" word)) words))
         (result (mapconcat 'identity quoted-words ", ")))
    (if (use-region-p)
        (progn
          (goto-char (region-end))
          (newline)
          (insert result))
      (kill-new result)
      (message "Copied to clipboard."))))

;; ── Disable curly bracket electric pair ──
(defun disable-curly-bracket-electric-pair ()
  (setq-local electric-pair-inhibit-predicate
              `(lambda (c)
                 (if (char-equal c ?{) t (,electric-pair-inhibit-predicate c)))))

;; ── Auto scroll hack ──
(defun my-auto-scroll-hack ()
  (set (make-local-variable 'window-point-insertion-type) t))

(add-hook 'shell-mode-hook 'my-auto-scroll-hack)

;; ── Auto indent method ──
(defun av/auto-indent-method ()
  "Automatically indent a method by adding two newlines."
  (interactive)
  (newline-and-indent)
  (newline-and-indent)
  (forward-line -1)
  (cond ((eq major-mode 'rust-mode) (rust-mode-indent-line))
        ((eq major-mode 'dart-mode) (dart-indent-simple))
        (t (c-indent-line-or-region))))

(defun av/auto-indent-method-maybe ()
  "Check if point is at a closing brace then auto indent."
  (interactive)
  (let ((char-at-point (char-after (point))))
    (if (char-equal ?} char-at-point)
        (av/auto-indent-method)
      (newline-and-indent))))

;; ── List element replacement ──
(defun replace-element-in-list (elem-src elem-dst ls &optional times comparison-fn)
  (setq times (or times (length ls)))
  (mapcar
   (lambda (item)
     (cond
      ((and (> times 0)
            (funcall (or comparison-fn #'eq) item elem-src))
       (cl-decf times)
       elem-dst)
      (t item)))
   ls))

(provide 'init-funcs-edit)
;;; init-funcs-edit.el ends here
