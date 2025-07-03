;;; init-syntaxcheck.el --- syntaxcheck -*- lexical-binding: t -*-

;; Author: shaowen he

;;; Commentary:

;; commentary

;;; Code:

(use-package flymake
  :ensure nil)

(use-package flycheck
  :commands (flycheck-mode)
  :bind ("C-c t f" . flycheck-mode)
  :config
  (when sys/win32p
    (setq flycheck-check-syntax-automatically '(save)))

  (flycheck-add-mode 'javascript-eslint 'web-mode)

  ;; 快捷键设定
  (+general-global-menu! "toggle" "t"
    "s" 'flycheck-mode)

  (flycheck-define-checker javascript-eslint
    "A Javascript syntax and style checker using eslint.

See URL `https://eslint.org/'."
    :command ("eslint" "--format=json"
              (option-list "--rulesdir" flycheck-eslint-rules-directories)
              (eval flycheck-eslint-args)
              "--stdin" "--stdin-filename" source-original)
    :standard-input t
    :error-parser flycheck-parse-eslint
    :enabled (lambda () (flycheck-eslint-config-exists-p))
    :modes (genehack-vue-mode js-mode js-jsx-mode js2-mode js2-jsx-mode js3-mode rjsx-mode
                              typescript-mode)
    :working-directory flycheck-eslint--find-working-directory
    :verify
    (lambda (_)
      (let* ((default-directory
              (flycheck-compute-working-directory 'javascript-eslint))
             (have-config (flycheck-eslint-config-exists-p)))
        (list
         (flycheck-verification-result-new
          :label "config file"
          :message (if have-config "found" "missing or incorrect")
          :face (if have-config 'success '(bold error))))))
    :error-explainer
    (lambda (err)
      (let ((error-code (flycheck-error-id err))
            (url "https://eslint.org/docs/rules/%s"))
        (and error-code
             ;; skip non-builtin rules
             (not ;; `seq-contains-p' is only in seq >= 2.21
              (with-no-warnings (seq-contains error-code ?/)))
             `(url . ,(format url error-code))))))

  ;; install json link with commands:  npm install -g mwks-jsonlint --force
  ;; don't use original jsonlint which doesn't respect comments in json
  (flycheck-define-checker json-jsonlint
    "A JSON syntax and style checker using jsonlint.

See URL `https://github.com/zaach/jsonlint'."
    ;; We can't use standard input for jsonlint, because it doesn't output errors
    ;; anymore when using -c -q with standard input :/
    :command ("jsonlint" "-c" "-C" "-q" source)
    :error-patterns
    ((error line-start
            (file-name)
            ": line " line
            ", col " column ", "
            (message) line-end))
    :error-filter
    (lambda (errors)
      (flycheck-sanitize-errors (flycheck-increment-error-columns errors)))
    :modes json-mode))


(use-package flymake-posframe
  :ensure nil
  :hook (flymake-mode . flymake-posframe-mode))


(provide 'init-syntaxcheck)

;;; init-syntaxcheck.el ends here
