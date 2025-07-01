;;; init-aidermacs.el --- aidermacs -*- lexical-binding: t -*-

;; Author: shaowen he

;;; Commentary:

;; Aideremacs.el

;;; Code:

(use-package aidermacs
  :init (setq aidermacs-program (expand-file-name "~/.local/bin/aider")
              aidermacs-default-model "openrouter/google/gemini-2.0-flash-exp:free"
              )
  :config (setenv "OPENROUTER_API_KEY"
                  (with-temp-buffer
                    (insert-file-contents "~/.config/openrouter/key.txt")
                    (string-trim (buffer-string))))
  :bind ("s-/" . aidermacs-transient-menu))


(provide 'init-aidermacs)

;;; init-aidermacs.el ends here
