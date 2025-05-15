;;; init-aidermacs.el --- aidermacs -*- lexical-binding: t -*-

;; Author: shaowen he

;;; Commentary:

;; Aideremacs.el

;;; Code:

(require 'aidermacs)

;;; Code:
(setq aidermacs-program (expand-file-name "~/.local/bin/aider"))

;; (setq aidermacs-default-model "openrouter/google/gemini-2.0-flash-exp:free")
(setq aidermacs-default-model "openrouter/google/gemini-2.5-pro-exp-03-25")

(setenv "OPENROUTER_API_KEY" (with-temp-buffer
                               (insert-file-contents "~/.config/openrouter/key.txt")
                               (string-trim (buffer-string))))

(provide 'init-aidermacs)

;;; init-aidermacs.el ends here
