;;; init-ai.el --- ai -*- lexical-binding: t -*-

;; Author: shaowen he

;;; Commentary:

;; commentary

;;; Code:


;; (use-package org-ai
;;   :commands (org-ai-mode)
;;   :init
;;   (add-hook 'org-mode-hook #'org-ai-mode)
;;   :config
;;   ;; if you are using yasnippet and want `ai` snippets
;;   (org-ai-install-yasnippets))

;; (require 'mind-wave)

;; (require 'chatgpt-shell)

;; (setq chatgpt-shell-openai-key
;;       (plist-get (car (auth-source-search :host "api.openai.com"))
;;                  :secret))

;; api-key : ~/.authinfo
;; moonshot token: sk-MLU7DNta9EFOwxezZkgGMo8uu9Y3UtQMdVZ1CeHMe6Mmzs6I
(use-package gptel
  :config
  (setq gptel-model "moonshot-v1-8k")
  (setq gptel-default-mode 'org-mode)
  (setq gptel-backend
        (gptel-make-openai "Moonshot"
          :key 'gptel-api-key
          :models '("moonshot-v1-8k"
                    "moonshot-v1-32k"
                    "moonshot-v1-128k")
          :host "api.moonshot.cn")))


(provide 'init-ai)

;;; init-ai.el ends here
