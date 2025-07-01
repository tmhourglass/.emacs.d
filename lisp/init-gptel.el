;;; init-gptel.el --- gptel -*- lexical-binding: t -*-

;; Author: shaowen he

;;; Commentary:

;; Configuration gptel - use-package版本

;;; Code:

(use-package gptel
  :defer t  ; 延迟加载以提升启动速度
  :commands (gptel start-gptel gptel-send gptel-return-dwim)
  :init
  ;; 在包加载前设置API密钥
  (defvar open-router-key nil "OpenRouter API密钥")

  ;; 延迟读取API密钥，避免启动时的文件IO
  (defun my/load-openrouter-key ()
    "延迟加载OpenRouter API密钥."
    (unless open-router-key
      (setq open-router-key
            (when (file-exists-p "~/.config/openrouter/key.txt")
              (condition-case err
                  (with-temp-buffer
                    (insert-file-contents "~/.config/openrouter/key.txt")
                    (string-trim (buffer-string)))
                (error
                 (message "警告: 无法读取OpenRouter API密钥: %s" (error-message-string err))
                 nil)))))
    open-router-key)

  :config
  ;; 确保gptel-curl可用
  (require 'gptel-curl)

  ;; 加载API密钥
  (my/load-openrouter-key)

  ;; 配置不同的backend
  ;; OpenRouter配置
  (if open-router-key
      (gptel-make-openai "OpenRouter"
        :host "openrouter.ai"
        :endpoint "/api/v1/chat/completions"
        :stream t
        :key open-router-key
        :models '(google/gemini-2.5-flash-preview-05-20))
    (message "提示: 未找到OpenRouter API密钥，请检查 ~/.config/openrouter/key.txt"))

  ;; Ollama本地配置
  (gptel-make-ollama "Ollama-local"
    :host "localhost:11434"
    :stream t
    :models '(llama3.1:8b))

  ;; 设置默认model和backend
  (setq gptel-model 'google/gemini-2.5-flash-preview-05-20)
  (when (gptel-get-backend "OpenRouter")
    (setq gptel-backend (gptel-get-backend "OpenRouter")))

  ;; 备用配置：本地ollama模型（用于翻译等场景）
  ;; (setq gptel-model 'llama3.1:8b)
  ;; (setq gptel-backend (gptel-get-backend "Ollama-local"))

  ;; 自定义函数定义
  (defun start-gptel ()
    "启动gptel会话."
    (interactive)
    (gptel "OpenRouter" nil nil t))

  ;; 智能回车函数：在提示行触发发送，否则普通换行
  (defun gptel-return-dwim (&optional arg)
    "If cursor at prompt line, call `gptel-send', otherwise call RET function."
    (interactive "P")
    (let ((in-prompt-line-p
           (save-excursion
             (beginning-of-line)
             (search-forward-regexp "^#+\\s-" (line-end-position) t))))
      (if in-prompt-line-p
          (gptel-send arg)
        (call-interactively (key-binding (kbd "C-m"))))))

  ;; 可选：添加一些便捷函数
  (defun my/gptel-quick-chat ()
    "快速启动gptel聊天."
    (interactive)
    (let ((buffer (get-buffer-create "*gptel-quick*")))
      (with-current-buffer buffer
        (gptel-mode)
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert "# "))
      (pop-to-buffer buffer)))

  (defun my/switch-to-ollama ()
    "切换到本地Ollama模型."
    (interactive)
    (setq gptel-model 'llama3.1:8b
          gptel-backend (gptel-get-backend "Ollama-local"))
    (message "已切换到本地Ollama模型"))

  (defun my/switch-to-openrouter ()
    "切换到OpenRouter模型."
    (interactive)
    (setq gptel-model 'google/gemini-2.5-flash-preview-05-20
          gptel-backend (gptel-get-backend "OpenRouter"))
    (message "已切换到OpenRouter模型"))

  ;; 可选：设置按键绑定
  :bind (("C-c g g" . gptel)
         ("C-c g s" . start-gptel)
         ("C-c g q" . my/gptel-quick-chat)
         ("C-c g o" . my/switch-to-ollama)
         ("C-c g r" . my/switch-to-openrouter))

  ;; 可选：在gptel-mode中的按键绑定
  :bind (:map gptel-mode-map
              ("RET" . gptel-return-dwim)))

(provide 'init-gptel)

;;; init-gptel.el ends here
