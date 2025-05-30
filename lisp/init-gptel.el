;;; init-gptel.el --- gptel -*- lexical-binding: t -*-

;; Author: shaowen he

;;; Commentary:

;; Configuration gptel

;;; Require
(require 'gptel)
(require 'gptel-curl)

;;; Code:

(setq open-router-key (with-temp-buffer
                        (insert-file-contents "~/.config/openrouter/key.txt")
                        (string-trim (buffer-string))))

(setq gptel-model 'google/gemini-2.5-flash-preview-05-20
      gptel-backend
      (gptel-make-openai "OpenRouter"
        :host "openrouter.ai"
        :endpoint "/api/v1/chat/completions"
        :stream t
        :key open-router-key
        :models '("google/gemini-2.5-flash-preview-05-20")))

(defun start-gptel ()
  (interactive)
  (gptel "OpenRouter" nil nil t))

;; 若在提示行，则触发gptel-send，执行。否则就是普通的RET （可在标题行后输入其他，可回车，多行）
;;;###autoload
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

(provide 'init-gptel)

;;; init-gptel.el ends here
