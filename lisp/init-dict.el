;;; init-dict.el --- dict -*- lexical-binding: t -*-

;; Author: shaowen he

;;; Commentary:

;; 与字典相关，英文翻译等

;;; Code:

;; ~/.authinfo
;; 使用百度翻译api进行翻译，100W/月  -- 用量短信提醒
;; brew install translate-shell
;; 改为使用bing
(use-package immersive-translate
  :init
  ;; (setq immersive-translate-backend 'baidu
  ;;       immersive-translate-baidu-appid "20240604002069851")
  (setq immersive-translate-backend 'trans
        immersive-translate-trans-engine "bing")
  :config
  (add-hook 'elfeed-show-mode-hook #'immersive-translate-setup)
  (add-hook 'nov-pre-html-render-hook #'immersive-translate-setup))


;; Centaur
;; 另一种翻译，选择使用
;; 分别提供4种字典，速度上有点慢，也可使用
(use-package fanyi
  :bind (("C-c d f" . fanyi-dwim)
         ("C-c d d" . fanyi-dwim2)
         ("C-c d h" . fanyi-from-history))
  :custom
  (fanyi-providers '(;; 海词
                     fanyi-haici-provider
                     ;; 有道同义词词典
                     fanyi-youdao-thesaurus-provider
                     ;; Etymonline - 英英词典
                     fanyi-etymon-provider
                     ;; Longman - 英英词典
                     fanyi-longman-provider)))


;; 设置三个翻译器，使用函数来调用，选择使用
;; 新增同化包posframe
(use-package go-translate
  :bind (("C-c d g" . gt-do-translate)
         ("C-c d G" . gt-do-translate-prompt)
         ("C-c d p" . gt-do-speak)
         ("C-c d s" . gt-do-setup)
         ("C-c d u" . gt-do-text-utility))
  :init
  (setq gt-langs '(en zh)
        gt-buffer-render-follow-p t
        gt-buffer-render-window-config
        '((display-buffer-reuse-window display-buffer-in-direction)
          (direction . bottom)
          (window-height . 0.4)))

  (setq gt-pop-posframe-forecolor (face-foreground 'tooltip nil t)
        gt-pop-posframe-backcolor (face-background 'tooltip nil t))
  (when (facep 'posframe-border)
    (setq gt-pin-posframe-bdcolor (face-background 'posframe-border nil t)))
  :config
  (with-no-warnings
    (setq gt-preset-translators
          `((default . ,(gt-translator
                         :taker   (list (gt-taker :pick nil :if 'selection)
                                        (gt-taker :text 'paragraph :if '(Info-mode help-mode helpful-mode devdocs-mode))
                                        (gt-taker :text 'buffer :pick 'fresh-word :if 'read-only)
                                        (gt-taker :text 'word))
                         :engines (if (display-graphic-p)
                                      (list (gt-bing-engine :if 'not-word)
                                            (gt-youdao-dict-engine :if 'word))
                                    (list (gt-bing-engine :if 'not-word)
                                          (gt-youdao-dict-engine :if 'word)
                                          (gt-youdao-suggest-engine :if 'word)
                                          (gt-google-engine :if 'word)))
                         :render  (list (gt-posframe-pop-render :if (lambda (translator)
                                                                      (and (display-graphic-p)
                                                                           (not (derived-mode-p 'Info-mode 'help-mode 'helpful-mode 'devdocs-mode))
                                                                           (not (member (buffer-name) '("COMMIT_EDITMSG")))))
                                                                :frame-params (list :accept-focus nil
                                                                                    :width 70
                                                                                    :height 15
                                                                                    :left-fringe 16
                                                                                    :right-fringe 16
                                                                                    :border-width 1
                                                                                    :border-color gt-pin-posframe-bdcolor))
                                        (gt-overlay-render :if 'read-only)
                                        (gt-insert-render :if (lambda (translator) (member (buffer-name) '("COMMIT_EDITMSG"))))
                                        (gt-buffer-render))))
            (multi-dict . ,(gt-translator :taker (gt-taker :langs '(en zh) :prompt t)
                                          :engines (list (gt-bing-engine)
                                                         (gt-youdao-dict-engine)
                                                         (gt-youdao-suggest-engine :if 'word)
                                                         (gt-google-engine))
                                          :render (gt-buffer-render)))
            (Text-Utility . ,(gt-text-utility :taker (gt-taker :pick nil)
                                              :render (gt-buffer-render)))))

    ;; 结合posframe弹窗来显示当前光标处的单词
    (defun gt--do-translate (dict)
      "Translate using DICT from the preset tranlators."
      (gt-start (alist-get dict gt-preset-translators)))

    ;; 通过提示来查询其他单词，使用多字典
    (defun gt-do-translate-prompt ()
      "Translate with prompt using the multiple dictionaries."
      (interactive)
      (gt--do-translate 'multi-dict))

    ;; 文本编码 md5/base64/sha1
    (defun gt-do-text-utility ()
      "Handle the texts with the utilities."
      (interactive)
      (gt--do-translate 'Text-Utility))))


;; OSX dictionary：使用macOS中的字典接口来查询
(when sys/macp
  (use-package osx-dictionary
    :bind (("C-c d i" . osx-dictionary-search-input)
           ("C-c d x" . osx-dictionary-search-pointer))))


(provide 'init-dict)

;;; init-dict.el ends here
