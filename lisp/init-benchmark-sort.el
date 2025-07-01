;;; init-benchmark-sort.el --- 对benchmark结果进行排序 -*- lexical-binding: t -*-

;; Author: shaowen he

;;; Commentary:

;; 提供对benchmark-init结果进行排序的功能，帮助快速识别启动性能瓶颈
;;
;; 使用方法：
;; 1. M-x benchmark-init/show-durations-tree
;; 2. 在Benchmark Init Tree mode中自动可用以下快捷键：
;;    - s: 按时间排序
;;    - n: 按名称排序
;;    - f: 按时间过滤
;;    - S: 显示摘要统计
;;
;; 特性：
;; - 自动排除benchmark-init/root根节点
;; - 支持evil模式，自动取消冲突的快捷键
;; - 提供详细的性能分析和优化建议

;;; Code:

(defun benchmark-sort-parse-line (line)
  "解析benchmark结果行，提取模块名和时间."
  (condition-case nil
      (when (string-match "\\[\\([^]]+\\) \\([^]]+\\) \\([0-9]+\\)ms\\]" line)
        (let ((name (match-string 1 line))
              (type (match-string 2 line))
              (time (string-to-number (match-string 3 line)))
              (indent (condition-case nil
                          (length (replace-regexp-in-string "[^│├╰─ ]" "" line))
                        (error 0))))
          (list :name name :type type :time time :indent indent :original line)))
    (error nil)))

(defun benchmark-sort-extract-entries ()
  "从当前buffer提取所有benchmark条目，排除根节点."
  (let ((entries '())
        (lines (split-string (buffer-string) "\n")))
    (dolist (line lines)
      (when (string-match "\\[.*[0-9]+ms\\]" line)
        (let ((entry (benchmark-sort-parse-line line)))
          (when (and entry
                     ;; 排除根节点 benchmark-init/root
                     (not (string= (plist-get entry :name) "benchmark-init/root")))
            (push entry entries)))))
    (nreverse entries)))

(defun benchmark-sort-by-time ()
  "按加载时间对benchmark结果进行排序."
  (interactive)
  (let* ((entries (benchmark-sort-extract-entries))
         (sorted-entries (sort entries (lambda (a b) (> (plist-get a :time) (plist-get b :time)))))
         (buffer-name "*Benchmark Results Sorted by Time*"))

    (with-current-buffer (get-buffer-create buffer-name)
      (erase-buffer)
      (insert "Benchmark结果 - 按加载时间排序\n")
      (insert "================================\n\n")
      (insert "🐌 最耗时的模块 (前20个):\n")
      (insert "------------------------\n")

      (let ((count 0))
        (dolist (entry sorted-entries)
          (when (< count 20)
            (let ((time (plist-get entry :time))
                  (name (plist-get entry :name))
                  (type (plist-get entry :type)))
              (insert (format "%3d. %4dms - %s (%s)\n"
                              (1+ count) time name type))
              (setq count (1+ count))))))

      (insert "\n📊 时间分布统计:\n")
      (insert "----------------\n")
      (let ((total-time 0)
            (over-100ms 0)
            (over-50ms 0)
            (over-20ms 0))
        (dolist (entry sorted-entries)
          (let ((time (plist-get entry :time)))
            (setq total-time (+ total-time time))
            (when (>= time 100) (setq over-100ms (1+ over-100ms)))
            (when (>= time 50) (setq over-50ms (1+ over-50ms)))
            (when (>= time 20) (setq over-20ms (1+ over-20ms)))))

        (insert (format "总加载时间: %dms\n" total-time))
        (insert (format "超过100ms的模块: %d个\n" over-100ms))
        (insert (format "超过50ms的模块: %d个\n" over-50ms))
        (insert (format "超过20ms的模块: %d个\n" over-20ms))
        (insert (format "总模块数: %d个\n" (length sorted-entries))))

      (insert "\n🎯 优化建议:\n")
      (insert "------------\n")
      (dolist (entry (seq-take sorted-entries 5))
        (let ((time (plist-get entry :time))
              (name (plist-get entry :name)))
          (cond
           ((>= time 200)
            (insert (format "• %s (%dms): 严重性能瓶颈，建议延迟加载\n" name time)))
           ((>= time 100)
            (insert (format "• %s (%dms): 较大性能影响，考虑优化\n" name time)))
           ((>= time 50)
            (insert (format "• %s (%dms): 中等性能影响，可以优化\n" name time))))))

      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(defun benchmark-sort-by-name ()
  "按模块名对benchmark结果进行排序."
  (interactive)
  (let* ((entries (benchmark-sort-extract-entries))
         (sorted-entries (sort entries (lambda (a b)
                                         (string< (plist-get a :name) (plist-get b :name)))))
         (buffer-name "*Benchmark Results Sorted by Name*"))

    (with-current-buffer (get-buffer-create buffer-name)
      (erase-buffer)
      (insert "Benchmark结果 - 按模块名排序\n")
      (insert "==============================\n\n")

      (dolist (entry sorted-entries)
        (let ((time (plist-get entry :time))
              (name (plist-get entry :name))
              (type (plist-get entry :type)))
          (insert (format "%-40s %4dms (%s)\n" name time type))))

      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(defun benchmark-sort-filter-by-time (min-time)
  "过滤显示加载时间超过MIN-TIME的模块."
  (interactive "n最小时间 (ms): ")
  (let* ((entries (benchmark-sort-extract-entries))
         (filtered-entries (seq-filter (lambda (entry)
                                         (>= (plist-get entry :time) min-time))
                                       entries))
         (sorted-entries (sort filtered-entries (lambda (a b)
                                                  (> (plist-get a :time) (plist-get b :time)))))
         (buffer-name (format "*Benchmark Results >%dms*" min-time)))

    (with-current-buffer (get-buffer-create buffer-name)
      (erase-buffer)
      (insert (format "Benchmark结果 - 加载时间超过%dms的模块\n" min-time))
      (insert "==========================================\n\n")

      (if (null sorted-entries)
          (insert (format "没有找到加载时间超过%dms的模块\n" min-time))
        (progn
          (insert (format "找到%d个加载时间超过%dms的模块:\n\n"
                          (length sorted-entries) min-time))
          (dolist (entry sorted-entries)
            (let ((time (plist-get entry :time))
                  (name (plist-get entry :name))
                  (type (plist-get entry :type)))
              (insert (format "%4dms - %s (%s)\n" time name type))))))

      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(defun benchmark-sort-show-summary ()
  "显示benchmark结果的摘要统计."
  (interactive)
  (let* ((entries (benchmark-sort-extract-entries))
         (buffer-name "*Benchmark Summary*"))

    (with-current-buffer (get-buffer-create buffer-name)
      (erase-buffer)
      (insert "Benchmark结果摘要\n")
      (insert "==================\n\n")

      (let ((total-time 0)
            (init-modules 0)
            (require-modules 0)
            (load-modules 0)
            (time-ranges (make-hash-table :test 'equal)))

        ;; 统计数据
        (dolist (entry entries)
          (let ((time (plist-get entry :time))
                (type (plist-get entry :type)))
            (setq total-time (+ total-time time))
            (cond
             ((string= type "require") (setq require-modules (1+ require-modules)))
             ((string= type "load") (setq load-modules (1+ load-modules)))
             ((string-prefix-p "init-" (plist-get entry :name))
              (setq init-modules (1+ init-modules))))

            ;; 时间范围统计
            (cond
             ((>= time 200) (puthash "200ms+" (1+ (gethash "200ms+" time-ranges 0)) time-ranges))
             ((>= time 100) (puthash "100-199ms" (1+ (gethash "100-199ms" time-ranges 0)) time-ranges))
             ((>= time 50) (puthash "50-99ms" (1+ (gethash "50-99ms" time-ranges 0)) time-ranges))
             ((>= time 20) (puthash "20-49ms" (1+ (gethash "20-49ms" time-ranges 0)) time-ranges))
             ((>= time 10) (puthash "10-19ms" (1+ (gethash "10-19ms" time-ranges 0)) time-ranges))
             (t (puthash "<10ms" (1+ (gethash "<10ms" time-ranges 0)) time-ranges)))))

        ;; 显示统计结果
        (insert (format "📊 总体统计:\n"))
        (insert (format "  总启动时间: %dms (%.2f秒)\n" total-time (/ total-time 1000.0)))
        (insert (format "  总模块数: %d个\n" (length entries)))
        (insert (format "  Init模块: %d个\n" init-modules))
        (insert (format "  Require模块: %d个\n" require-modules))
        (insert (format "  Load文件: %d个\n\n" load-modules))

        (insert "⏱️ 时间分布:\n")
        (let ((has-data nil)
              (total-entries (length entries)))
          (dolist (range '("200ms+" "100-199ms" "50-99ms" "20-49ms" "10-19ms" "<10ms"))
            (let ((count (gethash range time-ranges 0)))
              (when (> count 0)
                (setq has-data t)
                (insert (format "  %s: %d个模块\n" range count)))))
          (unless has-data
            (insert (format "  无时间分布数据 (总条目数: %d)\n" total-entries))
            ;; 调试信息：显示哈希表内容
            (insert "  调试信息 - 哈希表内容:\n")
            (maphash (lambda (key value)
                       (insert (format "    %s: %d\n" key value)))
                     time-ranges)))

        (insert "\n🎯 性能建议:\n")
        (let ((slow-count (+ (gethash "200ms+" time-ranges 0)
                             (gethash "100-199ms" time-ranges 0))))
          (if (> slow-count 0)
              (insert (format "  发现%d个较慢的模块，建议优化或延迟加载\n" slow-count))
            (insert "  启动性能良好，无明显瓶颈\n"))))

      (goto-char (point-min))
      (display-buffer (current-buffer)))))

;; 简洁的快捷键设置 - 直接使用evil-define-key覆盖
(with-eval-after-load 'benchmark-init-modes
  (when (boundp 'benchmark-init/tree-mode-map)
    ;; 直接使用evil-define-key设置快捷键，会覆盖原有绑定
    (when (featurep 'evil)
      (evil-define-key 'normal benchmark-init/tree-mode-map
        "s" 'benchmark-sort-by-time
        "n" 'benchmark-sort-by-name
        "f" 'benchmark-sort-filter-by-time
        "S" 'benchmark-sort-show-summary
        "q" 'quit-window)

      ;; 同时设置到普通的mode-map（非evil用户）
      (define-key benchmark-init/tree-mode-map (kbd "s") 'benchmark-sort-by-time)
      (define-key benchmark-init/tree-mode-map (kbd "n") 'benchmark-sort-by-name)
      (define-key benchmark-init/tree-mode-map (kbd "f") 'benchmark-sort-filter-by-time)
      (define-key benchmark-init/tree-mode-map (kbd "S") 'benchmark-sort-show-summary)
      (define-key benchmark-init/tree-mode-map (kbd "q") 'quit-window)

      (message "✅ Benchmark排序快捷键已设置: s(时间排序) n(名称排序) f(过滤) S(摘要)"))))

(provide 'init-benchmark-sort)

;;; init-benchmark-sort.el ends here
