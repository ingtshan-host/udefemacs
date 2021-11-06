;;; init-bujo.el -*- lexical-binding: t; -*-

;;; Commentary:

;; 

;;; Code:

(leaf-unit bujo-wf
  (defun bujo/month-page-another-window ()
    (interactive)
    (find-file-other-window
     (expand-file-name
      (concat "~/org/roam-v2/bujo/month.d/bujo-"
              (format-time-string "%Y%m") ".org")))
    (goto-char (point-min))
    (re-search-forward
     (concat "[[:space:]]*[[:space:]]"(format-time-string "%Y-%m")))
    (match-string-no-properties 0)
    )
  
  (defun bujo/today-another-window ()
    (interactive)
    (find-file-other-window
     (expand-file-name
      (concat "~/org/roam-v2/bujo/month.d/bujo-"
              (format-time-string "%Y%m") ".org")))
    (goto-char (point-min))
    (re-search-forward
     (concat "[[:space:]]"(format-time-string "%F")))
    (match-string-no-properties 0)
    )

  (defun bujo/current-cell ()
    "Return (bge . end) is point to a table cell
Otherwise return (nil)"
    (let ((beg nil)
          (end nil))

      (save-excursion
        (save-restriction
          (narrow-to-region
           (line-beginning-position)
           (line-end-position))

          (save-excursion
            ;; once match then stop and update beg
            (and (search-backward "|" nil "noerror")
                 (setq beg (point))))

          (and beg
               (save-excursion
                 ;; once match then stop and update end
                 (and (search-forward "|" nil "noerror")
                      (setq end (point)))))))

      (cons beg end)))

  (defun bujo/set-task-state (state)
    "Search bullet in current cell and change it to input state"
    (let ((cell (bujo/current-cell)))
      (when (cdr cell)
        (save-excursion
          (save-restriction
            (narrow-to-region
             (car cell)
             (cdr cell))
            (goto-char (point-min))
            (while (search-forward-regexp "[x\<\>\-]" nil "noerror")
              (replace-match state)))))))

  (defun bujo/check-task ()
    "Set task state to x"
    (interactive)
    (let ((cell (bujo/current-cell)))
      (when (cdr cell)
        (save-excursion
          (save-restriction
            (narrow-to-region
             (car cell)
             (cdr cell))
            (goto-char (point-min))
            (while (search-forward-regexp "[x\<\>\-]" nil "noerror")
              (if (string= (match-string 0) "x")
                  (replace-match "-") ;; redo
                (replace-match "x"))
              ))))))

  (defun bujo/set-current-task-state ()
    "Search bullet in current cell and change it to input state"
    (interactive)
    (let* ((cell (bujo/current-cell))
           (choice (if (cdr cell)
                       (completing-read
                        "新状态为:"             
                        '("- 待办" "< 计划" "x 完成" "> 转移"))
                     nil)))
      (when choice
        (save-excursion
          (save-restriction
            (narrow-to-region
             (car cell)
             (cdr cell))
            (goto-char (point-min))
            (while (search-forward-regexp "[x\<\>\-]" nil "noerror")
              (replace-match (substring choice 0 1))))))))

  );;end of bujo-wf

(leaf-unit log-wf
(defun log/org-create-current-file-d-subfile ()
  "workflow of logging
noviemacs-principal:
问题-答案，功能-方案，目的-过程 等自上而下带指导"
  (interactive)
  (let* ((title (read-string "问题/功能/目的："))
         ;;，。《》？【】「」、～（）——！｜‘’“”；：
         (rl '("[，。《》？【】「」、～（）——！｜‘’“”；：/ ]" . "\-"))
         (file (concat
                "./" (file-name-base (buffer-name)) ".d/"
                (if (string-match-p (car rl) title)
                    ;; use format title as filename
                    (replace-regexp-in-string
                     (car rl) (cdr rl) title) title) ".org")))
    ;; say we call at ~/.emacs.d/ with base name noviemacs-log
    ;; input title: 问题/功能/目的：log 流程
    ;; then file is relative path now as
    ;; ./noviemacs-log.d/问题-功能-目的-log-流程.el

    ;; leave link
    (insert (concat "[[" file "][" title "]]" ))
    ;; like
    ;; #+title: 问题/功能/目的：log 流程
    ;; #+author: ingtshan
    ;; #+date: <2021-10-29 Fri>

    ;; template

    ;; #+begin_quote
    ;; Summary:
    ;; 总结：

    ;; Concept and Solution:
    ;; 概念方法：
    ;; #+end_quote
    ;; - 自顶向下设计
    ;;   - 子功能

    ;; - Q
    ;;   - X false/or not now
    ;;   - A right
    ;;     - Q/A
    ;;     - S1 solution 1
    (unless (file-exists-p (file-truename file))
      (with-temp-buffer
        ;; title zoom
        (insert
         (concat "#+title: "
                 title
                 "\n#+author: ingtshan\n#+date: "
                 (format-time-string "<%F %a>\n\n")))
        ;; template content
        (insert "#+begin_quote\nSummary:\n总结：\n\nConcept and Solution:\n概念和方法：\n#+end_quote\n\n")
        (insert "- 自顶向下设计\n  - 子功能\n\n- Q\n  - X false/or not now\n  - A right\n    - Q/A\n    - S1 solution 1\n")
        (write-file (file-truename file))))))

)

(provide 'init-bujo)
;;;; init-bujo.el ends here

