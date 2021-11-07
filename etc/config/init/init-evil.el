;;; init-evil.el -*- lexical-binding: t; -*-

;;; Commentary:

;; 

;;; Code:

(leaf general
  :config
  ;; 将<SPC>设为leader键之一
  ;; 与当前编辑模式或编辑无关的通用操作，诸如打开文件、保存文件、切换minor-mode等
  (general-create-definer gaeric-space-leader-def
    :prefix "SPC"
    :states '(normal visual))
  ;; Spaces keybinds for vanilla Emacs
  (gaeric-space-leader-def
    "f"     '(:wk "file operation or find with jump")
    "ff"    'find-file
    "fo"    'find-file-other-window
    "fs"    '(save-buffer :wk "file save")
    "fl"    'leaf-find-with-unit
    ;;"fed"   'open-init-file
    "b"     '(:wk "buffer operation")
    "bo"    'switch-to-buffer-other-window
    "bw"    '(kill-this-buffer :wk "like H-w but with kill this buffer")
    "bs"    '(consult-buffer :wk "swith to buffer in current window")
    ;;"bc"    '(switch-to-scratch-buffer :wk "*scratch*")
    ;;"bm"    'switch-to-message-buffer

    ;;"w/"    'split-window-right
    ;;"w-"    'split-window-below
    ;;"ad"    'dired
    ;;"tl"    'toggle-truncate-lines
    ;;"tn"    'linum-mode
    ;;"wc"    'count-words
    ;;"nw"    'widen
    "!"     'shell-command
    "n"     '(:wk "note org-roam and bujo")
    "nn"    '(org-roam-node-find :wk "node")
    "ni"    '(org-roam-node-insert :wk "insert node")
    "nj"    '(org-roam-dailies-goto-today :wk "jump to today")
    "nr"    '(ns/org-roam-rg-search :wk "rg search your roam note")
    "nf"    '(ns/org-roam-rg-file-search :wk "find roam file")
    "nb"    '(:wk "note bujo")
    "nbj"   '(bujo/today-another-window :wk "bujo jump today")
    "nbm"   '(bujo/month-page-another-window :wk "bujo current moth")
    )
  ;; 将,键设置为另一个leader
  ;; 与编辑或编辑模式强相关的操作，诸如：org-mode下打开org-pomodoro、org-clock等
  (general-create-definer gaeric-comma-leader-def
    :prefix ","
    :states '(normal visual))
  (gaeric-comma-leader-def
    "c"   '(:wk "copy-paset-killring")
    "cc"  'copy-to-x-clipboard
    "cp"  'paste-from-x-clipboard
    "ck"  'kill-ring-to-clipboard
    ","   'evil-repeat-find-char-reverse
    "f"   '(:wk "find and jump")
    "b"   '(:wk "bujo wf")
    "bc"  'bujo/check-task
    "bt"  'bujo/set-current-task-state
    )
  )

(leaf evil
  :require evil evil-core evil-common
  :config
  ;; 切换至normal模式时，光标会回退一位（与vim行为保持一致）
  (setq evil-move-cursor-back t)
  (evil-declare-key 'normal org-mode-map
    ;;smarter behaviour on headlines 
    "$" 'org-end-of-line
    "^" 'org-beginning-of-line 
    (kbd "TAB") 'org-cycle);; ditto
  (setcdr evil-insert-state-map nil)
  (define-key evil-insert-state-map [escape] 'evil-normal-state)
  (evil-mode 1));; end of leaf evil

(provide 'init-evil)
;;; init-evil.el ends here