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
    "fS"    '(save-some-buffers :wk "save all modified buffers")
    "fl"    'leaf-find-with-unit
    "fi"    'config/init-file-other-window
    "fp"    '(:wk "projectile operation")
    "fpf"   'projectile-find-file
    "fpd"   'projectile-find-dir
    "fps"   'projectile-switch-project
    "fpr"   'denv/projectile-rg-search
    ;;"fed"   'open-init-file
    "b"     '(:wk "buffer operation")
    "bo"    'switch-to-buffer-other-window
    "bw"    '(kill-this-buffer :wk "like H-w but with kill this buffer")
    "bs"    '(consult-buffer :wk "swith to buffer in current window")
    "bp"    'consult-projectile
    "bk"    'kill-buffer
    ;;"bc"    '(switch-to-scratch-buffer :wk "*scratch*")
    ;;"bm"    'switch-to-message-buffer
    "g"     '(:wk "git operation")
    "gg"    'magit
    "w"     '(ace-window :wk "window operation")
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
    "nbj"   '(bujo/today-other-window :wk "bujo jump today")
    "nbf"   '(bujo/find-task :wk "bujo task current moth")
    "nbm"   '(bujo/month-page-other-window :wk "bujo current moth")
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
    "e"   '(:wk "eval operation")
    "ee"  'eval-last-sexp
    "eb"  'eval-buffer
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
  
  (setq evil-normal-state-cursor  '("DarkGoldenrod2" box)
        evil-insert-state-cursor  '("chartreuse3" (bar . 2))
        evil-emacs-state-cursor   '("SkyBlue2" box)
        evil-replace-state-cursor '("chocolate" (hbar . 2))
        evil-visual-state-cursor  '("gray" (hbar . 2))
        evil-motion-state-cursor  '("plum3" box))
  
  (setcdr evil-insert-state-map nil)
  (define-key evil-insert-state-map [escape] 'evil-normal-state)
  (evil-mode 1));; end of leaf evil

(provide 'init-evil)
;;; init-evil.el ends here
