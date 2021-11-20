;;; init-bas.el -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:

;; basic setup of my emacs
;; os key shortcut

;;; Code:

(leaf-unit os-bas
  ;; function
  (defun bas/do-kill-9 (&optional pfx)
    "quit emacs with confirm"
    (when (or pfx (y-or-n-p "Quit emacs now?"))
      (save-buffers-kill-terminal)))

  (defun bas/quit-emacs (&optional pfx)
    "quit emacs with need-test check"
    (interactive "P")
    (save-some-buffers)
    (cond
     ;; ((or nil
     ;; 	(and *need-test*
     ;;                (y-or-n-p "Seems Your init configs need test, do it?")))
     ;;  (wf/int-test-gui))
     (t (bas/do-kill-9))))

  (defun bas/close-frame (&optional pfx)
    "close emacs frame"
    (interactive "P")
    (let ((q nil))
      (condition-case ex
	      (delete-window) ('error (setq q t)))
      (if q (progn (setq q nil)
		           (condition-case ex
		               (delete-frame) ('error (setq q t)))
		           (if q (bas/quit-emacs pfx))))))
  ;; os hot key
  ;; set right command key of macOS
  (setq mac-command-modifier 'hyper mac-option-modifier 'meta)
  ;; what different between (kbd "H-v") and [(hyper v)] ?
  ;; os shortcut
  (global-set-key (kbd "H-a") #'mark-page)         ; 全选
  (global-set-key (kbd "H-v") #'yank)              ; 粘贴
  (global-set-key (kbd "H-x") #'kill-region)       ; 剪切
  (global-set-key (kbd "H-c") #'kill-ring-save)    ; 复制
  (global-set-key (kbd "H-s") #'save-buffer)       ; 保存
  (global-set-key (kbd "H-z") #'undo)             ; 撤销编辑修改
  (global-set-key (kbd "H-l") #'goto-line)         ; 行跳转
  (global-set-key [(hyper n)] #'make-frame-command); 新建窗口
  (global-set-key [(hyper q)] #'bas/quit-emacs)     ; 退出
  (global-set-key [(hyper w)] #'bas/close-frame)    ; 退出frame
  ;; make select more like other editro
  (delete-selection-mode 1)
  ;; use shift to extend select
  (global-set-key (kbd "<S-down-mouse-1>") #'mouse-save-then-kill)

  ;; os default
  (setq visible-bell t
        inhibit-compacting-font-caches t  ; Don’t compact font caches during GC.
        delete-by-moving-to-trash t       ; Deleting files go to OS's trash folder
        ;; Show path if names are same
        uniquify-buffer-name-style 'post-forward-angle-brackets)
  )

(leaf-unit editor-bas

  (setq-default
   major-mode 'text-mode
   fill-column 128
   tab-width 4
   ;; Permanently indent with spaces, never with TABs
   indent-tabs-mode nil)

  (setq
   adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*"
   adaptive-fill-first-line-regexp "^* *$"
   sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
   sentence-end-double-space nil)
  )

;; auto indent
(leaf aggressive-indent
  :hook ((emacs-lisp-mode-hook . aggressive-indent-mode)))

;; snippet
;; `yasnippet'
(leaf yasnippet
  :diminish (yas-minor-mode)
  :hook (after-init-hook . yas-global-mode)
  :config
  (leaf yasnippet-snippets))

(leaf transpose-frame)

;; window movement
(leaf ace-window
  :preface
  (defvar aw-dispatch-alist
    '((?x aw-delete-window "Delete Window")
      (?m aw-swap-window "Swap Windows")
      (?M aw-move-window "Move Window")
      (?c aw-copy-window "Copy Window")
      ;; with cursor move
      (?s aw-switch-buffer-in-window "Select Buffer")
      (?n aw-flip-window)
      ;; without cursor move
      (?b aw-switch-buffer-other-window "Switch Buffer Other Window")
      (?e aw-execute-command-other-window "Execute Command Other Window")
      (?F aw-split-window-fair "Split Fair Window")
      (?v aw-split-window-vert "Split Vert Window")
      (?h aw-split-window-horz "Split Horz Window")
      (?o delete-other-windows "Delete Other Windows")
      (?T aw-transpose-frame "Transpose Frame")
      ;; ?i ?r ?t are used by hyperbole.el
      (?? aw-show-dispatch-help))
    "List of actions for `aw-dispatch-default'.
Each action is a list of either:
  (char function description) where function takes a single window argument
or
  (char function) where function takes no argument and the description is omitted.")
  :config
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:foreground "orange" :height 2.5)))))

  (setq aw-keys '(?a ?u ?d ?f ?g ?i ?j ?k ?l)
        aw-dispatch-always t
        aw-background nil))

;; save file when unfocus
(leaf super-save
  :config
  (super-save-mode +1))

;; (leaf auto-save
;;   :straight (auto-save :type git :host github
;;                        :repo "manateelazycat/auto-save")
;;   :require t
;;   :config
;;   (setq auto-save-silent t ; quietly save
;;         auto-save-delete-trailing-whitespace t ; blank trim
;;         auto-save-idle 1.5) ; wait for 1.5s

;;   ;;; custom predicates if you don't want auto save.
;; ;;; disable auto save mode when current filetype is an gpg file.
;;   (setq auto-save-disable-predicates
;;         '((lambda ()
;;             (string-suffix-p
;;              "gpg"
;;              (file-name-extension (buffer-name)) t))))

;;   (auto-save-enable))

(provide 'init-bas)
;;; init-bas.el ends here
