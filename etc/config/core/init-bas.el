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
;; set right command key of macOS
(setq mac-command-modifier 'hyper mac-option-modifier 'meta)
;; what different between (kbd "H-v") and [(hyper v)] ?
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
)

(provide 'init-bas)
;;; init-bas.el ends here
