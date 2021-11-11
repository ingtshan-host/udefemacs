;;; init-editor.el -*- lexical-binding: t; -*-

;;; Commentary:

;; make emacs a better editor

;;; Code:

(leaf indent-guide
  :hook ((emacs-lisp-mode-hook . indent-guide-mode)))

;; markdown support
(leaf markdown-mode)
;; todo
;; Markdown/Org 实时预览插件：grip\-mode \- Emacs\-general \- Emacs China
;; https://emacs-china.org/t/markdown-org-grip-mode/10262/47

(leaf avy)
(leaf ace-pinyin)

(leaf-unit cursor-movement
  ;; 我混用emacs 和evil
  ;; globle C-a 跳到行首
  ;; globle C-e 跳到行尾
  ;; globle M-< 或 normal gg 跳到buffer 开头
  ;; globle M-> 或 normal G 跳到buffer 结尾
  ;; normal C-d 向下翻半页
  ;; normal C-u 向上翻半页
  ;; normal e/E 向后移动一个word/（把标点记入word）
  ;; normal b/B 向前移动一个word/（把标点记入word）
  ;; global C-p/C-n normal j/k 上下一行
  ;; global C-f/C-b normal lh 前后
  ;; global C-j c/l avy-goto-char/line
  
  (with-eval-after-load 'evil
    
    (evil-define-key '(normal visual) 'global
      (kbd "C-a") 'move-beginning-of-line
      (kbd "C-e") 'move-end-of-line
      (kbd "C-d") 'evil-scroll-down
      (kbd "C-u") 'evil-scroll-up
      (kbd "C-n") 'next-line
      (kbd "C-p") 'previous-line
      (kbd "C-f") 'forward-char
      (kbd "C-b") 'backward-char
      ))

  (with-eval-after-load 'general
    
    (global-unset-key (kbd "C-j"))
    
    (general-define-key
     :keymaps 'org-mode-map
     "C-j" 'nil)
    
    (general-define-key
     :prefix "C-j"
     ;; bind "C-c a" to 'org-agenda
     "c" 'ace-pinyin-dwim
     "l" 'avy-goto-line
     ))
  );; end of +unit-cursor-movement



(provide 'init-editor)
;;; init-editor.el ends here
