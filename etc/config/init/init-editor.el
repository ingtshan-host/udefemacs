;;; init-editor.el -*- lexical-binding: t; -*-

;;; Commentary:

;; make emacs a better editor

;;; Code:

;; auto indent
(leaf aggressive-indent
  :hook ((emacs-lisp-mode-hook . aggressive-indent-mode)))

(leaf indent-guide
  :hook ((emacs-lisp-mode-hook . indent-guide-mode)))

;; markdown support
(leaf markdown-mode)
;; todo
;; Markdown/Org 实时预览插件：grip\-mode \- Emacs\-general \- Emacs China
;; https://emacs-china.org/t/markdown-org-grip-mode/10262/47

(leaf avy :require t)
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


(leaf sis
  :config
  (sis-ism-lazyman-config
   "com.apple.keylayout.US"
   "com.sogou.inputmethod.sogou.pinyin")

  ;; enable the /cursor color/ mode
  ;;(sis-global-cursor-color-mode t)
  ;; enable the /respect/ mode
  (sis-global-respect-mode t)
  ;; enable the /context/ mode for all buffers
  (sis-global-context-mode t)
  ;; enable the /inline english/ mode for all buffers
  ;;(sis-global-inline-mode t)
  )

(leaf nlinum
  :hook ((org-mode-hook . nlinum-mode)
         (prog-mode-hook . nlinum-mode))
  :config
  ;; fix hl
  (leaf nlinum-hl :require t)
  (setq nlinum-highlight-current-line t)
  (defconst my-nlinum-format-function
    (lambda (line width)
      (let* ((is-current-line (= line nlinum--current-line))
             (str (format nlinum-format line)))
        ;; use -> as current line
        ;; or change to any symbol you like
        ;; here
        ;; (and is-current-line (setq str "->"))
        (when is-current-line
          (let* ((ms "->")
                 (el (- (length str) 2)))
            (while (> el 0)
              (setq ms (concat "-" ms))
              (setq el (1- el)))
            (setq str ms)))
        (when (< (length str) width)
          ;; Left pad to try and right-align the line-numbers.
          (setq str (concat (make-string (- width (length str)) ?\ ) str)))
        
        (put-text-property 0 width 'face
                           (if (and nlinum-highlight-current-line
                                    is-current-line)
                               'nlinum-current-line
                             'linum)
                           str)
        str)))
  ;;take effect
  (setq nlinum-format-function my-nlinum-format-function))

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

;; my highlight

(defun editor/unhi-all-regexp ()
  "my unhighlight method"
  (interactive)
  ;; loop list and unhighlight
  (when hi-lock-interactive-patterns
    (dolist (r (mapcar
                (lambda (pattern)
                  (substring-no-properties (car pattern)))
                hi-lock-interactive-patterns))
      (unhighlight-regexp r))))

(defun editor/hi-lock-regexp (regexp &optional face)
  "my highlight method"
  
  (or face (setq face (hi-lock-read-face-name)))
  (setq-local editor/hl--last-regexp regexp)
  (unless face 
    (setq regexp (hi-lock-regexp-okay regexp)))
  (or (facep face) (setq face 'hi-yellow))
  (unless hi-lock-mode (hi-lock-mode 1))
  
  (hi-lock-set-pattern regexp face))

(defun editor/hi-lock-regexp-dwim ()
  (interactive)
  (or (boundp 'hi-lock-interactive-patterns)
      (ignore-errors(call-interactively 'unhighlight-regexp)))

  (let ((regexp nil)
        (face nil))
    (cond
     ((region-active-p)
      (setq regexp (buffer-substring (region-beginning) (region-end))))
     ((thing-at-point 'word)
      (setq regexp (thing-at-point 'word))))

    (unless regexp (error "no symbol here"))
    
    (unless hi-lock-interactive-patterns
      (setq face 'hi-salmon))
    
    (if (member regexp
                (mapcar
                 (lambda (pattern)                    
                   (car pattern))
                 hi-lock-interactive-patterns))
        
        (unhighlight-regexp regexp)
      (editor/hi-lock-regexp regexp face))))

(defun editor/hi-search-forward ()
  (interactive)
  (if hi-lock-interactive-patterns
      ;; then
      (let ((regex nil))
        (if (eq 1 (length hi-lock-interactive-patternsi))
            (setq regex (substring-no-properties (caar hi-lock-interactive-patterns)))
          (setq regex (completing-read "Select" hi-lock-interactive-patterns)))
        (search-forward regex))
    ;; else
    (call-interactively 'search-forward)))

(defun editor/hi-search-backward ()
  (interactive)
  (if hi-lock-interactive-patterns
      ;; then
      (let ((regex nil))
        (if (eq 1 (length hi-lock-interactive-patterns))
            (setq regex (substring-no-properties (caar hi-lock-interactive-patterns)))
          (setq regex (completing-read "Select" hi-lock-interactive-patterns)))
        (search-backward regex))
    ;; else
    (call-interactively 'search-backward)))

(defun editor/hi-query-replace-regexp ()
  (interactive)
  (if hi-lock-interactive-patterns
      ;; then
      (let ((regex nil))
        (if (eq 1 (length hi-lock-interactive-patterns))
            (setq regex (substring-no-properties (caar hi-lock-interactive-patterns)))
          (setq regex (completing-read "Select to match" hi-lock-interactive-patterns)))
        (query-replace-regexp regex (read-string (format "Replace %s with :" regex))))
    ;; else
    (call-interactively 'query-replace-regexp)))

  (provide 'init-editor)
;;; init-editor.el ends here
