;;; init-org.el -*- lexical-binding: t; -*-

;;; Commentary:

;; my org

;;; Code:

;; org-basic

(leaf org
  :require indent-guide
  :hook((org-mode-hook . org-indent-mode)
        (org-mode-hook . indent-guide-mode)
        (org-mode-hook
         .
         (lambda ()
           (visual-line-mode)
           (make-local-variable 'word-wrap)
           (setq word-wrap nil)
           )))

  :bind((org-mode-map
         ("H-k" . ns/org-kill-link-at-point))
        (org-mode-map
         ("H-j c" . bujo/check-task))
        (org-mode-map
         ("H-j t" . bujo/set-current-task-state)))
  :config
  (leaf org-contrib)
  ;; when opening a org file, don't collapse headings
  (setq org-startup-folded nil)

  ;; wrap long lines. don't let it disappear to the right
  ;; (setq org-startup-truncated t)
  ;; when in a url link, enter key should open it
  (setq org-return-follows-link t)

  ;; make org-mode” syntax color embedded source code
  (setq org-src-fontify-natively t)

  ;; how the source code edit buffer is displayed
  (setq org-src-window-setup 'current-window)
  (setq org-src-fontify-natively t)
  (setq org-agenda-window-setup 'current-window)

  ;; no blank line between insert heading
  ;; The default value is '((heading . auto) (plain-list-item . auto)),
  (setf org-blank-before-new-entry
        '((heading . nil) (plain-list-item . nil)))

  ;; better latex preview
  (setq org-preview-latex-default-process 'dvisvgm)
  
  ;; (setq org-directory "~/iCloud/org/")
  ;; (setq org-agenda-files '("~/iCloud/org/"))

  ;; better refile
  (setq org-refile-targets '((nil :maxlevel . 9)))

  ); end of leaf

(leaf org-appear
  :straight (org-appear :type git :host github :repo "ingtshan/org-appear"))

;; function of org

(defun ns/org-kill-link-at-point ()
  (interactive)
  (when (eq major-mode 'org-mode)
    (let* ((context (org-element-context))
           (type (org-element-type context))
           (beg (org-element-property :begin context))
           (end (org-element-property :end context)))
      (when (eq type 'link)
        (kill-region beg end)))))

(defun ns/open-inline-link ()
  "Follow the next link on the same line"
  (interactive)

  (condition-case nil
      (org-open-at-point)
    (error
     (save-excursion
       (let ((b (line-beginning-position))
             (e (line-end-position)))
         (org-next-link)
         (cond ((< (point) e)
                (org-open-at-point))
               (t (org-next-link t)
                  (if (< b (point))
                      (org-open-at-point)
                    (message "no link on current line")))))))))

(defun ns/org-insert-src-block-with-enter-edit (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
  (interactive
   (let ((src-code-types
	      '("emacs-lisp" "rust" "python" "C" "shell" "java" "js" "clojure" "C++" "css"
	        "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
	        "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
	        "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
	        "scheme" "sqlite" "html" "text")))
     (list (ido-completing-read "Source code type: " src-code-types))))
  (save-excursion
    (newline-and-indent)
    (insert (format "#+begin_src %s\n" src-code-type))
    (newline-and-indent)
    (insert "#+end_src\n")
    (previous-line 2)
    (org-edit-src-code)))

(defun ns/org-insert-src-block ()
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
  (let* ((src-code-types
	      '("emacs-lisp"
            "rust"
            "python"
            "C"
            "shell"
            "java"
            "js"
            "clojure"
            "C++"
            "css"
	        "calc"
            "asymptote"
            "dot"
            "gnuplot"
            "ledger"
            "lilypond"
            "mscgen"
	        "octave"
            "oz"
            "plantuml"
            "R"
            "sass"
            "screen"
            "sql"
            "awk"
            "ditaa"
	        "haskell"
            "latex"
            "matlab"
            "ocaml"
            "org"
            "perl"
            "ruby"
	        "scheme"
            "sqlite"
            "html"))
         (src-code-type
          (completing-read
           "Source code type: " src-code-types)))
    src-code-type))

;; (leaf org-persist
;;   :ensure nil
;;   :straight nil
;;   :custom
;;   ((org-persist-directory
;;     . `,(expand-file-name "var/org-persist/" user-emacs-directory))))

;; org src code block
(leaf org-src
  :ensure nil
  :straight nil

  :hook((org-indent-mode
         . (lambda()
             (diminish 'org-indent-mode)
             ;; WORKAROUND: Prevent text moving around while using brackets
             ;; @see https://github.com/seagle0128/.emacs.d/issues/88
             (make-variable-buffer-local 'show-paren-mode)
             (setq show-paren-mode nil))))
  ;; :custom (
  ;;          (org-src-fontify-natively . t)
  ;;          (org-src-tab-acts-natively . t)
  ;;          (org-edit-src-content-indentation  . 0)
  ;;          )
  )

(leaf org-latex-impatient
  :straight (org-latex-impatient
             :type git :host github 
             :repo "ingtshan/org-latex-impatient")
  ;; :hook ((org-mode-hook . org-latex-impatient-mode))
  :init
  (setq org-latex-impatient-tex2svg-bin
        ;; location of tex2svg executable
        "~/node_modules/mathjax-node-cli/bin/tex2svg"))

(leaf org-fragtog
  :hook (org-mode-hook
         . (lambda ()
             (if org-startup-with-latex-preview
                 (org-fragtog-mode 1))))
  :config)

(leaf separate-inline
  :ensure nil
  :straight (separate-inline
             :type git :host github
             :repo "ingtshan/separate-inline.el"
             :branch "devel")
  :hook ((org-mode-hook . separate-inline-mode)
         (org-mode-hook
          .
          (lambda ()
            (add-hook 'separate-inline-mode-hook
                      'separate-inline-use-default-rules-for-org-local
                      nil 'make-it-local)))))

;; image preview
(leaf org-yt
  :straight (org-yt :type git :host github
                    :repo "TobiasZawada/org-yt")
  :require t)

(with-eval-after-load 'org

  (defun org-image-link (protocol link _description)
    "Interpret LINK as base64-encoded image data."
    (cl-assert (string-match "\\`img" protocol) nil
               "Expected protocol type starting with img")
    (let ((buf (url-retrieve-synchronously (concat (substring protocol 3) ":" link))))
      (cl-assert buf nil
                 "Download of image \"%s\" failed." link)
      (with-current-buffer buf
        (goto-char (point-min))
        (re-search-forward "\r?\n\r?\n")
        (buffer-substring-no-properties (point) (point-max)))))

  (org-link-set-parameters
   "imghttp"
   :image-data-fun #'org-image-link)

  (org-link-set-parameters
   "imghttps"
   :image-data-fun #'org-image-link))

(leaf org-anki
  :straight(org-anki
            :type git :host github
            :repo "eyeinsky/org-anki"
            :fork (:branch "devel"))
  :config
  (defun org-anki/bounds-of-current-elem ()
    "Return the bounds of org elem currently surrounding the cursor.
If there is none, return nil."
    (interactive)
    (let*
        ;; Element surrounding the cursor
        ((elem (org-element-context))
         ;; Type of element surrounding the cursor
         (elem-type (nth 0 elem))
         ;; List of fragment's properties
         (elem-plist (nth 1 elem))
         ;; A LaTeX fragment or environment is surrounding the cursor
         (elem-is-latex (and (member elem-type '(latex-fragment latex-environment))
                             ;; Normally org-mode considers whitespace after an
                             ;; element as part of the element.
                             ;; Avoid this behavior and consider trailing
                             ;; whitespace as outside the fragment.
                             (< (point) (- (plist-get elem-plist :end)
                                           (plist-get elem-plist :post-blank))))))
      (unless elem-is-latex
        ;; more org elem
        (org-appear-toggle)
        (setq elem (org-appear--current-elem)))
      (if (or elem-is-latex elem)
          (cons (org-element-property :begin elem)
                (org-element-property :end elem))
        nil)))
  
  (eval-after-load 'org-anki
    '(defun org-anki--is-cloze (text)
       "Check if TEXT has cloze syntax, return nil if not."
       ;; Check for something similar to {{c1::Hidden-text::Hint}} in TEXT
       (if (string-match "{{c[0-9]+::" text)
           "Cloze"
         nil)))
  
  (eval-after-load 'org-anki
    '(defun org-anki--region-to-cloze (begin end arg hint)
       "Cloze region from BEGIN to END with number ARG."
       (let ((region (buffer-substring begin end)))
         (save-excursion
           (delete-region begin end)
           (insert (with-output-to-string
                     (princ (format "{{c%d:: %s " (or arg 1) region))
                     (unless (string-blank-p hint) (princ (format "::%s" hint)))
                     (princ "}}")))))))
  
  (eval-after-load 'org-anki
    '(defun org-anki-cloze-dwim (&optional arg hint)
       "Convert current active region or word under cursor to Cloze
syntax."
       (interactive "P")
       
       (unless hint         
         (sis-global-respect-mode -1)
         (sis-set-other)
         (unwind-protect
             (setq hint (read-string "Hint (Optional) : ..."))
           (sis-set-english)
           (sis-global-respect-mode 1))
         
         (or (string-blank-p hint) (setq hint (concat "..." hint))))
       
       (if (region-active-p)
           (org-anki--region-to-cloze
            (region-beginning) (region-end) arg hint)
         (let ((bounds (org-anki/bounds-of-current-elem)))
           
           (or bounds
               (and (thing-at-point 'word)
                    (setq bounds (bounds-of-thing-at-point 'word))))
           
           (if bounds
               (org-anki--region-to-cloze (car bounds) (cdr bounds) arg hint)
             (error "Nothing to create cloze from"))
           ))))
  )

;; 选中 插入样式 ** == // __ ++ ~~ emphasize mark
(defun ns/org-region-to-emphasiz(begin end mark)
  (let ((select-text (buffer-substring begin end)))
    (save-excursion
      (delete-region begin end)
      (insert (concat mark select-text mark)))))

(defun ns/org-emphasize-dwim (&optional mark)
  (interactive (list (completing-read "emphasize: " '("*" "=" "~" "/" "_" "+"))))
  (cond
   ((region-active-p)
    (ns/org-region-to-emphasiz
     (region-beginning) (region-end) mark))
   ((thing-at-point 'word)
    (let ((bounds (bounds-of-thing-at-point 'word)))
      (ns/org-region-to-emphasiz
       (car bounds) (cdr bounds) mark)))
   (t (error "Nothing to emphasiz from"))))

(defun ns/org-cycle()
  "better cycle logic"
  (interactive)
  (let ((head (org-babel-where-is-src-block-head)))
    (cond
     (head (goto-char head))
     ((not (org-at-heading-p)) (org-back-to-heading)))
    (org-cycle)))

;; Yjgo 学习观 断墨寻径
;; 策略：记忆 理解

;; 理解 =泛化学习

;; 明确对象(指令、定义、实例、输入、输出)
;; 指令 =知识名字
;; 定义 =指令的文字性描述
;; 实例 =(输入,输出) 输入->理解模型->输出

;; 一阶知识 =分类模型、回归模型
;; 二阶知识 =分而治之(拆分知识) =组合关系、执行步骤

;; 学习任务 =构建一阶知识，构建二阶知识
;; 构建一阶知识 =分类任务，回归任务
;; 分类任务 =它是什么(判断是和不是)，为什么是(结构,特点等)
;; 回归任务 =它的目的(作用,影响)，如何达成(详细过程)
;; 组合关系 =父知识 ​=子知识+​子知识
;; 执行步骤 =父知识 =1.子知识 2.子知识

;; 训练 =一阶训练、二阶训练、实例有效训练、归纳训练(二例对比)
;; 验证 =同义转换指令和定义、费曼技巧

;; 一阶分类训练 =是什么 (判断 是或不是)，为什么 (给出具体理由)
;; 一阶回归训练 =有什么用，为了什么，详细过程
;; 二阶组合任务 =组成部分(概念化，尽量不展开)，正向和逆向
;; 二阶步骤拆分 =执行顺序(概念化，尽量不展开)，正向和逆向
;; 实例训练 =明确输入求输出 比对标准，自我反馈
;; 归纳训练 =对比实例自我总结

;; 同义转换 =自己的话表述 定义
;; 费曼技巧 =目的导向(致用新情况)、以教促学

(defun ns/study-in-right-way-insert ()
  "断墨寻径 我的学习关键词"
  (interactive)
  (let ((arg (car (split-string
                   (completing-read
                    "学习元素"
                    '(" 记忆"
                      " 分析 =逻辑过程"
                      " 理解 =泛化学习"
                      "明确对象 =指令、定义、实例、输入、输出"
                      " 训练 =一阶训练、二阶训练、实例有效训练、归纳训练(二例对比)"
                      " 验证 =同义转换指令和定义、费曼技巧"))))))
    (pcase arg
      ("记忆" (setq arg " 记忆:"))
      ("理解" (setq arg " 理解:"))
      ("分析" (setq arg " 分析:"))      
      ("明确对象"
       (setq arg
             (concat
              (completing-read
               "明确对象" '("指令" "定义" "实例" "问题" "答案"))
              ": ")))
      ("训练"
       (call-interactively
        'org-insert-heading-respect-content)
       (setq arg
             (car
              (split-string
               (completing-read
                "训练任务"
                '("判断是或者不是,再描述原因 =是什么、为什么 一阶分类知识"
                  "有什么用 =一阶回归知识"
                  "特点和原因 =一阶回归知识"
                  "详细过程 =一阶回归知识"
                  "提供什么(概念化，向上链) =二阶组合知识"
                  "组成部分(概念化，尽量不展开，正向和逆向) =二阶组合知识"
                  "拆分步骤(概念化，尽量不展开，正向和逆向) =二级步骤拆分"
                  "实例训练(输入->输出) =明确输入求输出 比对标准，自我反馈"
                  "归纳训练(归纳，总结) =对比实例自我总结"
                  ))))))
      ("验证"
       (setq arg
             (car
              (split-string
               (completing-read
                "训练任务"
                '("用自己话解释定义，比喻指令 =同义转换"
                  "学以致用,目的导向学习 =费曼技巧"
                  "以教促学 =费曼技巧"
                  )))))))
    (pcase arg
      ("判断是或者不是,再描述原因"
       (insert "一阶训练：判断是不是\n")
       (save-excursion
         (call-interactively 'org-insert-subheading)
         (insert "答案\n")
         (call-interactively 'org-insert-subheading)
         (insert "一阶训练：为什么\n")
         (insert "答案:")
         (call-interactively
          'org-insert-heading-respect-content)
         (insert "一阶训练: 举其他例子"))
       (setq arg ""))
      ("实例训练(输入->输出)"
       (insert "实例训练(输入->输出): ")
       (save-excursion
         (call-interactively 'org-insert-subheading)
         (insert "输出")
         (setq arg "")))
      ("特点和原因"
       (setq arg "一阶训练：它的特性和原因"))
      ("有什么用"
       (setq arg "一阶训练：它的作用"))
      ("详细过程"       
       (setq arg "一阶训练：详细说明(根据新的策略执行)"))
      ("组成部分(概念化，尽量不展开，正向和逆向)"       
       (setq arg "二阶知识：组成、构成"))
      ("提供什么(概念化，向上链)"       
       (setq arg "二阶训练：参与组成 或 关键步骤"))
      ("拆分步骤(概念化，尽量不展开，正向和逆向)"
       (setq arg "二阶知识：关键步骤")))

    (insert arg)))


(provide 'init-org)
;;; init-org.el ends here
