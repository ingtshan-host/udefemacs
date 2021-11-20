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

  ;; (setq org-directory "~/iCloud/org/")
  ;; (setq org-agenda-files '("~/iCloud/org/"))

  (defun ns/org-kill-link-at-point ()
    (interactive)
    (when (eq major-mode 'org-mode)
      (let* ((context (org-element-context))
             (type (org-element-type context))
             (beg (org-element-property :begin context))
             (end (org-element-property :end context)))
        (when (eq type 'link)
          (kill-region beg end)))))
  )

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

  :config
  (defun org/insert-src-block (src-code-type)
    "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
    (interactive
     (let ((src-code-types
	        '("emacs-lisp" "rust" "python" "C" "shell" "java" "js" "clojure" "C++" "css"
	          "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
	          "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
	          "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
	          "scheme" "sqlite" "html")))
       (list (ido-completing-read "Source code type: " src-code-types))))
    (save-excursion
      (newline-and-indent)
      (insert (format "#+begin_src %s\n" src-code-type))
      (newline-and-indent)
      (insert "#+end_src\n")
      (previous-line 2)
      (org-edit-src-code)))
  )

(leaf-unit org-nano-theme

  (require 'cl-lib)   ; for delete*

  (with-eval-after-load 'org
    (set-face 'org-archived                            'nano-face-faded)

    (set-face 'org-block                                       'hl-line)
    (set-face 'org-block-begin-line                    'nano-face-faded)
    (set-face 'org-block-end-line                      'nano-face-faded)
    (unless (version< emacs-version "27.0")
      (set-face-attribute 'org-block nil                      :extend t)
      (set-face-attribute 'org-block-begin-line nil           :extend t)
      (set-face-attribute 'org-block-end-line nil             :extend t))

    (set-face 'org-checkbox                            'nano-face-faded)
    ;; (set-face 'org-checkbox-statistics-done            'nano-face-faded)
    ;; (set-face 'org-checkbox-statistics-todo            'nano-face-faded)
    (set-face 'org-clock-overlay                       'nano-face-faded)
    (set-face 'org-code                                'nano-face-faded)
    (set-face 'org-column                              'nano-face-faded)
    (set-face 'org-column-title                        'nano-face-faded)
    (set-face 'org-date                                'nano-face-faded)
    (set-face 'org-date-selected                       'nano-face-faded)
    (set-face 'org-default                             'nano-face-faded)
    (set-face 'org-document-info                       'nano-face-faded)
    (set-face 'org-document-info-keyword               'nano-face-faded)
    (set-face 'org-document-title                      'nano-face-faded)
    (set-face 'org-done                              'nano-face-default)
    (set-face 'org-drawer                              'nano-face-faded)
    (set-face 'org-ellipsis                            'nano-face-faded)
    (set-face 'org-footnote                            'nano-face-faded)
    (set-face 'org-formula                             'nano-face-faded)
    (set-face 'org-headline-done                       'nano-face-faded)
    ;; (set-face 'org-hide                             'nano-face-faded)
    ;; (set-face 'org-indent                           'nano-face-faded)
    (set-face 'org-latex-and-related                   'nano-face-faded)
    ;; (set-face 'org-level-1                            'nano-face-strong)
    ;; (set-face 'org-level-2                            'nano-face-strong)
    ;; (set-face 'org-level-3                            'nano-face-strong)
    ;; (set-face 'org-level-4                            'nano-face-strong)
    ;; (set-face 'org-level-5                            'nano-face-strong)
    ;; (set-face 'org-level-6                            'nano-face-strong)
    ;; (set-face 'org-level-7                            'nano-face-strong)
    ;; (set-face 'org-level-8                            'nano-face-strong)
    (set-face 'org-link                              'nano-face-salient)
    (set-face 'org-list-dt                             'nano-face-faded)
    (set-face 'org-macro                               'nano-face-faded)
    (set-face 'org-meta-line                           'nano-face-faded)
    (set-face 'org-mode-line-clock                     'nano-face-faded)
    (set-face 'org-mode-line-clock-overrun             'nano-face-faded)
    (set-face 'org-priority                            'nano-face-faded)
    (set-face 'org-property-value                      'nano-face-faded)
    (set-face 'org-quote                               'nano-face-faded)
    (set-face 'org-scheduled                           'nano-face-faded)
    (set-face 'org-scheduled-previously                'nano-face-faded)
    (set-face 'org-scheduled-today                     'nano-face-faded)
    (set-face 'org-sexp-date                           'nano-face-faded)
    (set-face 'org-special-keyword                     'nano-face-faded)
    (set-face 'org-table                               'nano-face-faded)
    (set-face 'org-tag                                'nano-face-popout)
    (set-face 'org-tag-group                           'nano-face-faded)
    (set-face 'org-target                              'nano-face-faded)
    (set-face 'org-time-grid                           'nano-face-faded)
    ;; (set-face 'org-todo                              'nano-face-salient)
    (set-face 'org-upcoming-deadline                 'nano-face-default)
    (set-face 'org-verbatim                           'nano-face-popout)
    (set-face 'org-verse                               'nano-face-faded)
    (set-face 'org-warning                            'nano-face-popout)

    ;; 设置org标题1-8级的字体大小和颜色，颜色摘抄自monokai。;希望org-mode标题的字体大小和正文一致，设成1.0， 如果希望标题字体大一点可以设成1.2
    (custom-set-faces
     '(org-level-1
       ((t (:inherit outline-1 :height 1.2  :foreground "#FD971F"))))
     '(org-level-2
       ((t (:inherit outline-2 :height 1.2  :foreground "#A6E22E"))))
     '(org-level-3
       ((t (:inherit outline-3 :height 1.2  :foreground "#66D9EF"))))
     '(org-level-4
       ((t (:inherit outline-4 :height 1.2  :foreground "#E6DB74"))))
     '(org-level-5
       ((t (:inherit outline-5 :height 1.2  :foreground "#A1EFE4"))))
     '(org-level-6
       ((t (:inherit outline-6 :height 1.2  :foreground "#A6E22E"))))
     '(org-level-7
       ((t (:inherit outline-7 :height 1.2  :foreground "#F92672"))))
     '(org-level-8
       ((t (:inherit outline-8 :height 1.2  :foreground "#66D9EF"))))

     )

    ;; change default color of emphasis below
    (setq org-emphasis-alist
          (cons '("+" '(:strike-through t :foreground "gray"))
                (cl-delete "+" org-emphasis-alist :key 'car :test 'equal)))
    (setq org-emphasis-alist
          (cons '("*" '(:emphasis t :foreground "#E6DB74"))
                (cl-delete "*" org-emphasis-alist :key 'car :test 'equal)))
    (setq org-emphasis-alist
          (cons '("/" '(:italic t :foreground "#66D9EF"))
                (cl-delete "/" org-emphasis-alist :key 'car :test 'equal)))

    ))

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
(provide 'init-org)
;;; init-org.el ends here
