;;; init-org.el -*- lexical-binding: t; -*-

;;; Commentary:

;; 

;;; Code:

;; org-basic
(leaf org
  :require indent-guide
  :hook((org-mode-hook . org-indent-mode)
        (org-mode-hook . indent-guide-mode))
  :config
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

  )

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

(provide 'init-org)
;;; init-org.el ends here
