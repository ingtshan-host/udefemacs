;;; init-nano.el -*- lexical-binding: t; -*-

;;; Commentary:

;; 

;;; Code:

;; layout

(require 'disp-table)
;; Fix bug on OSX in term mode & zsh (spurious % after each command)
(add-hook 'term-mode-hook
	      (lambda () (setq buffer-display-table (make-display-table))))
;; No ugly button for checkboxes
(setq widget-image-enable nil)
;; Hide org markup for README
(setq org-hide-emphasis-markers t)

;;设置org标题1-8级的字体大小和颜色，颜色摘抄自monokai。
;;希望org-mode标题的字体大小和正文一致，设成1.0
;;如果希望标题字体大一点可以设成1.2

(defface my-nano-face-level-1
  '((t :inherit font-lock-function-name-face
       :height 1.2  :foreground "#FD971F"))
  "Level 1." :group 'nano)

(defface my-nano-face-level-2
  '((t :inherit font-lock-variable-name-face
       :height 1.2  :foreground "#A6E22E"))
  "Level 2." :group 'nano)

(defface my-nano-face-level-3
  '((t :inherit font-lock-keyword-face
       :height 1.2  :foreground "#66D9EF"))
  "Level 3." :group 'nano)

(defface my-nano-face-level-4
  '((t :inherit font-lock-comment-face
       :height 1.2  :foreground "#E6DB74"))
  "Level 4." :group 'nano)

(defface my-nano-face-level-5
  '((t :inherit font-lock-type-face
       :height 1.2  :foreground "#A1EFE4"))
  "Level 5." :group 'nano)

(defface my-nano-face-level-6
  '((t :inherit font-lock-constant-face
       :height 1.2  :foreground "#A6E22E"))
  "Level 6." :group 'nano)

(defface my-nano-face-level-7
  '((t :inherit font-lock-builtin-face
       :height 1.2  :foreground "#F92672"))
  "Level 7." :group 'nano)

(defface my-nano-face-level-8
  '((t :inherit font-lock-string-face
       :height 1.2  :foreground "#66D9EF"))
  "Level 8." :group 'nano)

(leaf nano-emacs
  :straight (nano-emacs :type git :host github
                        :repo "rougier/nano-emacs"
                        :fork (:host github :branch "adapt"))
  :require disp-table
  :config
  (leaf nano-faces
    :load-path
    `,(expand-file-name "straight/build/nano-emacs" user-emacs-directory)
    :straight nil
    :require outline
    :require nano-theme-dark nano-theme nano-help nano-modeline
    :custom ((nano-font-family-monospaced . "Fira Code")
             (nano-font-size . 16))
    :config
    (nano-faces)

    ;; inherit nano face to create my face
    (defface my-nano-face-blod
      '((t (:inherit nano-face-strong :foreground "#E6DB74")))
      "blod." :group 'nano)
    (defface my-nano-face-italic
      '((t (:inherit nano-face-faded :foreground "#66D9EF")))
      "italic." :group 'nano)

    ;; theme take effect
    
    (nano-theme--basics)
    
    (nano-theme--font-lock)
    (nano-theme--mode-line)
    (nano-theme--minibuffer)
    (nano-theme--buttons)
    (nano-theme--info)
    (nano-theme--bookmark)
    (nano-theme--speedbar)
    (nano-theme--message)
    (nano-theme--outline)
    (nano-theme--customize)
    (nano-theme--package)
    ;;(nano-theme--flyspell)
    ;;(nano-theme--ido)
    ;;(nano-theme--diff)
    (nano-theme--term)
    (nano-theme--calendar)
    (nano-theme--agenda)

    (nano-theme--org)
    ;;modify nano-theme--org    
    (with-eval-after-load 'org
      (setq org-emphasis-alist
            '(("*" my-nano-face-blod)
              ("/" my-nano-face-italic)
              ("_" underline :foreground "cyan" )
              ("=" org-verbatim verbatim)
              ("~" (:foreground "PaleGreen1" ))
              ("+" (:strike-through t :foreground "gray" ))))
      
      (set-face 'org-level-1 'my-nano-face-level-1)
      (set-face 'org-level-2 'my-nano-face-level-2)
      (set-face 'org-level-3 'my-nano-face-level-3)
      (set-face 'org-level-4 'my-nano-face-level-4)
      (set-face 'org-level-5 'my-nano-face-level-5)
      (set-face 'org-level-6 'my-nano-face-level-6)
      (set-face 'org-level-7 'my-nano-face-level-7)
      (set-face 'org-level-8 'my-nano-face-level-8))
    
    ;;(nano-theme--mu4e)
    ;;(nano-theme--elfeed)
    (nano-theme--deft)
    (nano-theme--rst)
    (nano-theme--markdown)
    ;;(nano-theme--ivy)
    ;;(nano-theme--helm)
    ;;(nano-theme--helm-swoop)
    ;;(nano-theme--helm-occur)
    ;;(nano-theme--helm-ff)
    ;;(nano-theme--helm-grep)
    (nano-theme--hl-line)
    (nano-theme--company)
    ))
;; (require 'nano-colors)

(provide 'init-nano)
;;; init-nano.el ends here
