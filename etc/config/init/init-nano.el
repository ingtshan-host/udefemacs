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

(leaf nano-emacs
  :ensure nil
  :straight (nano-emacs :type git :host github :repo "rougier/nano-emacs")
  :require disp-table
  :config
  (leaf nano-faces
    :load-path `,(expand-file-name "straight/build/nano-emacs" user-emacs-directory)
    :ensure nil
    :straight nil
    :require nano-theme-dark nano-theme nano-modeline nano-help
    :custom ((nano-font-family-monospaced . "Fira Code")
             (nano-font-size . 16))
    :config
    (nano-faces)

    ;; theme    
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
    ;;(nano-theme--org)
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
