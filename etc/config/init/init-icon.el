;;; init-icon.el -*- lexical-binding: t; -*-

;;; Commentary:

;; all icons and emoji

;;; Code:

(leaf all-the-icons)
;; then you should manually install fonst
;; M-x `all-the-icons-install-fonts'

(leaf all-the-icons-completion
  :config
  (all-the-icons-completion-mode))

(provide 'init-icon)
;;; init-icon.el ends here
