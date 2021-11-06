;;; init-editor.el -*- lexical-binding: t; -*-

;;; Commentary:

;; make emacs a better editor

;;; Code:

(leaf indent-guide
  :hook ((emacs-lisp-mode-hook . indent-guide-mode)))

(provide 'init-editor)
;;; init-editor.el ends here
