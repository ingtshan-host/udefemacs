;;; init-editor.el -*- lexical-binding: t; -*-

;;; Commentary:

;; make emacs a better editor

;;; Code:

(leaf indent-guide
  :hook ((emacs-lisp-mode-hook . indent-guide-mode)))
(prog1 'indent-guide
  (leaf-handler-leaf-path indent-guide)
  (leaf-handler-leaf-protect indent-guide
    (unless
        (fboundp 'indent-guide-mode)
      (autoload #'indent-guide-mode "indent-guide" nil t))
    (declare-function indent-guide-mode "indent-guide")
    (leaf-handler-package indent-guide indent-guide nil)
    (straight-use-package 'indent-guide)
    (add-hook 'emacs-lisp-mode-hook #'indent-guide-mode)))

(provide 'init-editor)
;;; init-editor.el ends here
