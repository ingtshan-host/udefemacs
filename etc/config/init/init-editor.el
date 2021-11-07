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

(provide 'init-editor)
;;; init-editor.el ends here
